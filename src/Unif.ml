(* The module of UnionFind *)

module UF = UnionFind.Make (UnionFind.StoreMap)

(* Type of variable *)

type var = Constraint.variable

(* Type of structure *)

type structure = var Structure.t option

(* Type of status *)

type status =
  | Flexible (* Variable not generalized yet *)
  | Generic (* Variable already generalized *)

(* Type of rank *)

type rank = int

let base_rank = 0 (* The top of the program *)

(* The internal representation in terms of union-find nodes. *)
type uvar = unode UF.rref

(* The data of the variable *)
and unode =
  { var : var
  ; data : uvar Structure.t option
  ; status : status
  ; rank : rank
  }

(* The user-facing representation hides union-find nodes,
   replaced by the corresponding constraint variables. *)
type repr =
  { var : var
  ; structure : structure
  ; status : status
  ; rank : rank
  }

(* Type du pool *)

module IntMap = Map.Make (Int)

type pool = content_pool IntMap.t

and content_pool = var list

(* Environment's code *)

module Env = struct
  type t =
    { store : unode UF.store
    ; map : uvar Constraint.Var.Map.t
    ; pool : pool
    ; young : rank
    }

  let empty =
    let store = UF.new_store () in
    let map = Constraint.Var.Map.empty in
    let pool = IntMap.empty in
    let young = base_rank - 1 in
    { store; map; pool; young }

  let get_young env = env.young

  let incr_young env = { env with young = env.young + 1 }

  let decr_young env = { env with young = env.young - 1 }

  let pool_is_empty rank env =
    match IntMap.find_opt rank env.pool with
    | None | Some [] -> true
    | Some _ -> false

  let clean_pool rank env = { env with pool = IntMap.remove rank env.pool }

  let get_pool rank env =
    Option.value ~default:[] @@ IntMap.find_opt rank env.pool

  let uvar var env : uvar = Constraint.Var.Map.find var env.map

  let mem var env = Constraint.Var.Map.mem var env.map

  let is_representative (var : var) env =
    UF.is_representative env.store @@ Constraint.Var.Map.find var env.map

  let add_data data env =
    let var = data.var in
    let status = data.status in
    let rank = data.rank in
    let data =
      Option.map (Structure.map @@ fun v -> uvar v env) data.structure
    in

    let uvar = UF.make env.store { var; data; status; rank } in
    { env with map = Constraint.Var.Map.add var uvar env.map }

  let add var structure env =
    let status = Flexible in
    let rank = env.young in
    add_data { var; structure; status; rank } env

  let register var ~rank env =
    let l =
      match IntMap.find_opt rank env.pool with
      | None -> [ var ]
      | Some l -> var :: l
    in
    { env with pool = IntMap.add rank l env.pool }

  let repr var env =
    let { var; data; status; rank } = UF.get env.store @@ uvar var env in
    let var_of_uvar uv = (UF.get env.store uv).var in
    let structure = Option.map (Structure.map var_of_uvar) data in
    { var; structure; status; rank }

  let debug_repr_assoc env =
    let open PPrint in
    Constraint.Var.Map.fold
      begin
        fun var _ acc ->
          let var_doc = Constraint.Var.print var in
          let repr_doc = Constraint.Var.print (repr var env).var in

          acc ^^ var_doc ^^ string " |--> " ^^ repr_doc ^^ break 1
      end
      env.map empty

  let debug_rank env =
    let open PPrint in
    Constraint.Var.Map.fold
      begin
        fun var uvar acc ->
          let var_doc = Constraint.Var.print var in
          let rank = (UF.get env.store uvar).rank in
          let str = Format.sprintf ".rank = %d" rank in

          acc ^^ var_doc ^^ string str ^^ break 1
      end
      env.map empty
end

(* Errors check *)

type clash = var Utils.clash

exception Clash of clash

type cycle = var Utils.cycle

exception Cycle of cycle

type err =
  | Clash of clash
  | Cycle of cycle

let check_no_cycle env v =
  let open struct
    type status =
      | Visiting
      | Visited
  end in
  let table = Hashtbl.create 16 in

  let rec loop v =
    let n = UF.get env.Env.store v in

    match Hashtbl.find table n.var with
    | Visited -> ()
    | Visiting -> raise @@ Cycle (Utils.Cycle n.var)
    | exception Not_found ->
      Hashtbl.replace table n.var Visiting;
      Option.iter (Structure.iter loop) n.data;
      Hashtbl.replace table n.var Visited
  in

  loop v

(* Unify's code *)

let rec unify orig_env v1 v2 : (Env.t, err) result =
  let env = { orig_env with Env.store = UF.copy orig_env.Env.store } in

  let find v =
    try Env.uvar v env
    with Not_found ->
      Printf.ksprintf invalid_arg
        "Constraint variable '%s' is unbound at this point"
      @@ Constraint.Var.name v
  in

  let queue = Queue.create () in
  Queue.add (find v1, find v2) queue;

  match unify_uvars env.Env.store queue with
  | exception Clash clash -> Error (Clash clash)
  | () -> begin
    match check_no_cycle env (Env.uvar v1 env) with
    | exception Cycle v -> Error (Cycle v)
    | () -> Ok env
  end

and unify_uvars store (queue : (uvar * uvar) Queue.t) =
  match Queue.take_opt queue with
  | None -> ()
  | Some (u1, u2) ->
    UF.merge store (merge queue) u1 u2 |> ignore;
    unify_uvars store queue

and merge queue (n1 : unode) (n2 : unode) : unode =
  let clash () = raise @@ Clash (n1.var, n2.var) in

  let data =
    match (n1.data, n2.data) with
    | None, None -> None
    | None, (Some _ as d) | (Some _ as d), None -> d
    | Some st1, Some st2 -> begin
      let merge =
        Structure.merge
          begin
            fun v1 v2 ->
              Queue.add (v1, v2) queue;
              v1
          end
          st1 st2
      in

      match merge with None -> clash () | Some d -> Some d
    end
  in

  { n1 with data }

let unifiable env v1 v2 =
  match unify env v1 v2 with Ok _ -> true | Error _ -> false
