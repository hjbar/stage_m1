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

  let add_repr repr env =
    let var = repr.var in
    let data =
      Option.map (Structure.map @@ fun v -> uvar v env) repr.structure
    in
    let status = repr.status in
    let rank = repr.rank in

    let uvar = UF.make env.store { var; data; status; rank } in
    { env with map = Constraint.Var.Map.add var uvar env.map }

  let register var ~rank env =
    let l =
      match IntMap.find_opt rank env.pool with
      | None -> [ var ]
      | Some l -> var :: l
    in
    { env with pool = IntMap.add rank l env.pool }

  let add_flexible var structure env =
    let status = Flexible in
    let rank = env.young in

    env |> add_repr { var; structure; status; rank } |> register var ~rank

  let repr var env =
    let { var; data; status; rank } = UF.get env.store @@ uvar var env in
    let var_of_uvar uv = (UF.get env.store uv).var in
    let structure = Option.map (Structure.map var_of_uvar) data in
    { var; structure; status; rank }

  let is_representative var env =
    match repr var env with
    | var_repr -> Constraint.Var.eq var var_repr.var
    | exception Not_found -> true

  let debug_var var uvar =
    let module PP = PPrint in
    let ( ^^ ) = PP.( ^^ ) in
    if var <> uvar.var then
      (* not representative *)
      (Constraint.Var.print var
       ^^ PP.string " |--> "
       ^^ Constraint.Var.print uvar.var,
       `Non_repr)
    else begin
      let rank_doc, order = match uvar.status with 
        | Flexible -> PP.string (string_of_int uvar.rank), `Rank uvar.rank
        | Generic -> PP.string "G", `Generic
       in
       let structure_doc =
         match uvar.structure with
         | None -> PP.empty
         | Some s ->
           PP.string "= "
           ^^ Structure.print Constraint.Var.print s
       in
       (Constraint.Var.print var
        ^^ PP.parens rank_doc ^^ PP.space
        ^^ structure_doc,
        order)
    end

  let debug (env : t) =
    let open PPrint in
    env.map
    |> Constraint.Var.Map.bindings
    |> List.map (fun (v, _uv) ->
      let doc, order = debug_var v (repr v env) in
      doc,
      match order with
      | `Non_repr -> -10
      | `Rank n -> n
      | `Generic -> max_int
    )
    |> List.sort (fun (_, o1) (_, o2) -> Int.compare o1 o2)
    |> concat_map (fun (doc, _order) -> doc ^^ hardline)

  let debug_pool_assoc env =
    let open PPrint in
    IntMap.fold
      begin
        fun rank variables acc ->
          let variables_doc =
            space ^^
            separate_map space Constraint.Var.print variables
          in
          acc
          ^^ string (Format.sprintf "%d |--> " rank)
          ^^ variables_doc ^^ break 1
      end
      env.pool empty
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
