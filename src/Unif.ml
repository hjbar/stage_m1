(* The UnionFind module *)

module UF = UnionFind.Make (UnionFind.StoreMap)

(* Variable type *)

type var = Constraint.variable

(* Structure type *)

type structure = var Structure.t option

(* Status type *)

type status =
  | Flexible
  | Generic

(* Rank type *)

type rank = int

let base_rank = 0

(* Pool type *)

module IntMap = Map.Make (Int)

type pool = var list IntMap.t

(* Internal representation using union-find nodes. *)
type uvar = unode UF.rref

(* Data associated with a variable *)
and unode = {
  var : var;
  data : uvar Structure.t option;
  status : status;
  rank : rank;
}

(* The user-facing representation hides union-find nodes,
   replaced by the corresponding constraint variables. *)

type repr = {
  var : var;
  structure : structure;
  status : status;
  rank : rank;
}

(* Environment module implementation *)

module Env = struct
  (* Definition of the type [t] *)

  type t = {
    store : unode UF.store;
    map : uvar Constraint.Var.Map.t;
    pool : pool;
    young : rank;
  }

  (* Empty environment *)

  let empty () =
    let store = UF.new_store () in
    let map = Constraint.Var.Map.empty in
    let pool = IntMap.empty in
    let young = base_rank - 1 in
    { store; map; pool; young }


  (* Functions to check whether parts of the environment are empty *)

  let map_is_empty env = Constraint.Var.Map.is_empty env.map

  let pool_is_empty env = IntMap.is_empty env.pool

  let pool_k_is_empty rank env =
    match IntMap.find_opt rank env.pool with
    | None | Some [] -> true
    | Some _ -> false


  (* Membership test functions *)

  let mem var env = Constraint.Var.Map.mem var env.map

  (* Getter functions for environment data *)

  let uvar var env = Constraint.Var.Map.find var env.map

  let repr var env =
    let { var; data; status; rank } = UF.get env.store (uvar var env) in
    let var_of_uvar uv = (UF.get env.store uv).var in
    let structure = Option.map (Structure.map var_of_uvar) data in

    { var; structure; status; rank }


  let get_young env = env.young

  let get_pool rank env =
    Option.value ~default:[] @@ IntMap.find_opt rank env.pool


  (* Conversion functions *)

  let unode_of_repr repr env =
    let var = repr.var in
    let data =
      Option.map (Structure.map (fun v -> uvar v env)) repr.structure
    in
    let status = repr.status in
    let rank = repr.rank in

    { var; data; status; rank }


  (* Functions to add or register variables to the environment *)

  let add repr env =
    let var = repr.var in
    let unode = unode_of_repr repr env in

    assert (not (Constraint.Var.Map.mem var env.map));

    let uvar = UF.make env.store unode in
    let map = Constraint.Var.Map.add var uvar env.map in

    { env with map }


  let register var ~rank env =
    let l = Option.value ~default:[] @@ IntMap.find_opt rank env.pool in
    let pool = IntMap.add rank (var :: l) env.pool in

    { env with pool }


  let add_flexible var structure env =
    let status = Flexible in
    let rank = env.young in

    env |> add { var; structure; status; rank } |> register var ~rank


  (* Functions to remove variables from the environment *)

  let unbind var env =
    let map = Constraint.Var.Map.remove var env.map in
    { env with map }


  (* Setter functions *)

  let set repr env =
    let store = UF.copy env.store in
    let env = { env with store } in

    let var = repr.var in
    let unode = unode_of_repr repr env in

    assert (mem var env);
    let node = uvar var env in

    UF.set env.store node unode;
    env


  (* Functions to manipulate the environment's young rank *)

  let incr_young env =
    let young = env.young + 1 in
    { env with young }


  let decr_young env =
    let young = env.young - 1 in
    { env with young }


  (* Functions to manipulate the pool *)

  let clean_pool rank env =
    let pool = IntMap.remove rank env.pool in
    { env with pool }


  (* Functions on representatives *)

  let is_representative var env =
    match repr var env with
    | var_repr -> Constraint.Var.eq var var_repr.var
    | exception Not_found -> true


  (* Debugging functions *)

  let debug_uvar var uvar =
    let module PP = PPrint in
    let ( ^^ ) = PP.( ^^ ) in

    if not (Constraint.Var.eq var uvar.var) then
      (* not representative *)
      let doc =
        Constraint.Var.print var
        ^^ PP.string " |--> "
        ^^ Constraint.Var.print uvar.var
      in
      (doc, `Non_repr)
    else
      let order =
        match uvar.status with
        | Flexible -> `Rank uvar.rank
        | Generic -> `Generic
      in

      let rank_doc =
        match uvar.status with
        | Flexible -> PP.string (string_of_int uvar.rank)
        | Generic -> PP.string "G"
      in

      let structure_doc =
        match uvar.structure with
        | None -> PP.empty
        | Some s -> PP.string "= " ^^ Structure.print Constraint.Var.print s
      in

      let doc =
        Constraint.Var.print var
        ^^ PP.parens rank_doc
        ^^ PP.space
        ^^ structure_doc
      in
      (doc, order)


  let debug_var var env = debug_uvar var (repr var env) |> fst

  let sort_debug_vars l =
    l
    |> List.map
         begin
           fun (doc, order) ->
             let cmp =
               match order with
               | `Non_repr -> min_int
               | `Rank n -> n
               | `Generic -> max_int
             in
             (doc, cmp)
         end
    |> List.sort (fun (_, o1) (_, o2) -> Int.compare o1 o2)
    |> List.map fst


  let debug_env env =
    let open PPrint in
    env.map
    |> Constraint.Var.Map.bindings
    |> List.map fst
    |> List.map (fun v -> debug_uvar v (repr v env))
    |> sort_debug_vars
    |> separate hardline


  let debug_pool env =
    let open PPrint in
    env.pool
    |> IntMap.bindings
    |> List.map
         begin
           fun (rank, variables) ->
             let alinea = hardline ^^ space ^^ space ^^ space in
             let variables_doc =
               variables
               |> List.map (fun v -> debug_uvar v (repr v env))
               |> sort_debug_vars
               |> separate alinea
             in
             string (Format.sprintf "%d |-->" rank) ^^ alinea ^^ variables_doc
         end
    |> separate hardline
end

(* Error checks *)

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


(* Unification functions *)

let rec unify orig_env v1 v2 : (Env.t, err) result =
  let env = { orig_env with Env.store = UF.copy orig_env.Env.store } in

  let find v =
    try Env.uvar v env
    with Not_found ->
      Printf.ksprintf invalid_arg
        "Constraint variable '%s' is unbound at this point"
        (Constraint.Var.name v)
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
  let generic (n : unode) () =
    Printf.ksprintf invalid_arg
      "Constraint variable '%s' is generic at this point"
      (Constraint.Var.name n.var)
  in

  let var = n1.var in

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

  let status =
    match (n1.status, n2.status) with
    | Flexible, Flexible -> Flexible
    | Generic, _ -> generic n1 ()
    | _, Generic -> generic n2 ()
  in

  let rank = min n1.rank n2.rank in

  { var; data; status; rank }


let unifiable env v1 v2 =
  match unify env v1 v2 with Ok _ -> true | Error _ -> false
