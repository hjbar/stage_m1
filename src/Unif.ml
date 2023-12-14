(*sujet
(* There is nothing that you have to implement in this file/module,
   and no particular need to read its implementation. On the other hand,
   you want to understand the interface exposed in [Unif.mli] has it
   is important to implement a constraint solver in Solver.ml. *)
/sujet*)

module UF = UnionFind.Make(UnionFind.StoreMap)

type var = Constraint.variable

(* The internal representation in terms of union-find nodes. *)
type uvar = unode UF.rref
and unode = {
  var: var;
  data: uvar Structure.t option;
}

(* The user-facing representation hides union-find nodes,
   replaced by the corresponding constraint variables. *)
type repr = {
  var: var;
  structure: var Structure.t option;
}

module Env : sig
  type t = {
    store: unode UF.store;
    map: uvar Constraint.Var.Map.t;
  }

  val empty : unit -> t

  val mem : var -> t -> bool

  val add : var -> Constraint.structure option -> t -> t

  val uvar : var -> t -> uvar

  val repr : var -> t -> repr
end = struct
  type t = {
    store: unode UF.store;
    map: uvar Constraint.Var.Map.t;
  }

  let empty () =
    let store = UF.new_store () in
    let map = Constraint.Var.Map.empty in
    { store; map }

  let uvar var env : uvar =
    Constraint.Var.Map.find var env.map

  let mem var env =
    Constraint.Var.Map.mem var env.map

  let add var structure env =
    let data = Option.map (Structure.map (fun v -> uvar v env)) structure in
    let uvar = UF.make env.store { var; data } in
    { env with map = Constraint.Var.Map.add var uvar env.map }

  let repr var env =
    let { var; data; } = UF.get env.store (uvar var env) in
    let var_of_uvar uv = (UF.get env.store uv).var in
    let structure = Option.map (Structure.map var_of_uvar) data in
    { var; structure; }
end

type clash = var Utils.clash
exception Clash of clash
exception Cycle of var Utils.cycle

type err =
  | Clash of clash
  | Cycle of var Utils.cycle

let check_no_cycle env v =
  let open struct
    type status = Visiting | Visited
  end in
  let table = Hashtbl.create 42 in
  let rec loop v =
    let n = UF.get env.Env.store v in
    match Hashtbl.find table n.var with
    | Visited ->
      ()
    | Visiting ->
      raise (Cycle (Utils.Cycle n.var))
    | exception Not_found ->
      Hashtbl.replace table n.var Visiting;
      Option.iter (Structure.iter loop) n.data;
      Hashtbl.replace table n.var Visited;
  in loop v

let rec unify orig_env v1 v2 : (Env.t, err) result =
  let env = { orig_env with Env.store = UF.copy orig_env.Env.store } in
  let queue = Queue.create () in
  Queue.add (Env.uvar v1 env, Env.uvar v2 env) queue;
  match unify_uvars env.Env.store queue with
  | exception Clash clash -> Error (Clash clash)
  | () ->
  match check_no_cycle env (Env.uvar v1 env) with
  | exception Cycle v -> Error (Cycle v)
  | () -> Ok env

and unify_uvars store (queue : (uvar * uvar) Queue.t) =
  match Queue.take_opt queue with
  | None -> ()
  | Some (u1, u2) ->
    ignore (UF.merge store (merge queue) u1 u2);
    unify_uvars store queue

and merge queue (n1 : unode) (n2 : unode) : unode =
  let clash () = raise (Clash (n1.var, n2.var)) in
  let data =
    match n1.data, n2.data with
    | None, None -> None
    | None, (Some _ as d) | (Some _ as d), None -> d
    | Some st1, Some st2 ->
      match
        Structure.merge (fun v1 v2 ->
          Queue.add (v1, v2) queue;
          v1
        ) st1 st2
      with
      | None -> clash ()
      | Some d -> Some d
  in
  { n1 with data }

let unifiable env v1 v2 =
  match unify env v1 v2 with
  | Ok _ -> true
  | Error _ -> false
