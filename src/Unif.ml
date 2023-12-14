module UF = UnionFind.Make(UnionFind.StoreMap)

type uvar = unode UF.rref
and unode = {
  var: Constraint.variable;
  data: structure option;
}
and structure = uvar Structure.t

module Env : sig
  type t = {
    store: unode UF.store;
    map: uvar Constraint.Var.Map.t;
  }

  val empty : unit -> t

  val copy : t -> t

  val mem : Constraint.Var.t -> t -> bool

  val add : Constraint.Var.t -> Constraint.structure option -> t -> t

  val uvar : t -> Constraint.Var.t -> uvar

  val unode : t -> Constraint.Var.t -> unode
end = struct
  type t = {
    store: unode UF.store;
    map: uvar Constraint.Var.Map.t;
  }

  let empty () =
    let store = UF.new_store () in
    let map = Constraint.Var.Map.empty in
    { store; map }

  let copy env = { env with store = UF.copy env.store }

  let uvar env v : uvar = Constraint.Var.Map.find v env.map

  let unode env v = UF.get env.store (uvar env v)

  let mem var env =
    Constraint.Var.Map.mem var env.map

  let add var structure env =
    let data = Option.map (Structure.map (uvar env)) structure in
    let uvar = UF.make env.store { var; data } in
    { env with map = Constraint.Var.Map.add var uvar env.map }
end

type clash = Constraint.variable Utils.clash
exception Clash of clash
exception Cycle of Constraint.variable Utils.cycle

type err =
  | Clash of clash
  | Cycle of Constraint.variable Utils.cycle

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
  Queue.add (Env.uvar env v1, Env.uvar env v2) queue;
  match unify_uvars env.Env.store queue with
  | exception Clash clash -> Error (Clash clash)
  | () ->
  match check_no_cycle env (Env.uvar env v1) with
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
        Structure.map2 (fun v1 v2 ->
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
