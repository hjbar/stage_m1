module UF = UnionFind.Make(UnionFind.StoreMap)

type uvar = unode UF.rref
and unode = {
  var: Constraint.variable;
  data: structure option;
}
and structure = uvar STLC.structure

module Env : sig
  type t = {
    store: unode UF.store;
    map: uvar Constraint.Var.Map.t;
  }

  val empty : unit -> t

  val copy : t -> t

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

  let add var structure env =
    let data = Option.map (Structure.map (uvar env)) structure in
    let uvar = UF.make env.store { var; data } in
    { env with map = Constraint.Var.Map.add var uvar env.map }
end

type clash = Constraint.variable * Constraint.variable
exception Clash of clash

let rec unify orig_env v1 v2 : (Env.t, clash) result =
  let env = { orig_env with Env.store = UF.copy orig_env.Env.store } in
  let queue = Queue.create () in
  Queue.add (Env.uvar env v1, Env.uvar env v2) queue;
  match unify_uvars env.Env.store queue with
  | exception Clash clash -> Error clash
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
