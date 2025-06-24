(* The UnionFind module *)

module UF = UnionFind.Make (UnionFind.StoreMap)

(* Variable type *)

type var = Constraint.variable

(* Structure type *)

type structure = var Structure.t option

(* The internal representation in terms of union-find nodes. *)
type uvar = unode UF.rref

(* Data associated with a variable *)
and unode = {
  var : var;
  data : uvar Structure.t option;
}

(* The user-facing representation hides union-find nodes,
   replaced by the corresponding constraint variables. *)

type repr = {
  var : var;
  structure : structure;
}

(* Environment module implementation *)

module Env = struct
  (* Definition of the type [t] *)

  type t = {
    store : unode UF.store;
    map : uvar Constraint.Var.Map.t;
  }

  (* Empty environment *)

  let empty () =
    let store = UF.new_store () in
    let map = Constraint.Var.Map.empty in
    { store; map }


  (* Membership test functions *)

  let mem var env = Constraint.Var.Map.mem var env.map

  (* Getter functions for environment data *)

  let uvar var env = Constraint.Var.Map.find var env.map

  let repr var env =
    let { var; data } = UF.get env.store (uvar var env) in
    let var_of_uvar uv = (UF.get env.store uv).var in
    let structure = Option.map (Structure.map var_of_uvar) data in
    { var; structure }


  (* Functions to add  variables to the environment *)

  let add var structure env =
    let data = Option.map (Structure.map (fun v -> uvar v env)) structure in
    let uvar = UF.make env.store { var; data } in
    { env with map = Constraint.Var.Map.add var uvar env.map }


  (* Debugging functions *)

  let debug_var var uvar =
    let module PP = PPrint in
    let ( ^^ ) = PP.( ^^ ) in

    if not (Constraint.Var.eq var uvar.var) then
      (* not representative *)
      Constraint.Var.print var
      ^^ PP.string " |--> "
      ^^ Constraint.Var.print uvar.var
    else
      let structure_doc =
        match uvar.structure with
        | None -> PP.empty
        | Some s -> PP.string "= " ^^ Structure.print Constraint.Var.print s
      in
      Constraint.Var.print var ^^ PP.space ^^ structure_doc


  let debug_env env =
    let open PPrint in
    env.map
    |> Constraint.Var.Map.bindings
    |> List.map fst
    |> List.map (fun v -> debug_var v (repr v env))
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

      match merge with
      | None -> clash ()
      | Some d -> Some d
    end
  in

  { n1 with data }


let unifiable env v1 v2 =
  match unify env v1 v2 with
  | Ok _ -> true
  | Error _ -> false
