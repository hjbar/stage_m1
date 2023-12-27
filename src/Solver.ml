(*
   As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor].
*)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make(T)
  module SatConstraint = SatConstraint.Make(T)
  module ConstraintSimplifier = ConstraintSimplifier.Make(T)
  module ConstraintPrinter = ConstraintPrinter.Make(T)

  type env = Unif.Env.t
  type log = PPrint.document list

  let make_logger c0 =
    let logs = Queue.create () in
    let c0_erased = SatConstraint.erase c0 in
    let add_to_log env =
      let doc =
        c0_erased
        |> ConstraintSimplifier.simplify env
        |> ConstraintPrinter.print_sat_constraint
      in
      Queue.add doc logs
    in
    let get_log () =
      logs |> Queue.to_seq |> List.of_seq
    in
    add_to_log, get_log

  (** Expand variable to its type. This is naturally a subprocedure of
      converting from a [var Utils.clash] (returned by [Unif]) and a
      [STLC.ty Utils.Clash] (output of [eval]). *)
  let rec typeof (env : env) (var : Constraint.variable) : STLC.ty =
      let repr = Unif.Env.repr var env in
      match repr.structure with
      (* Just recurse into all types. Very straightforward. *)
      | Some (Var y) -> Constr (Var y)
      | Some (Arrow (t, u)) -> Constr (Arrow (typeof env t, typeof env u))
      | Some (Prod tup) -> Constr (Prod (List.map (typeof env) tup))
      (* Honestly this arm is probably unreachable code because
         there's no way a variable that doesn't define any structure
         could be part of a clash. *)
      | None -> Constr (Var (Structure.TyVar.fresh "x"))

  (** See [../README.md] ("High-level description") or [Solver.mli]
      for a description of normal constraints and
      our expectations regarding the [eval] function. *)
  type ('a, 'e) normal_constraint =
    | NRet of 'a Constraint.on_sol
    | NErr of 'e
    | NDo of ('a, 'e) Constraint.t T.t

    (** Produces an effectful closure that returns false when given the same
        value twice in a row.
        This is is then used as [if delta x then log x] to log the value of `x`
        exactly when it changes *)
  let make_delta () : 'a -> bool =
        let x = ref None in
        let delta y =
            let res = (!x <> Some y) in
            x := Some y;
            res
        in delta

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t)
    : log * env * (a, e) normal_constraint
  =
    let add_to_log, get_log =
      if log then make_logger c0
      else ignore, (fun _ -> [])
    in
    let delta = make_delta () in
    let rec reduce : type a e . env -> (a, e) Constraint.t -> env * (a, e) normal_constraint =
        fun env c ->
        if delta env then add_to_log env;
        match c with
        | Ret map -> (env, NRet map)
        | Err e -> (env, NErr e)
        | Map (x, f) -> (
            match reduce env x with
            | (env', NRet map) -> (env', NRet (fun x -> f (map x)))
            | (env', NErr e) -> (env', NErr e)
            | (_, NDo _) -> failwith "Solver.reduce Map / Do"
        )
        | MapErr (x, fe) -> (
            match reduce env x with
            | (env', NRet map) -> (env', NRet map)
            | (env', NErr e) -> (env', NErr (fe e))
            | (_, NDo _) -> failwith "Solver.reduce MapErr / Do"
        )
        | Conj (c1, c2) -> (
            match reduce env c1 with
            | (env', NRet map1) -> (
                match reduce env' c2 with
                | (env'', NRet map2) -> (env'', NRet (fun v -> (map1 v, map2 v)))
                | (env'', NErr e) -> (env'', NErr e)
                | (_, NDo _) -> failwith "Solver.reduce Conj / Do"
            )
            | (env', NErr e) -> (env', NErr e)
            | (_, NDo _) -> failwith "Solver.reduce Conj / Do"
        )
        | Eq (v1, v2) -> (
            match Unif.unify env v1 v2 with
            | Ok env' -> (env', NRet (fun _ -> ()))
            | Error (Unif.Clash (v1, v2)) -> (env, NErr (Clash (typeof env v1, typeof env v2)))
            | Error (Unif.Cycle c) -> (env, NErr (Cycle c))
        )
        | Exist (x, ty, u) -> (
            (* Possible improvement: remove dangling existentials ? *)
            reduce (Unif.Env.add x ty env) u
        )
        | Decode x -> (
            (env, NRet (fun map -> map x))
        )
        | Do _ -> Utils.not_yet "Solver.reduce Do" (add_to_log, reduce)
    in
    let (env, res) = reduce env c0 in
    (get_log (), env, res)
    (* We recommend calling the function [add_to_log] above
       whenever you get an updated environment. Then call
       [get_log] at the end to get a list of log message.

       $ dune exec -- minihell --log-solver foo.test

       will show a log that will let you see the evolution
       of your input constraint (after simplification) as
       the solver progresses, which is useful for debugging.

       (You can also tweak this code temporarily to print stuff on
       stderr right away if you need dirtier ways to debug.)
    *)

end
