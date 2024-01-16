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
            let open Constraint in
            match c with
            (* Trivial base cases *)
            | Ret map -> env, NRet map
            | Err e -> env, NErr e
            (* [Exist] gets consumed with a side-effect of a new variable in the context *)
            | Exist (x, ty, u) -> reduce (Unif.Env.add x ty env) u
            (* [Decode] is the "true" base case because the function that we construct here
               is the one that will end up inside an [NRet] eventually.
               Whatever final mapping we get, it will be instanciated by [x] *)
            | Decode x -> env, NRet (( |> ) x)
            (* We don't compute under [Do] nodes *)
            | Do p -> env, NDo p
            (* Simplify [x] then apply [f] *)
            | Map (x, f) -> (
                let env, res = reduce env x in
                env, match res with
                | NRet map -> NRet (fun x -> f (map x))
                | NErr e -> NErr e
                (* Transpose [NDo] with [Map] *)
                | NDo ts -> NDo (
                    ts |> T.map (fun t ->
                    Map (t, f)
                ))
            )
            (* Simplify [x] then apply [f] *)
            | MapErr (x, fe) -> (
                let env, res = reduce env x in
                env, match res with
                | NErr e -> NErr (fe e)
                | NRet map -> NRet map
                (* Transpose [NDo] with [MapErr] *)
                | NDo ts -> NDo (
                    ts |> T.map (fun t ->
                    MapErr (t, fe)
                ))
            )
            | Conj (c1, c2) -> (
                let env, res1 = reduce env c1 in
                let env, res2 = reduce env c2 in
                env, match res1, res2 with
                | NRet map1, NRet map2 -> NRet (fun v -> (map1 v, map2 v))
                | NErr e, _ | _, NErr e -> NErr e
                (* Transpose [NDo] with [Conj].
                   All cases must preserve the number of [Do] or [NDo] constructors
                   or generated terms will be too deep.
                   In particular it is important that the [Do _, Do _] case doesn't
                   combine the two [Do] constructors into only one [NDo]. *)
                | NRet map1, NDo ts2 -> NDo (
                    ts2 |> T.map (fun t2 ->
                    Conj (Ret map1, t2)
                ))
                | NDo ts1, NRet map2 -> NDo (
                    ts1 |> T.map (fun t1 ->
                    Conj (t1, Ret map2)
                ))
                | NDo ts1, NDo ts2 -> NDo (
                    ts1 |> T.map (fun t1 ->
                    Conj (t1, Do ts2)
                ))
            )
            | Eq (v1, v2) -> (
                (* Add the equality to the context *)
                match Unif.unify env v1 v2 with
                | Ok env -> env, NRet (fun _ -> ())
                | Error (Unif.Clash (v1, v2)) ->
                    (* Turn a clash on variables into a clash on types *)
                    env, NErr (Clash (Decode.decode env v1, Decode.decode env v2))
                | Error (Unif.Cycle c) -> env, NErr (Cycle c)
            )
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
