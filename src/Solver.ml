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

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t)
    : log * env * (a, e) normal_constraint
  =
    let add_to_log, get_log =
      if log then make_logger c0
      else ignore, (fun _ -> [])
    in
(*sujet
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
    Utils.not_yet "Solver.eval" (env, c0, add_to_log, get_log)
/sujet*)
(*corrige*)
    let env = ref env in
    let decode v = Decode.decode !env v in
    let rec eval
      : type a e . (a, e) Constraint.t -> (a, e) normal_constraint
    =
      let open Constraint in
      let (let+) nf f = T.map f nf in
      function
      | Ret v -> NRet v
      | Err e -> NErr e
      | Map (c, f) ->
        begin match eval c with
        | NRet v -> NRet (fun sol -> f (v sol))
        | NErr e -> NErr e
        | NDo p -> NDo (let+ c = p in Map (c, f))
        end
      | MapErr (c, f) ->
        begin match eval c with
        | NRet v -> NRet v
        | NErr e -> NErr (f e)
        | NDo p -> NDo (let+ c = p in MapErr (c, f))
        end
      | Conj (c, d) ->
        begin match eval c with
          | NErr e -> NErr e
          | NDo p -> NDo (let+ c = p in Conj (c, d))
          | NRet v ->
          match eval d with
          | NErr e -> NErr e
          | NRet w -> NRet (fun sol -> (v sol, w sol))
          | NDo p -> NDo (let+ d = p in Conj (Ret v, d))
        end
      | Eq (x1, x2) ->
        let result = Unif.unify !env x1 x2 in
        begin match result with
          | Ok new_env ->
            env := new_env;
            add_to_log !env;
            NRet (fun _sol -> ())
          | Error (Cycle cy) ->
            NErr (Cycle cy)
          | Error (Clash (y1, y2)) ->
            NErr (Clash (decode y1, decode y2))
        end
      | Exist (x, s, c) ->
        (* Our solver may re-enter existentials
           that it has already traversed. In this
           case we do not want to re-bind them in the
           environment, but reuse the previous binding
           which accumulated information through unifications. *)
        if not (Unif.Env.mem x !env) then
          env := Unif.Env.add x s !env;
        add_to_log !env;
        begin match eval c with
        | NRet v -> NRet v
        | NErr e -> NErr e
        | NDo p -> NDo (let+ c = p in Exist (x, s, c))
        end
      | Decode v -> NRet (fun sol -> sol v)
      | Do p -> NDo p
    in
    add_to_log !env;
    let result = eval c0 in
    get_log (), !env, result
(*/corrige*)

end
