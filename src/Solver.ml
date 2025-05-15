(*
   As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor].
*)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module SatConstraint = SatConstraint.Make (T)
  module ConstraintSimplifier = ConstraintSimplifier.Make (T)
  module ConstraintPrinter = ConstraintPrinter.Make (T)

  type unif_env = Unif.Env.t

  module SEnv = Map.Make (Constraint.SVar)

  type solver_env = Generalization.scheme SEnv.t

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
    let get_log () = logs |> Queue.to_seq |> List.of_seq in

    (add_to_log, get_log)

  (** See [../README.md] ("High-level description") or [Solver.mli] for a
      description of normal constraints and our expectations regarding the
      [eval] function. *)
  type ('a, 'e) normal_constraint =
    | NRet of 'a Constraint.on_sol
    | NErr of 'e
    | NDo of ('a, 'e) Constraint.t T.t

  let nret t = NRet t

  let nerr t = NErr t

  let ndo t = NDo t

  let eval (type a e) ~log (unif_env : unif_env) (c0 : (a, e) Constraint.t) :
    log * unif_env * (a, e) normal_constraint =
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
    let add_to_log, get_log =
      if log then make_logger c0 else (ignore, fun _ -> [])
    in

    let solver_env : solver_env ref = ref SEnv.empty in
    let unif_env : unif_env ref = ref unif_env in

    let rec eval : type a e. (a, e) Constraint.t -> (a, e) normal_constraint =
      let open Constraint in
      let ( let+ ) nf f = T.map f nf in

      function
      | Ret v -> NRet v
      | Err e -> NErr e
      | Map (c, f) -> begin
        match eval c with
        | NRet v -> nret @@ fun sol -> f @@ v sol
        | NErr e -> NErr e
        | NDo p ->
          ndo
          @@
          let+ c = p in
          Map (c, f)
      end
      | MapErr (c, f) -> begin
        match eval c with
        | NRet v -> NRet v
        | NErr e -> nerr @@ f e
        | NDo p ->
          ndo
          @@
          let+ c = p in
          MapErr (c, f)
      end
      | Conj (c, d) -> begin
        match (eval c, eval d) with
        | NRet v, NRet w -> nret @@ fun sol -> (v sol, w sol)
        | NErr e, _ | _, NErr e -> NErr e
        | NDo p, NDo q ->
          ndo
          @@
          let+ c = p in
          Conj (c, Do q)
        | NDo p, NRet w ->
          ndo
          @@
          let+ c = p in
          Conj (c, Ret w)
        | NRet v, NDo q ->
          ndo
          @@
          let+ d = q in
          Conj (Ret v, d)
      end
      | Eq (x1, x2) -> begin
        match Unif.unify !unif_env x1 x2 with
        | Ok new_env ->
          unif_env := new_env;
          add_to_log !unif_env;

          nret @@ fun _sol -> ()
        | Error (Cycle cy) -> nerr @@ Cycle cy
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode !unif_env () in
          nerr @@ Clash (decoder y1, decoder y2)
      end
      | Exist (x, s, c) -> begin
        (*
          Our solver may re-enter existentials that
          it has already traversed. In this case we
          do not want to re-bind them in the environment,
          but reuse the previous binding which accumulated
          information through unifications.
        *)
        if not @@ Unif.Env.mem x !unif_env then begin
          unif_env := Unif.Env.add x s !unif_env;
          add_to_log !unif_env
        end;

        match eval c with
        | NRet v -> NRet v
        | NErr e -> NErr e
        | NDo p ->
          ndo
          @@
          let+ c = p in
          Exist (x, s, c)
      end
      | Decode v -> nret @@ fun sol -> sol v
      | Do p -> NDo p
      | DecodeScheme sch_var ->
        ignore sch_var;
        failwith "Solver.eval.DecodeScheme TODO"
      | Instance (sch_var, w) -> begin
        let sch = SEnv.find sch_var !solver_env in
        let witnesses, var = Generalization.instantiate sch !unif_env in

        match Unif.unify !unif_env w var with
        | Ok new_env ->
          unif_env := new_env;
          add_to_log !unif_env;

          nret @@ fun sol -> List.map sol witnesses
        | Error (Cycle cy) -> nerr @@ Cycle cy
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode !unif_env () in
          nerr @@ Clash (decoder y1, decoder y2)
      end
      | Let (sch_var, var, c1, c2) -> begin
        unif_env := Generalization.enter !unif_env;

        let new_unif_env, uvar = Generalization.flexible None !unif_env in
        unif_env := new_unif_env;
        add_to_log !unif_env;

        if not @@ Unif.Env.mem var !unif_env then begin
          unif_env := Unif.Env.add var None !unif_env;
          add_to_log !unif_env
        end;

        match eval c1 with
        | NRet r1 -> begin
          let new_unif_env, gammas, schemes =
            Generalization.exit [ uvar ] !unif_env
          in
          unif_env := new_unif_env;

          assert (List.length gammas = 1);
          assert (List.length schemes = 1);

          solver_env := SEnv.add sch_var (List.hd schemes) !solver_env;

          match eval c2 with
          | NRet r2 -> nret @@ fun on_sol -> (r1 on_sol, r2 on_sol)
          | NErr e -> NErr e
          | NDo p ->
            ndo
            @@
            let+ c2 = p in
            Let (sch_var, var, Ret r1, c2)
        end
        | NErr e -> NErr e
        | NDo p ->
          ndo
          @@
          let+ c1 = p in
          Let (sch_var, var, c1, c2)
      end
    in

    add_to_log !unif_env;
    let result = eval c0 in
    (get_log (), !unif_env, result)
end
