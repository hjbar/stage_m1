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
    let c0_erased = SatConstraint.erase c0 in
    fun env ->
      c0_erased
      |> ConstraintSimplifier.simplify env
      |> ConstraintPrinter.print_sat_constraint |> Utils.string_of_doc
      |> prerr_endline

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
    unif_env * (a, e) normal_constraint =
    (* We recommend calling the function [add_to_log] above
         whenever you get an updated environment.

         $ dune exec -- minihell --log-solver foo.test

         will show a log that will let you see the evolution
         of your input constraint (after simplification) as
         the solver progresses, which is useful for debugging.

         (You can also tweak this code temporarily to print stuff on
         stderr right away if you need dirtier ways to debug.)
    *)
    let add_to_log = if log then make_logger c0 else ignore in

    let exception Located of Utils.loc * exn * Printexc.raw_backtrace in
    let locate_exn loc exn =
      match exn with
      | Located (_, _, _) as exn -> raise exn
      | base_exn ->
        let bt = Printexc.get_raw_backtrace () in
        raise @@ Located (loc, base_exn, bt)
    in

    let solver_env : solver_env ref = ref SEnv.empty in
    let unif_env : unif_env ref = ref unif_env in

    let print_enter, print_comeback, print_re_comeback =
      let cpt = ref ~-1 in

      let f1 s =
        incr cpt;
        Format.printf "Enter %s - %d\n%!" s !cpt
      in

      let f2 s =
        incr cpt;
        Format.printf "Comeback %s - %d\n%!" s !cpt
      in

      let f3 s =
        incr cpt;
        Format.printf "Re-comeback %s - %d\n%!" s !cpt
      in

      (f1, f2, f3)
    in

    let rec eval : type a e. (a, e) Constraint.t -> (a, e) normal_constraint =
      let open Constraint in
      let ( let+ ) nf f = T.map f nf in

      function
      | Loc (loc, c) -> begin
        print_enter "Loc";
        try eval c with exn -> locate_exn loc exn
      end
      | Ret v ->
        print_enter "Ret";
        NRet v
      | Err e ->
        print_enter "Err";
        NErr e
      | Map (c, f) -> begin
        print_enter "Map";

        let res =
          match eval c with
          | NRet v -> nret @@ fun sol -> f @@ v sol
          | NErr e -> NErr e
          | NDo p ->
            ndo
            @@
            let+ c = p in
            Map (c, f)
        in

        print_comeback "Map";
        res
      end
      | MapErr (c, f) -> begin
        print_enter "MapErr";

        let res =
          match eval c with
          | NRet v -> NRet v
          | NErr e -> nerr @@ f e
          | NDo p ->
            ndo
            @@
            let+ c = p in
            MapErr (c, f)
        in

        print_comeback "MapErr";
        res
      end
      | Conj (c, d) -> begin
        print_enter "Conj";

        let res = eval c in
        print_comeback "Conj";

        match res with
        | NErr e -> NErr e
        | nc -> begin
          let res =
            match (nc, eval d) with
            | NErr e, _ | _, NErr e -> NErr e
            | NRet v, NRet w -> nret @@ fun sol -> (v sol, w sol)
            | NRet v, NDo q ->
              ndo
              @@
              let+ d = q in
              Conj (Ret v, d)
            | NDo p, NRet w ->
              ndo
              @@
              let+ c = p in
              Conj (c, Ret w)
            | NDo p, NDo q ->
              ndo
              @@
              let+ c = p in
              Conj (c, Do q)
          in

          print_re_comeback "Conj";
          res
        end
      end
      | Eq (x1, x2) -> begin
        print_enter "Eq";
        match Unif.unify !unif_env x1 x2 with
        | Ok new_env ->
          unif_env := new_env;
          add_to_log !unif_env;
          if Constraint.Var.name x1 = "wu" then
            Debug.print_header "UNIF ENV" (Unif.Env.debug !unif_env);
          nret @@ fun _sol -> ()
        | Error (Cycle cy) -> nerr @@ Cycle cy
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode !unif_env () in
          nerr @@ Clash (decoder y1, decoder y2)
      end
      | Exist (x, s, c) -> begin
        print_enter "Exist";
        (*
          Our solver may re-enter existentials that
          it has already traversed. In this case we
          do not want to re-bind them in the environment,
          but reuse the previous binding which accumulated
          information through unifications.
        *)
        if not @@ Unif.Env.mem x !unif_env then begin
          unif_env := Unif.Env.add_flexible x s !unif_env;
          add_to_log !unif_env
        end;

        let res =
          match eval c with
          | NRet v -> NRet v
          | NErr e -> NErr e
          | NDo p ->
            ndo
            @@
            let+ c = p in
            Exist (x, s, c)
        in

        print_comeback "Exist";
        res
      end
      | Decode v ->
        print_enter "Decode";
        nret @@ fun sol -> sol v
      | Do p ->
        print_enter "Do";
        NDo p
      | DecodeScheme sch_var ->
        print_enter "DecodeScheme";
        let scheme = SEnv.find sch_var !solver_env in

        let body sol = sol @@ Generalization.body scheme in
        let quantifiers (sol : variable -> STLC.ty) : Structure.TyVar.t list =
          scheme |> Generalization.quantifiers
          |> List.map
               begin
                 fun var ->
                   let (Constr ty) = sol var in
                   match ty with Var v -> v | Arrow _ | Prod _ -> assert false
               end
        in

        nret @@ fun sol -> (quantifiers sol, body sol)
      | Instance (sch_var, w) -> begin
        print_enter "Instance";

        let sch = SEnv.find sch_var !solver_env in

        Format.printf "Before instantiate\n%!";
        Debug.print_header "DEBUG ENV" (Unif.Env.debug !unif_env);

        match Generalization.instantiate sch w !unif_env with
        | Ok (new_unif_env, witnesses) ->
          unif_env := new_unif_env;
          add_to_log !unif_env;

          Format.printf "After instantiate (%s <= %s) at level %d\n%!"
            (Constraint.SVar.print sch_var |> Utils.string_of_doc)
            (Constraint.Var.print w |> Utils.string_of_doc)
            (Unif.Env.get_young !unif_env);
          Debug.print_header "DEBUG ENV" (Unif.Env.debug !unif_env);

          nret @@ fun sol -> List.map sol witnesses
        | Error (Cycle cy) -> nerr @@ Cycle cy
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode !unif_env () in
          nerr @@ Clash (decoder y1, decoder y2)
      end
      | Let (sch_var, var, c1, c2) -> begin
        print_enter "Let";
        unif_env := Generalization.enter !unif_env;

        if not @@ Unif.Env.mem var !unif_env then begin
          unif_env := Unif.Env.add_flexible var None !unif_env;
          add_to_log !unif_env
        end;

        let res = eval c1 in
        print_comeback "Let";

        match res with
        | NRet r1 -> begin
          let new_unif_env, _gammas, schemes =
            Generalization.exit [ var ] !unif_env
          in
          unif_env := new_unif_env;
          add_to_log !unif_env;

          let scheme =
            assert (List.length schemes = 1);
            List.hd schemes
          in

          Debug.print_header "DEBUG ENV" (Unif.Env.debug !unif_env);
          Debug.print_header "DEBUG SCHEME" (Generalization.debug_scheme scheme);

          solver_env := SEnv.add sch_var scheme !solver_env;

          let res =
            match eval c2 with
            | NRet r2 -> nret @@ fun on_sol -> (r1 on_sol, r2 on_sol)
            | NErr e -> NErr e
            | NDo p ->
              ndo
              @@
              let+ c2 = p in
              Let (sch_var, var, Ret r1, c2)
          in

          print_re_comeback "Let";
          res
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
    match eval c0 with
    | exception Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" @@ MenhirLib.LexerUtil.range loc;
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result -> (!unif_env, result)
end
