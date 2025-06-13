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

  module Env = struct
    module SMap = Constraint.SVar.Map

    type unif = Unif.Env.t

    type schemes = Generalization.scheme SMap.t

    type t =
      { unif : unif
      ; schemes : schemes
      }

    let empty = { unif = Unif.Env.empty; schemes = SMap.empty }

    let debug_schemes schemes =
      let open PPrint in
      schemes |> Constraint.SVar.Map.bindings
      |> List.map
           begin
             fun (s, sch) ->
               Constraint.SVar.print s ^^ colon ^^ break 1
               ^^ Generalization.debug_scheme sch
           end
      |> separate hardline

    let debug { unif; schemes } =
      let open PPrint in
      debug_schemes schemes ^^ Unif.Env.debug_env unif
      ^^ Unif.Env.debug_pool unif
  end

  type env = Env.t

  type log = PPrint.document list

  let make_logger c0 =
    let c0_erased = SatConstraint.erase c0 in
    fun (env : Env.t) ->
      Debug.print_header "DEBUG ENV" @@ Env.debug env;
      c0_erased
      |> ConstraintSimplifier.simplify env.unif
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

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t)
    (k : (a, e) normal_constraint -> (a, e) normal_constraint) :
    env * (a, e) normal_constraint =
    let unif_env : Env.unif ref = ref env.unif in
    let scheme_env : Env.schemes ref = ref env.schemes in

    (* We recommend calling the function [add_to_log] below
         whenever you get an updated environment.

         $ dune exec -- minihell --log-solver foo.test

         will show a log that will let you see the evolution
         of your input constraint (after simplification) as
         the solver progresses, which is useful for debugging.

         (You can also tweak this code temporarily to print stuff on
         stderr right away if you need dirtier ways to debug.)
    *)
    let add_to_log =
      let logger = if log then make_logger c0 else ignore in
      fun () -> logger { unif = !unif_env; schemes = !scheme_env }
    in

    let exception Located of Utils.loc * exn * Printexc.raw_backtrace in
    let locate_exn loc exn =
      match exn with
      | Located (_, _, _) as exn -> raise exn
      | base_exn ->
        let bt = Printexc.get_raw_backtrace () in
        raise @@ Located (loc, base_exn, bt)
    in

    let rec eval : type a b e f.
         (a, e) Constraint.t
      -> ((a, e) normal_constraint -> (b, f) normal_constraint)
      -> (b, f) normal_constraint =
      let open Constraint in
      let ( let+ ) nf f = T.map f nf in
      Debug.print_header "DEBUG ENV" @@ Unif.Env.debug_env !unif_env;
      Debug.print_header "DEBUG POOL" @@ Unif.Env.debug_pool !unif_env;

      fun c k ->
        match c with
        | Loc (loc, c) -> begin try eval c k with exn -> locate_exn loc exn end
        | Ret v -> k @@ NRet v
        | Err e -> k @@ NErr e
        | Map (c, f) -> begin
          eval c @@ fun nc ->
          match nc with
          | NRet v -> k @@ nret @@ fun sol -> f @@ v sol
          | NErr e -> k @@ NErr e
          | NDo p ->
            k @@ ndo
            @@
            let+ c = p in
            Map (c, f)
        end
        | MapErr (c, f) -> begin
          eval c @@ fun nc ->
          match nc with
          | NRet v -> k @@ NRet v
          | NErr e -> k @@ nerr @@ f e
          | NDo p ->
            k @@ ndo
            @@
            let+ c = p in
            MapErr (c, f)
        end
        | Conj (c, d) -> begin
          eval c @@ fun nc ->
          match nc with
          | NErr e -> k @@ NErr e
          | nc -> begin
            eval d @@ fun nd ->
            match (nc, nd) with
            | NErr e, _ | _, NErr e -> k @@ NErr e
            | NRet v, NRet w -> k @@ nret @@ fun sol -> (v sol, w sol)
            | NRet v, NDo q ->
              k @@ ndo
              @@
              let+ d = q in
              Conj (Ret v, d)
            | NDo p, NRet w ->
              k @@ ndo
              @@
              let+ c = p in
              Conj (c, Ret w)
            | NDo p, NDo q ->
              k @@ ndo
              @@
              let+ c = p in
              Conj (c, Do q)
          end
        end
        | Eq (x1, x2) -> begin
          match Unif.unify !unif_env x1 x2 with
          | Ok new_env ->
            unif_env := new_env;
            add_to_log ();

            k @@ nret @@ fun _sol -> ()
          | Error (Cycle cy) -> k @@ nerr @@ Cycle cy
          | Error (Clash (y1, y2)) ->
            let decoder = Decode.decode !unif_env () in
            k @@ nerr @@ Clash (decoder y1, decoder y2)
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
            unif_env := Unif.Env.add_flexible x s !unif_env;
            add_to_log ()
          end;

          eval c @@ fun nc ->
          match nc with
          | NRet v -> k @@ NRet v
          | NErr e -> k @@ NErr e
          | NDo p ->
            k @@ ndo
            @@
            let+ c = p in
            Exist (x, s, c)
        end
        | Decode v -> k @@ nret @@ fun sol -> sol v
        | Do p -> k @@ NDo p
        | DecodeScheme sch_var -> begin
          let scheme = Env.SMap.find sch_var !scheme_env in

          let body sol = sol @@ Generalization.body scheme in
          let quantifiers (sol : variable -> STLC.ty) : Structure.TyVar.t list =
            scheme |> Generalization.quantifiers
            |> List.map
                 begin
                   fun var ->
                     let (Constr ty) = sol var in
                     match ty with
                     | Var v -> v
                     | Arrow _ | Prod _ -> assert false
                 end
          in

          k @@ nret @@ fun sol -> (quantifiers sol, body sol)
        end
        | Instance (sch_var, w) -> begin
          let sch = Env.SMap.find sch_var !scheme_env in

          match Generalization.instantiate sch w !unif_env with
          | new_unif_env, Ok witnesses ->
            unif_env := new_unif_env;
            add_to_log ();

            k @@ nret @@ fun sol -> List.map sol witnesses
          | _, Error (Cycle cy) -> k @@ nerr @@ Cycle cy
          | new_unif_env, Error (Clash (y1, y2)) ->
            let decoder = Decode.decode new_unif_env () in
            k @@ nerr @@ Clash (decoder y1, decoder y2)
        end
        | Let (sch_var, var, c1, c2) -> begin
          let solve_c2 r1 k =
            eval c2 @@ fun nc2 ->
            match nc2 with
            | NRet r2 -> k @@ nret @@ fun on_sol -> (r1 on_sol, r2 on_sol)
            | NErr e -> k @@ NErr e
            | NDo p ->
              k @@ ndo
              @@
              let+ c2 = p in
              Let (sch_var, var, Ret r1, c2)
          in

          let solve_c1 k =
            if not @@ Unif.Env.mem var !unif_env then begin
              unif_env := Generalization.enter !unif_env;
              unif_env := Unif.Env.add_flexible var None !unif_env;
              add_to_log ()
            end;

            eval c1 @@ fun nc1 ->
            match nc1 with
            | NRet r1 -> begin
              let new_unif_env, _gammas, schemes =
                Generalization.exit [ var ] !unif_env
              in
              unif_env := new_unif_env;
              add_to_log ();

              assert (List.length schemes = 1);
              let scheme = List.hd schemes in

              Debug.print_header "DEBUG SCHEME"
              @@ Generalization.debug_scheme scheme;

              scheme_env := Env.SMap.add sch_var scheme !scheme_env;

              solve_c2 r1 k
            end
            | NErr e -> k @@ NErr e
            | NDo p ->
              k @@ ndo
              @@
              let+ c1 = p in
              Let (sch_var, var, c1, c2)
          in

          match Env.SMap.mem sch_var !scheme_env with
          | false -> solve_c1 k
          | true ->
            let r1 = match c1 with Ret r1 -> r1 | _ -> assert false in
            solve_c2 r1 k
        end
    in

    Debug.print_message "New evaluation of the Solver";
    add_to_log ();

    match eval c0 k with
    | exception Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" @@ MenhirLib.LexerUtil.range loc;
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result ->
      let env = { Env.unif = !unif_env; schemes = !scheme_env } in
      (env, result)
end
