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
  type ('ok, 'err) normal_constraint =
    | NRet : 'a Constraint.on_sol -> ('a, 'e) normal_constraint
    | NErr : 'e -> ('a, 'e) normal_constraint
    | NDo : ('a, 'e) Constraint.t T.t -> ('a, 'e) normal_constraint

  type ('a, 'e) cont =
    env * ('a, 'e) normal_constraint -> env * ('a, 'e) normal_constraint

  let nret t = NRet t

  let nerr t = NErr t

  let ndo p = NDo p

  let eval : type a e.
       log:bool
    -> env
    -> (a, e) Constraint.t
    -> (a, e) cont
    -> env * (a, e) normal_constraint =
   fun ~log env c0 k ->
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
      fun env -> logger env
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
         env
      -> (a, e) Constraint.t
      -> (env * (a, e) normal_constraint -> env * (b, f) normal_constraint)
      -> env * (b, f) normal_constraint =
      let open Constraint in
      let ( let+ ) nf f = T.map f nf in

      fun env c k ->
        match c with
        | Loc (loc, c) -> begin
          try eval env c k with exn -> locate_exn loc exn
        end
        | Ret v -> k (env, NRet v)
        | Err e -> k (env, NErr e)
        | Map (c, f) -> begin
          eval env c @@ fun (env, nc) ->
          match nc with
          | NRet v -> k (env, nret @@ fun sol -> f @@ v sol)
          | NErr e -> k (env, NErr e)
          | NDo p ->
            k
              ( env
              , ndo
                @@ let+ c = p in
                   Map (c, f) )
        end
        | MapErr (c, f) -> begin
          eval env c @@ fun (env, nc) ->
          match nc with
          | NRet v -> k (env, NRet v)
          | NErr e -> k (env, nerr @@ f e)
          | NDo p ->
            k
              ( env
              , ndo
                @@ let+ c = p in
                   MapErr (c, f) )
        end
        | Conj (c, d) -> begin
          eval env c @@ fun (env, nc) ->
          match nc with
          | NErr e -> k (env, NErr e)
          | nc -> begin
            eval env d @@ fun (env, nd) ->
            match (nc, nd) with
            | NErr e, _ | _, NErr e -> k (env, NErr e)
            | NRet v, NRet w -> k (env, nret @@ fun sol -> (v sol, w sol))
            | NRet v, NDo q ->
              k
                ( env
                , ndo
                  @@ let+ d = q in
                     Conj (Ret v, d) )
            | NDo p, NRet w ->
              k
                ( env
                , ndo
                  @@ let+ c = p in
                     Conj (c, Ret w) )
            | NDo p, NDo q ->
              k
                ( env
                , ndo
                  @@ let+ c = p in
                     Conj (c, Do q) )
          end
        end
        | Eq (x1, x2) -> begin
          match Unif.unify env.unif x1 x2 with
          | Ok unif ->
            let env = { env with unif } in
            add_to_log env;

            k (env, nret @@ fun _sol -> ())
          | Error (Cycle cy) -> k (env, nerr @@ Cycle cy)
          | Error (Clash (y1, y2)) ->
            let decoder = Decode.decode env.unif () in
            k (env, nerr @@ Clash (decoder y1, decoder y2))
        end
        | Exist (x, s, c) -> begin
          (*
          Our solver may re-enter existentials that
          it has already traversed. In this case we
          do not want to re-bind them in the environment,
          but reuse the previous binding which accumulated
          information through unifications.
          *)
          let env =
            if Unif.Env.mem x env.unif then env
            else
              let unif = Unif.Env.add_flexible x s env.unif in
              let env = { env with unif } in
              add_to_log env;
              env
          in

          eval env c @@ fun (env, nc) ->
          match nc with
          | NRet v -> k (env, NRet v)
          | NErr e -> k (env, NErr e)
          | NDo p ->
            k
              ( env
              , ndo
                @@ let+ c = p in
                   Exist (x, s, c) )
        end
        | Decode v -> k (env, nret @@ fun sol -> sol v)
        | Do p -> k (env, NDo p)
        | DecodeScheme sch_var -> begin
          let scheme = Env.SMap.find sch_var env.schemes in

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

          k (env, nret @@ fun sol -> (quantifiers sol, body sol))
        end
        | Instance (sch_var, w) -> begin
          let sch = Env.SMap.find sch_var env.schemes in

          let unif, result = Generalization.instantiate sch w env.unif in

          let env = { env with unif } in
          add_to_log env;

          match result with
          | Ok witnesses -> k (env, nret @@ fun sol -> List.map sol witnesses)
          | Error (Cycle cy) -> k (env, nerr @@ Cycle cy)
          | Error (Clash (y1, y2)) ->
            let decoder = Decode.decode env.unif () in
            k (env, nerr @@ Clash (decoder y1, decoder y2))
        end
        | Let (sch_var, var, c1, c2) -> begin
          let solve_c2 env r1 k =
            eval env c2 @@ fun (env, nc2) ->
            match nc2 with
            | NRet r2 -> k (env, nret @@ fun on_sol -> (r1 on_sol, r2 on_sol))
            | NErr e -> k (env, NErr e)
            | NDo p ->
              k
                ( env
                , ndo
                  @@
                  let+ c2 = p in
                  Let (sch_var, var, Ret r1, c2) )
          in

          let solve_c1 (env : env) k =
            let env =
              if Unif.Env.mem var env.unif then env
              else
                let unif = Generalization.enter env.unif in
                let unif = Unif.Env.add_flexible var None unif in
                let env = { env with unif } in
                add_to_log env;
                env
            in

            eval env c1 @@ fun (env, nc1) ->
            match nc1 with
            | NRet r1 -> begin
              let unif, _gammas, schemes =
                Generalization.exit [ var ] env.unif
              in
              let env = { env with unif } in
              add_to_log env;

              assert (List.length schemes = 1);
              let scheme = List.hd schemes in

              Debug.print_header "DEBUG SCHEME"
              @@ Generalization.debug_scheme scheme;

              let schemes = Env.SMap.add sch_var scheme env.schemes in
              let env = { env with schemes } in

              solve_c2 env r1 k
            end
            | NErr e -> k (env, NErr e)
            | NDo p ->
              k
                ( env
                , ndo
                  @@
                  let+ c1 = p in
                  Let (sch_var, var, c1, c2) )
          in

          match Env.SMap.mem sch_var env.schemes with
          | false -> solve_c1 env k
          | true ->
            let r1 = match c1 with Ret r1 -> r1 | _ -> assert false in
            solve_c2 env r1 k
        end
    in

    add_to_log env;
    match eval env c0 k with
    | exception Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" @@ MenhirLib.LexerUtil.range loc;
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result -> result
end
