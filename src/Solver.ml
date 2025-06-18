(*
   As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor].
*)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module SatConstraint = SatConstraint.Make (T)
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

  let do_log env c k =
    PPrint.(nest 2
      (string "- " ^^
        ConstraintPrinter.print_constraint_in_context
          ~env:(Env.debug env)
          c k
    ))
    |> Utils.string_of_doc
    |> prerr_endline

  (** See [../README.md] ("High-level description") or [Solver.mli] for a
      description of normal constraints and our expectations regarding the
      [eval] function. *)
  type ('ok, 'err) normal_constraint =
    | NRet : 'a Constraint.on_sol -> ('a, 'e) normal_constraint
    | NErr : 'e -> ('a, 'e) normal_constraint
    | NDo :
        ('a, 'e) Constraint.t T.t * ('a, 'e, 'ok, 'err) Constraint.cont
        -> ('ok, 'err) normal_constraint

  let eval (type a1 e1 a e) ~log (env : env) (c0 : (a1, e1) Constraint.t)
    (k : (a1, e1, a, e) Constraint.cont) : env * (a, e) normal_constraint =
    (* We recommend calling the function [add_to_log] below
         whenever you get an updated environment.

         $ dune exec -- minihell --log-solver foo.test

         will show a log that will let you see the evolution
         of your input constraint (after simplification) as
         the solver progresses, which is useful for debugging.

         (You can also tweak this code temporarily to print stuff on
         stderr right away if you need dirtier ways to debug.)
    *)
    let add_to_log env c k =
      if log then do_log env c k
    in

    let exception Located of Utils.loc * exn * Printexc.raw_backtrace in
    let locate_exn loc exn =
      match exn with
      | Located (_, _, _) as exn -> raise exn
      | base_exn ->
        let bt = Printexc.get_raw_backtrace () in
        raise @@ Located (loc, base_exn, bt)
    in

    let rec eval : type a1 e1 a e.
         env
      -> (a1, e1) Constraint.t
      -> (a1, e1, a, e) Constraint.cont
      -> env * (a, e) normal_constraint =
     fun env c k ->
      match c with
      | Loc (loc, c) -> begin
        try eval env c k with exn -> locate_exn loc exn
      end
      | Ret v -> continue env (Ok v) k
      | Err e -> continue env (Error e) k
      | Map (c, f) -> eval env c (Next (KMap f, k))
      | MapErr (c, f) -> eval env c (Next (KMapErr f, k))
      | Conj (c, d) -> eval env c (Next (KConj1 d, k))
      | Eq (x1, x2) -> begin
        match Unif.unify env.unif x1 x2 with
        | Ok unif ->
          let env = { env with unif } in
          add_to_log env c k;

          continue env (Ok (fun _sol -> ())) k
        | Error (Cycle cy) -> continue env (Error (Constraint.Cycle cy)) k
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode env.unif () in
          continue env (Error (Constraint.Clash (decoder y1, decoder y2))) k
      end
      | Exist (x, s, c) ->
        let unif = Unif.Env.add_flexible x s env.unif in
        let env = { env with unif } in
        add_to_log env c k;

        eval env c (Next (KExist x, k))
      | Decode v -> continue env (Ok (fun sol -> sol v)) k
      | Do p -> (env, NDo (p, k))
      | DecodeScheme sch_var -> begin
        let scheme = Env.SMap.find sch_var env.schemes in

        let body sol = sol @@ Generalization.body scheme in
        let quantifiers (sol : Constraint.variable -> STLC.ty) :
          Structure.TyVar.t list =
          scheme |> Generalization.quantifiers
          |> List.map
               begin
                 fun var ->
                   let (Constr ty) = sol var in
                   match ty with Var v -> v | Arrow _ | Prod _ -> assert false
               end
        in

        continue env (Ok (fun sol -> (quantifiers sol, body sol))) k
      end
      | Instance (sch_var, w) -> begin
        let sch = Env.SMap.find sch_var env.schemes in

        let unif, result = Generalization.instantiate sch w env.unif in

        let env = { env with unif } in
        add_to_log env c k;

        let res =
          match result with
          | Ok witnesses -> Ok (fun sol -> List.map sol witnesses)
          | Error (Cycle cy) -> Error (Constraint.Cycle cy)
          | Error (Clash (y1, y2)) ->
            let decoder = Decode.decode env.unif () in
            Error (Constraint.Clash (decoder y1, decoder y2))
        in

        continue env res k
      end
      | Let (sch_var, var, c1, c2) ->
        let unif = Generalization.enter env.unif in
        let unif = Unif.Env.add_flexible var None unif in
        let env = { env with unif } in
        add_to_log env c k;

        eval env c1 (Next (KLet1 (sch_var, var, c2), k))
    and continue : type a1 e1 a e.
         env
      -> (a1 Constraint.on_sol, e1) result
      -> (a1, e1, a, e) Constraint.cont
      -> env * (a, e) normal_constraint =
     fun env res k ->
      match res with
      | Error e -> begin
        match k with
        | Done -> (env, NErr e)
        | Next (KMapErr f, k) -> continue env (Error (f e)) k
        | Next (KMap _, k) -> continue env (Error e) k
        | Next (KConj1 _, k) -> continue env (Error e) k
        | Next (KConj2 _, k) -> continue env (Error e) k
        | Next (KExist _, k) -> continue env (Error e) k
        | Next (KLet1 _, k) -> continue env (Error e) k
        | Next (KLet2 _, k) -> continue env (Error e) k
      end
      | Ok v -> begin
        match k with
        | Done -> (env, NRet v)
        | Next (KMap f, k) -> continue env (Ok (fun sol -> f @@ v sol)) k
        | Next (KConj1 c, k) -> eval env c (Next (KConj2 v, k))
        | Next (KConj2 w, k) -> continue env (Ok (fun sol -> (w sol, v sol))) k
        | Next (KExist _x, k) ->
          (* We could in theory remove [x] from the solver environment
             at this point, as it is not in scope for the rest of the
             constraint. But our notion of "solution" for the whole
             constraint expects a map with witnesses for all
             variables, even those that are bound locally with an
             existential. We must those keep these variables in the
             environment to be able to provide the solution at the
             end. *)
          continue env res k
        | Next (KLet1 (sch_var, var, c), k) as k0 ->
          let unif, _gammas, schemes = Generalization.exit [ var ] env.unif in

          let env = { env with unif } in
          add_to_log env
            (match res with Ok v -> Ret v | Error e -> Err e)
            k0;

          assert (List.length schemes = 1);
          let scheme = List.hd schemes in

          let schemes = Env.SMap.add sch_var scheme env.schemes in
          let env = { env with schemes } in

          eval env c (Next (KLet2 v, k))
        | Next (KLet2 w, k) -> continue env (Ok (fun sol -> (w sol, v sol))) k
        | Next (KMapErr _, k) -> continue env (Ok v) k
      end
    in

    add_to_log env c0 k;

    match eval env c0 k with
    | exception Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" @@ MenhirLib.LexerUtil.range loc;
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result -> result
end
