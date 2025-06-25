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

  type env = Unif.Env.t

  type log = PPrint.document list

  let do_log c0 =
    let c0_erased = SatConstraint.erase c0 in
    fun env ->
      c0_erased
      |> ConstraintSimplifier.simplify env
      |> ConstraintPrinter.print_sat_constraint
      |> Utils.string_of_doc
      |> prerr_endline


  (** See [../README.md] ("High-level description") or [Solver.mli] for a
      description of normal constraints and our expectations regarding the
      [eval] function. *)
  type ('ok, 'err) normal_constraint =
    | NRet : env * 'a Constraint.on_sol -> ('a, 'e) normal_constraint
    | NErr : Utils.loc option * 'e -> ('a, 'e) normal_constraint
    | NDo : ('a, 'e) normal_constraint T.t -> ('a, 'e) normal_constraint

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t) :
    (a, e) normal_constraint =
    let add_to_log = if log || Debug.debug then do_log c0 else ignore in

    let exception Located of Utils.loc * exn * Printexc.raw_backtrace in
    let locate_exn loc exn =
      match exn with
      | Located (_, _, _) as exn -> raise exn
      | base_exn ->
        let bt = Printexc.get_raw_backtrace () in
        raise @@ Located (loc, base_exn, bt)
    in
    let at_loc loco f =
      match loco with
      | None -> f ()
      | Some loc -> ( try f () with exn -> locate_exn loc exn )
    in
    let rec eval : type a1 e1 a e.
      env ->
      Utils.loc option * (a1, e1) Constraint.t ->
      (a1, e1, a, e) Constraint.cont ->
      (a, e) normal_constraint =
     fun env (loco, c) k ->
      match c with
      | Loc (loc, c) -> eval env (Some loc, c) k
      | Ret v -> continue env (Ok v) k
      | Err e -> continue env (Error (loco, e)) k
      | Map (c, f) -> eval env (loco, c) (Next ((loco, KMap f), k))
      | MapErr (c, f) -> eval env (loco, c) (Next ((loco, KMapErr f), k))
      | Conj (c, d) -> eval env (loco, c) (Next ((loco, KConj1 d), k))
      | Eq (x1, x2) -> begin
        match at_loc loco @@ fun () -> Unif.unify env x1 x2 with
        | Ok env ->
          add_to_log env;
          continue env (Ok (fun _sol -> ())) k
        | Error (Cycle cy) -> continue env (Error (loco, Constraint.Cycle cy)) k
        | Error (Clash (y1, y2)) ->
          let decoder = Decode.decode env () in
          at_loc loco @@ fun () ->
          continue env
            (Error (loco, Constraint.Clash (decoder y1, decoder y2)))
            k
      end
      | Exist (x, s, c) ->
        let env = Unif.Env.add x s env in
        add_to_log env;
        eval env (loco, c) (Next ((loco, KExist x), k))
      | Decode v ->
        continue env (Ok (fun sol -> at_loc loco @@ fun () -> sol v)) k
      | Do p -> NDo (T.map (fun c -> eval env (loco, c) k) p)
    and continue : type a1 e1 a e.
      env ->
      (a1 Constraint.on_sol, Utils.loc option * e1) result ->
      (a1, e1, a, e) Constraint.cont ->
      (a, e) normal_constraint =
     fun env res k ->
      match res with
      | Error (loco, e) -> begin
        match k with
        | Done -> NErr (loco, e)
        | Next ((_, KMapErr f), k) -> continue env (Error (loco, f e)) k
        | Next ((_, KMap _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KConj1 _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KConj2 _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KExist _), k) -> continue env (Error (loco, e)) k
      end
      | Ok v -> begin
        match k with
        | Done -> NRet (env, v)
        | Next ((loco, KMap f), k) ->
          continue env (Ok (fun sol -> at_loc loco @@ fun () -> f (v sol))) k
        | Next ((loco, KConj1 c), k) ->
          eval env (loco, c) (Next ((loco, KConj2 v), k))
        | Next ((loco, KConj2 w), k) ->
          continue env
            (Ok (fun sol -> at_loc loco @@ fun () -> (w sol, v sol)))
            k
        | Next ((_, KExist _x), k) ->
          (* We could in theory remove [x] from the solver environment
             at this point, as it is not in scope for the rest of the
             constraint. But our notion of "solution" for the whole
             constraint expects a map with witnesses for all variables,
             even those that are bound locally with an existential.
             We must those keep these variables in the environment
             to be able to provide the solution at the end. *)
          continue env res k
        | Next ((_, KMapErr _), k) -> continue env (Ok v) k
      end
    in

    let k0 = Constraint.Done in
    add_to_log env;

    match eval env (None, c0) k0 with
    | exception Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" (MenhirLib.LexerUtil.range loc);
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result -> result
end
