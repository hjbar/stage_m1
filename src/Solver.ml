(* As explained in the README.md ("Abstracting over an effect"),
   this module as well as other modules is parametrized over
   an arbitrary effect [T : Functor]. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module SatConstraint = SatConstraint.Make (T)
  module ConstraintPrinter = ConstraintPrinter.Make (T)

  module Env = struct
    module SMap = Constraint.SVar.Map

    type t = {
      unif : Unif.Env.t;
      schemes : Generalization.scheme SMap.t;
    }

    let empty () = { unif = Unif.Env.empty (); schemes = SMap.empty }

    let debug_schemes schemes =
      let open PPrint in
      schemes
      |> Constraint.SVar.Map.bindings
      |> List.map
           begin
             fun (s, sch) ->
               Constraint.SVar.print s
               ^^ colon
               ^^ space
               ^^ Generalization.debug_scheme sch
           end
      |> separate hardline


    let debug { unif; schemes } =
      let open PPrint in
      let has_no_schemes = SMap.is_empty schemes in
      let schemes_doc =
        if has_no_schemes then empty
        else
          group
            (string "Schemes :" ^^ nest 2 (hardline ^^ debug_schemes schemes))
      in

      let has_no_env = Unif.Env.map_is_empty unif in
      let env_doc =
        if has_no_env then empty
        else
          group (string "Env :" ^^ nest 2 (hardline ^^ Unif.Env.debug_env unif))
      in

      let pool_doc =
        if Unif.Env.pool_is_empty unif then empty
        else
          group
            (string "Pool :" ^^ nest 2 (hardline ^^ Unif.Env.debug_pool unif))
      in

      schemes_doc
      ^^ (if has_no_schemes then empty else hardline)
      ^^ env_doc
      ^^ (if has_no_env then empty else hardline)
      ^^ pool_doc
  end

  type env = Env.t

  type log = PPrint.document list

  let do_log ~dir (env : env) c k =
    let open PPrint in
    let env = Env.debug env in
    let dir =
      match dir with
      | `Init -> "--"
      | `Enter -> "->"
      | `Continue -> "<-"
    in

    nest 2
      begin
        string dir
        ^^ space
        ^^ ConstraintPrinter.print_constraint_in_context ~env c k
      end
    |> Utils.string_of_doc
    |> print_endline


  (** See [../README.md] ("High-level description") or [Solver.mli] for a
      description of normal constraints and our expectations regarding the
      [eval] function. *)
  type ('ok, 'err) normal_constraint =
    | NRet : env * 'a Constraint.on_sol -> ('a, 'e) normal_constraint
    | NErr : Utils.loc option * 'e -> ('a, 'e) normal_constraint
    | NDo : ('a, 'e) normal_constraint T.t -> ('a, 'e) normal_constraint

  let eval (type a e) ~log (env : env) (c0 : (a, e) Constraint.t) :
    (a, e) normal_constraint =
    (* We recommend calling the function [add_to_log] below
       whenever you get an updated environment.

       $ dune exec -- minihell --log-solver foo.test

       will show a log that will let you see the evolution
       of your input constraint as the solver progresses,
       which is useful for debugging.

       (You can also tweak this code temporarily to print stuff
       on stderr right away if you need dirtier ways to debug.)
    *)
    let add_to_log ~dir env c k =
      if log || Debug.debug then do_log ~dir env c k
    in
    let at_loc = Utils.at_loc in
    let result_to_constr = function
      | Ok v -> Constraint.Ret v
      | Error (_loco, e) -> Constraint.Err e
    in
    let decode_error env loco = function
      | Unif.Cycle cy -> (loco, Constraint.Cycle cy)
      | Clash (y1, y2) ->
        let decoder = Decode.decode env.Env.unif () in
        at_loc loco @@ fun () ->
        (loco, Constraint.Clash (decoder y1, decoder y2))
    in
    let rec eval : type a1 e1 a e.
      env ->
      Utils.loc option * (a1, e1) Constraint.t ->
      (a1, e1, a, e) Constraint.cont ->
      (a, e) normal_constraint =
     fun env (loco, c) k ->
      let dir = `Enter in

      match c with
      | Loc (loc, c) -> eval env (Some loc, c) k
      | Ret v -> continue env (Ok v) k
      | Err e -> continue env (Error (loco, e)) k
      | Map (c, f) -> eval env (loco, c) (Next ((loco, KMap f), k))
      | MapErr (c, f) -> eval env (loco, c) (Next ((loco, KMapErr f), k))
      | Conj (c, d) -> eval env (loco, c) (Next ((loco, KConj1 d), k))
      | Eq (x1, x2) -> begin
        match at_loc loco @@ fun () -> Unif.unify env.unif x1 x2 with
        | Error err -> continue env (Error (decode_error env loco err)) k
        | Ok unif ->
          let env = { env with unif } in
          add_to_log ~dir env c k;
          continue env (Ok (fun _sol -> ())) k
      end
      | Exist (x, s, c) ->
        let env = { env with unif = Unif.Env.add_flexible x s env.unif } in
        add_to_log ~dir env c k;
        eval env (loco, c) (Next ((loco, KExist x), k))
      | Decode v ->
        continue env (Ok (fun sol -> at_loc loco @@ fun () -> sol v)) k
      | Do p -> NDo (T.map (fun c -> eval env (loco, c) k) p)
      | DecodeScheme sch_var -> begin
        let scheme = Env.SMap.find sch_var env.schemes in

        let body sol = sol (Generalization.body scheme) in

        let quantifiers sol =
          scheme
          |> Generalization.quantifiers
          |> List.map
               begin
                 fun var ->
                   let (Typed.Constr ty) = sol var in
                   match ty with
                   | Var v -> v
                   | Arrow _ | Prod _ -> assert false
               end
        in

        continue env (Ok (fun sol -> (quantifiers sol, body sol))) k
      end
      | Instance (sch_var, w) -> begin
        let sch = Env.SMap.find sch_var env.schemes in

        let unif, result = Generalization.instantiate sch w env.unif in

        let env = { env with unif } in
        add_to_log ~dir env c k;

        let res =
          match result with
          | Error err -> Error (decode_error env loco err)
          | Ok witnesses -> Ok (fun sol -> List.map sol witnesses)
        in

        continue env res k
      end
      | Let (bindings, c1, c2) ->
        let env =
          List.fold_left
            begin
              fun (env : env) (_, var) ->
                let unif = Generalization.enter env.unif in
                let unif = Unif.Env.add_flexible var None unif in
                { env with unif }
            end
            env bindings
        in
        add_to_log ~dir env c k;

        eval env (loco, c1) (Next ((loco, KLet1 (bindings, c2)), k))
    and continue : type a1 e1 a e.
      env ->
      (a1 Constraint.on_sol, Utils.loc option * e1) result ->
      (a1, e1, a, e) Constraint.cont ->
      (a, e) normal_constraint =
     fun env res k ->
      let dir = `Continue in

      match res with
      | Error (loco, e) -> begin
        match k with
        | Done -> NErr (loco, e)
        | Next ((_, KMapErr f), k) -> continue env (Error (loco, f e)) k
        | Next ((_, KMap _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KConj1 _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KConj2 _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KExist _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KLet1 _), k) -> continue env (Error (loco, e)) k
        | Next ((_, KLet2 _), k) -> continue env (Error (loco, e)) k
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
        | Next ((loco, KLet1 (bindings, c)), k) as k0 ->
          let sch_vars, vars = List.split bindings in

          let unif, schemes = Generalization.exit vars env.unif in
          let env = { env with unif } in

          let schemes =
            List.fold_right2 Env.SMap.add sch_vars schemes env.schemes
          in
          let env = { env with schemes } in
          add_to_log ~dir env (result_to_constr res) k0;

          eval env (loco, c) (Next ((loco, KLet2 v), k))
        | Next ((loco, KLet2 w), k) ->
          continue env
            (Ok (fun sol -> at_loc loco @@ fun () -> (w sol, v sol)))
            k
        | Next ((_, KMapErr _), k) -> continue env (Ok v) k
      end
    in

    let dir = `Init in
    let k0 = Constraint.Done in
    add_to_log ~dir env c0 k0;

    match eval env (None, c0) k0 with
    | exception Utils.Located (loc, exn, bt) ->
      Printf.eprintf "Error at %s" (MenhirLib.LexerUtil.range loc);
      Printexc.raise_with_backtrace exn bt
    | exception exn -> raise exn
    | result -> result
end
