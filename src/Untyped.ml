(** Our syntax of untyped terms.

    As explained in the README.md ("Abstracting over an effect"), this module as
    well as other modules is parametrized over an arbitrary effect
    [T : Functor]. *)

module Make (T : Utils.Functor) = struct
  module Var = Typed.TeVar

  (** ['t term_] is parametrized over the representation of term variables. Most
      of the project code will work with the non-parametrized instance [term]
      below. *)
  type 't term_ =
    | Var of 'tev
    | App of 't term_ * 't term_
    | Abs of 'tev * 't term_
    | Let of 'tev * 't term_ * 't term_
    | Tuple of 't term_ list
    | LetTuple of 'tev list * 't term_ * 't term_
    | Annot of 't term_ * 'tyv Typed.ty_
    | Do of 't term_ T.t
    | Loc of Utils.loc * 't term_
    constraint 't = < tevar : 'tev ; tyvar : 'tyv >

  (** [raw_term] are terms with raw [string] for their variables. Several
      binders may use the same variable. These terms are produced by the parser.
  *)
  type raw_term = < tevar : string ; tyvar : string > term_

  (** [term] are terms using [Typed.TeVar.t] variables, which include a unique
      stamp to guarantee uniqueness of binders. This is what most of the code
      manipulates. *)
  type term = < tevar : Var.t ; tyvar : Structure.TyVar.t > term_

  let freshen : raw_term -> term =
    let module Env = Map.Make (String) in
    let bind env x =
      let x_var = Var.fresh x in
      let env = Env.add x x_var env in
      (env, x_var)
    in
    let rec freshen env loco =
      let freshen env ?(loco = loco) t = freshen env loco t in
      function
      | Var x -> begin
        match Env.find_opt x env with
        | None ->
          Utils.print_loco loco;
          Printf.ksprintf invalid_arg
            "Constraint variable '%s' is unbound at this point" x
        | Some v -> Var v
      end
      | App (t1, t2) -> App (freshen env t1, freshen env t2)
      | Abs (x, t) ->
        let env, x = bind env x in
        Abs (x, freshen env t)
      | Let (x, t1, t2) ->
        let env_inner, x = bind env x in
        Let (x, freshen env t1, freshen env_inner t2)
      | Tuple ts -> Tuple (List.map (freshen env) ts)
      | LetTuple (xs, t1, t2) ->
        let env_inner, xs = List.fold_left_map bind env xs in
        LetTuple (xs, freshen env t1, freshen env_inner t2)
      | Annot (t, ty) -> Annot (freshen env t, Typed.freshen_ty ty)
      | Do p -> Do (T.map (freshen env) p)
      | Loc (loc, t) -> Loc (loc, freshen env ~loco:(Some loc) t)
    in
    fun t -> freshen Env.empty None t
end
