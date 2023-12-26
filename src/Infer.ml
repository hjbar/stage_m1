(** Infer contains the logic to generate an inference constraint from
    an untyped term, that will elaborate to an explicitly-typed term
    or fail with a type error. *)

(* You have to implement the [has_type] function below,
   which is the constraint-generation function. *)

module Make(T : Utils.Functor) = struct
  module Constraint = Constraint.Make(T)
  open Constraint
  module Untyped = Untyped.Make(T)

  (** The "environment" of the constraint generator maps each program
      variable to an inference variable representing its (monomorphic)
      type.

      For example, to infer the type of the term [lambda x. t], we
      will eventually call [has_type env t] with an environment
      mapping [x] to a local inference variable representing its type.
  *)
  module Env = Untyped.Var.Map
  type env = variable Env.t

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  let eq v1 v2 = Eq(v1, v2)
  let decode v = MapErr(Decode v, fun e -> Cycle e)

  let assume_pair = function
    | [v1; v2] -> (v1, v2)
    | other ->
      Printf.ksprintf failwith
        "Error: this implementation currently only supports pairs,
         not tuples of size %d."
        (List.length other)

  (** This is a helper function to implement constraint generation for
      the [Annot] construct.
     
      [bind ty k] takes a type [ty], and a constraint [k] parametrized
      over a constraint variable. It creates a constraint context that
      binds a new constraint variable [?w] that must be equal to [ty],
      and places [k ?w] within this context.
      
      For example, if [ty] is the type [?v1 -> (?v2 -> ?v3)] , then
      [bind ty k] could be the constraint
        [∃(?w1 = ?v2 -> ?v3). ∃(?w2 = ?v1 -> ?w1). k ?w2], or equivalently
        [∃?w3 ?w4. ?w3 = ?v1 -> ?w4 ∧ ?w4 = ?v2 -> ?v3 ∧ k ?w3].
  *)
  let rec bind (ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) : ('a, 'e) t =
        match ty with
        | Constr (Var x) ->
                let x_var = Constraint.Var.fresh (Structure.TyVar.name x) in
                Exist (x_var, Some (Var x),
                    k x_var
                )
        | Constr (Arrow (x, y)) ->
                let xy_var = Constraint.Var.fresh "xy" in
                bind x (fun x_var ->
                    bind y (fun y_var ->
                        Exist (xy_var, Some (Arrow (x_var, y_var)),
                            k xy_var
                        )
                    )
                )
        | Constr (Prod tup) ->
                let (x, y) = assume_pair tup in
                let xy_var = Constraint.Var.fresh "xy" in
                bind x (fun x_var ->
                    bind y (fun y_var ->
                        Exist (xy_var, Some (Prod [x_var; y_var]),
                            k xy_var
                        )
                    )
                )

  (** This function generates a typing constraint from an untyped term:
      [has_type env t w] generates a constraint [C] which contains [w] as
      a free inference variable, such that [C] has a solution if and only
      if [t] is well-typed in [env], and in that case [w] is the type of [t].

      For example, if [t] is the term [lambda x. x], then [has_type env t w]
      generates a constraint equivalent to [∃?v. ?w = (?v -> ?v)].

      Precondition: when calling [has_type env t], [env] must map each
      term variable that is free in [t] to an inference variable.
  *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) : (STLC.term, err) t =
    match t with
    | Untyped.Var x ->
            let x_inference = STLC.TeVar.Map.find x env in
            let+ () = eq w x_inference
            in STLC.Var x
    | Untyped.App (t, u) ->
            let t_var = Constraint.Var.fresh "wt"
            and u_var = Constraint.Var.fresh "wu" in
            Exist (u_var, None,
                Exist (t_var, Some (Arrow (u_var, w)),
                    let+ t_typed = has_type env t t_var
                    and+ u_typed = has_type env u u_var
                    in STLC.(App (t_typed, u_typed))
                )
            )
    | Untyped.Abs (x, t) ->
            let x_var = Constraint.Var.fresh (Untyped.Var.name x)
            and t_var = Constraint.Var.fresh "warr"
            and res_var = Constraint.Var.fresh "wt"
            in
            Exist (x_var, None,
                Exist (res_var, None,
                    Exist (t_var, Some (Arrow (x_var, res_var)),
                        let env' = STLC.TeVar.Map.(env |> add x x_var) in
                        let+ () = eq w t_var
                        and+ in_typ = decode x_var
                        and+ t_typed = has_type env' t res_var
                        in STLC.(Abs (x, in_typ, t_typed))
                    )
                )
            )
    | Untyped.Let (x, t, u) ->
            let t_var = Constraint.Var.fresh (Untyped.Var.name x)
            and u_var = Constraint.Var.fresh "u" in
            Exist (t_var, None,
                Exist (u_var, None,
                    let env' = STLC.TeVar.Map.(env |> add x t_var) in
                    let+ let_typ = decode t_var
                    and+ t_typed = has_type env t t_var
                    and+ u_typed = has_type env' u u_var
                    in STLC.(Let (x, let_typ, t_typed, u_typed))
                )
            )
    | Untyped.Annot (t, ty) ->
            bind ty (fun ty_var ->
                let+ t_typed = has_type env t ty_var
                and+ () = eq ty_var w
                in t_typed
            )
    | Untyped.Tuple ts ->
        let (t1, t2) = assume_pair ts
        and t1_var = Constraint.Var.fresh "w1"
        and t2_var = Constraint.Var.fresh "w2"
        and prod_var = Constraint.Var.fresh "prod"
        in
        Exist (t1_var, None,
            Exist (t2_var, None,
                Exist (prod_var, Some (Prod [t1_var; t2_var]),
                    let+ t1_typed = has_type env t1 t1_var
                    and+ t2_typed = has_type env t2 t2_var
                    and+ () = eq w prod_var
                    in STLC.(Tuple [t1_typed; t2_typed])
                )
            )
        )
    | Untyped.LetTuple (xs, t, u) ->
        let (x1, x2) = assume_pair xs in
        let x1_var = Constraint.Var.fresh (Untyped.Var.name x1)
        and x2_var = Constraint.Var.fresh (Untyped.Var.name x2)
        and prod_var = Constraint.Var.fresh "wt"
        in
        Exist (x1_var, None,
            Exist (x2_var, None,
                Exist (prod_var, Some (Prod [x1_var; x2_var]),
                    let env' = STLC.TeVar.Map.(env |> add x1 x1_var |> add x2 x2_var) in
                    let+ x1_typ = decode x1_var
                    and+ x2_typ = decode x2_var
                    and+ t_typed = has_type env t prod_var
                    and+ u_typed = has_type env' u w
                    in STLC.LetTuple ([(x1, x1_typ); (x2, x2_typ)], t_typed, u_typed)
                )
            )
        )
    | Do p ->
      (* Feel free to postone this until you start looking
         at random generation. Getting type inference to
         work on all the other cases is a good first step. *)
      Utils.not_yet "Infer.has_type: Do case" (env, p, fun () -> has_type)
end
