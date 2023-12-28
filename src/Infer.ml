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
                "Error: this implementation currently only supports pairs, not tuples of size %d."
                (List.length other)

    let fresh : string -> Constraint.variable =
        Constraint.Var.fresh

    let uname : Untyped.Var.t -> string =
        Untyped.Var.name

    (** Shorthand to introduce an existential variable to a constraint. *)
    let ( let/? ) ((ident, structure) : string * structure option) (term : Constraint.variable -> ('a, 'b) t) =
        let ident = fresh ident in
        Exist (ident, structure, term ident)

    (** This is a helper binder to implement constraint generation for
        the [Annot] construct.

        [let*! ty_var = ty in k] takes a type [ty], and a constraint [k] parametrized
        over a constraint variable. It creates a constraint context that
        binds a new constraint variable [?w] that must be equal to [ty],
        and places [k ?w] within this context.

        For example, if [ty] is the type [?v1 -> (?v2 -> ?v3)] , then
        [let*! ty_var = ty in k] could be the constraint
            [∃(?w1 = ?v2 -> ?v3). ∃(?w2 = ?v1 -> ?w1). k ?w2], or equivalently
            [∃?w3 ?w4. ?w3 = ?v1 -> ?w4 ∧ ?w4 = ?v2 -> ?v3 ∧ k ?w3].
    *)
    let rec ( let-: ) (ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) : ('a, 'e) t =
        match ty with
        | Constr (Var x) -> (
            let/? x_var = (Structure.TyVar.name x, Some (Var x)) in
            k x_var
        )
        | Constr (Arrow (x, y)) -> (
            let-: x in
            let-: y in
            let/? xy_var = ("xy", Some (Arrow (x, y))) in
            k xy_var
        )
        | Constr (Prod tup) -> (
            let (x, y) = assume_pair tup in
            let-: x in
            let-: y in
            let/? xy_var = ("xy", Some (Prod [x; y])) in
            k xy_var
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
        | Untyped.Var x -> (
            let x_inference = STLC.TeVar.Map.find x env in
            let+ () = eq w x_inference
            in STLC.Var x
        )
        | Untyped.App (t, u) -> (
            let/? u_var = ("wu", None) in
            let/? t_var = ("wt", Some (Arrow (u_var, w))) in
            let+ t_typed = has_type env t t_var
            and+ u_typed = has_type env u u_var
            in STLC.(App (t_typed, u_typed))
        )
        | Untyped.Abs (x, t) -> (
            let/? x_var = (uname x, None) in
            let/? res_var = ("wt", None) in
            let/? t_var = ("warr", Some (Arrow (x_var, res_var))) in
            let env' = STLC.TeVar.Map.(env |> add x x_var) in
            let+ () = eq w t_var
            and+ in_typ = decode x_var
            and+ t_typed = has_type env' t res_var
            in STLC.(Abs (x, in_typ, t_typed))
        )
        | Untyped.Let (x, t, u) -> (
            let/? t_var = (uname x, None) in
            let env' = STLC.TeVar.Map.(env |> add x t_var) in
            let+ let_typ = decode t_var
            and+ t_typed = has_type env t t_var
            and+ u_typed = has_type env' u w
            in STLC.(Let (x, let_typ, t_typed, u_typed))
        )
        | Untyped.Annot (t, ty) -> (
            let-: ty in
            let+ t_typed = has_type env t ty
            and+ () = eq ty w
            in t_typed
        )
        | Untyped.Tuple ts -> (
            let (t1,  t2) = assume_pair ts in
            let/? t1_var = ("w1", None) in
            let/? t2_var = ("w2", None) in
            let/? prod_var = ("prod", Some (Prod [t1_var; t2_var])) in
            let+ t1_typed = has_type env t1 t1_var
            and+ t2_typed = has_type env t2 t2_var
            and+ () = eq w prod_var
            in STLC.(Tuple [t1_typed; t2_typed])
        )
        | Untyped.LetTuple (xs, t, u) -> (
            let (x1, x2) = assume_pair xs in
            let/? x1_var = (uname x1, None) in
            let/? x2_var = (uname x2, None) in
            let/? prod_var = ("wt", Some (Prod [x1_var; x2_var])) in
            let env' = STLC.TeVar.Map.(env |> add x1 x1_var |> add x2 x2_var) in
            let+ x1_typ = decode x1_var
            and+ x2_typ = decode x2_var
            and+ t_typed = has_type env t prod_var
            and+ u_typed = has_type env' u w
            in STLC.LetTuple ([(x1, x1_typ); (x2, x2_typ)], t_typed, u_typed)
        )
        | Do p -> (
          (* Feel free to postone this until you start looking
             at random generation. Getting type inference to
             work on all the other cases is a good first step. *)
            Utils.not_yet "Infer.has_type: Do case" (env, p, fun () -> has_type)
        )
end
