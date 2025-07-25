(** Infer contains the logic to generate an inference constraint from an untyped
    term, that will elaborate to an explicitly-typed term or fail with a type
    error. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module Untyped = Untyped.Make (T)
  open Constraint

  (* Type definitions *)

  (** The "environment" of the constraint generator maps each program variable
      to either an inference variable representing its (monomorphic) type or an
      inference scheme variable representing its (polymorphic) scheme.

      For example:
      - To infer the type of the term [lambda x. t], we will eventually call
        [has_type env t] with an environment mapping [x] to a local inference
        variable representing its type.
      - To infer the type of the term [let x = t in u], we will eventually call
        [has_type env t] and [has_type env u] with an environment mapping [x] to
        a local inference scheme variable representing its scheme. *)
  module Env = struct
    module Map = Untyped.Var.Map

    type t = {
      variables : variable Map.t;
      schemes : scheme_variable Map.t;
    }

    let empty = { variables = Map.empty; schemes = Map.empty }

    let findopt_variable x env = Map.find_opt x env.variables

    let find_scheme x env = Map.find x env.schemes

    let add_variable x v env =
      let variables = Map.add x v env.variables in
      { env with variables }


    let add_scheme x s env =
      let schemes = Map.add x s env.schemes in
      { env with schemes }
  end

  type env = Env.t

  type err = eq_error =
    | Clash of Typed.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  (* Helper functions for constraint constructors *)

  let eq v1 v2 = Eq (v1, v2)

  let exist v s c = Exist (v, s, c)

  let let_ bindings c1 c2 = Let (bindings, c1, c2)

  let decode v = MapErr (Decode v, fun e -> Cycle e)

  let decode_scheme s = MapErr (DecodeScheme s, fun e -> Cycle e)

  (* Helper functions for generating fresh variable names *)

  let fresh_var = Constraint.Var.fresh

  let fresh_var_name x = Constraint.Var.fresh (Untyped.Var.name x)

  let fresh_scheme_name x =
    Constraint.SVar.fresh ("scheme_" ^ Untyped.Var.name x)


  (** This is a helper function to implement constraint generation for the
      [Annot] construct.

      [bind ty k] takes a type [ty], and a constraint [k] parametrized over a
      constraint variable. It creates a constraint context that binds a new
      constraint variable [?w] that must be equal to [ty], and places [k ?w]
      within this context.

      For example, if [ty] is the type [?v1 -> (?v2 -> ?v3)] , then [bind ty k]
      could be the constraint [∃(?w1 = ?v2 -> ?v3). ∃(?w2 = ?v1 -> ?w1). k ?w2],
      or equivalently [∃?w3 ?w4. ?w3 = ?v1 -> ?w4 ∧ ?w4 = ?v2 -> ?v3 ∧ k ?w3].
  *)
  let rec bind (Constr ty : Typed.ty) (k : Constraint.variable -> ('a, 'e) t) :
    ('a, 'e) t =
    match ty with
    | Var v ->
      let w = fresh_var v.name in
      exist w (Some (Var v)) @@ k w
    | Arrow (ty1, ty2) ->
      let warr = fresh_var "arr" in

      bind ty1 @@ fun w1 ->
      bind ty2 @@ fun w2 -> exist warr (Some (Arrow (w1, w2))) @@ k warr
    | Prod tys ->
      let wprod = fresh_var "prod" in

      let rec loop tys k =
        match tys with
        | [] -> k []
        | ty :: tys ->
          bind ty @@ fun w ->
          loop tys @@ fun ws -> k (w :: ws)
      in

      loop tys @@ fun ws -> exist wprod (Some (Prod ws)) @@ k wprod


  (** This function generates a typing constraint from an untyped term:
      [has_type env t w] generates a constraint [C] which contains [w] as a free
      inference variable, such that [C] has a solution if and only if [t] is
      well-typed in [env], and in that case [w] is the type of [t].

      For example, if [t] is the term [lambda x. x], then [has_type env t w]
      generates a constraint equivalent to [∃?v. ?w = (?v -> ?v)].

      More precisely, one possible generated constraint would be:
      {[
        Exist
          ( v,
            None,
            Map
              ( Conj
                  ( Exist (arr, Some (Arrow (v, v)), Eq (arr, w)),
                    MapErr (Decode v, fun e -> Cycle e) ),
                fun ((), ty) -> Abs (x, ty, Var x) ) )
      ]}
      but the actually generated constraint may be more complex/verbose.

      Precondition: when calling [has_type env t], [env] must map every term
      variable free in [t] to either an inference variable or a scheme variable.
  *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) :
    (Typed.term, err) t =
    match t with
    | Untyped.Loc (loc, t) -> Constraint.Loc (loc, has_type env t w)
    | Untyped.Var x -> begin
      (* [[x : w]] := (E(x) = w) if w is in the environment's variables
         [[x : w]] := (E(x) = Instance(s, w)) where s is the scheme of x *)
      match Env.findopt_variable x env with
      | Some wx ->
        let+ () = eq w wx in
        Typed.Var x
      | None ->
        let sch = Env.find_scheme x env in

        let+ tys = Instance (sch, w) in
        Typed.TyApp (Typed.Var x, tys)
    end
    | Untyped.App (t, u) ->
      (* [[t u : w]] := ∃wu. [[t : wu -> w]] ∧ [[u : wu]] *)
      let wu = fresh_var "wu" in
      let wt = fresh_var "wt" in

      exist wu None
      @@ exist wt (Some (Arrow (wu, w)))
      @@ let+ t' = has_type env t wt
         and+ u' = has_type env u wu in
         Typed.App (t', u')
    | Untyped.Abs (x, t) ->
      (* [[fun x -> t : w]] := ∃wx wt. w = wx -> wt ∧ [[t : wt]](E,x ↦ wx) *)
      let wx = fresh_var_name x in
      let wt = fresh_var "wt" in
      let warr = fresh_var "warr" in

      let env = Env.add_variable x wx env in

      exist wx None
      @@ exist wt None
      @@ exist warr (Some (Arrow (wx, wt)))
      @@ let+ () = eq w warr
         and+ t' = has_type env t wt
         and+ tyx = decode wx in
         Typed.Abs (x, tyx, t')
    | Untyped.Let (x, t, u) ->
      (* [[let x = t in u : w]] := Let ([(x, s)], [[t : wt]], [[u : w]](E,x ↦ s))
         where wt is a fresh variable and s is a fresh scheme variable *)
      let wt = fresh_var_name x in
      let s = fresh_scheme_name x in

      let inner_env = Env.add_scheme x s env in

      let+ t', (u', scheme) =
        let_ [ (s, wt) ] (has_type env t wt)
        @@ let+ u' = has_type inner_env u w
           and+ scheme = decode_scheme s in
           (u', scheme)
      in

      Typed.Let (x, scheme, TyAbs (fst scheme, t'), u')
    | Untyped.Annot (t, ty) ->
      (* [[(t : ty) : w]] := ∃(wt = ty). [[t : wt]] /\ [[wt = w]] *)
      bind ty @@ fun wt ->
      let+ () = eq wt w
      and+ t' = has_type env t wt in
      Typed.Annot (t', ty)
    | Untyped.Tuple ts ->
      (* [[(t₁, t₂ ... tₙ) : w]] :=
         ∃w₁.
         [[t₁ : w₁]] /\
          ∃w₂.
          [[t₂ : w₂]] /\
           ...
            ∃wₙ.
            [[tₙ : wₙ]] /\
             w = (w₁ * w₂ * ... * wₙ) *)
      let rec loop i ts k =
        match ts with
        | [] -> k []
        | t :: ts ->
          let w = Printf.ksprintf fresh_var "w%d" (i + 1) in

          exist w None
          @@ let+ t' = has_type env t w
             and+ ts' = loop (i + 1) ts @@ fun ws -> k (w :: ws) in
             t' :: ts'
      in

      let+ ts =
        loop 0 ts @@ fun ws ->
        let wprod = fresh_var "wprod" in

        exist wprod (Some (Prod ws))
        @@ let+ () = eq w wprod in
           []
      in

      Typed.Tuple ts
    | Untyped.LetTuple (xs, t, u) ->
      (* [[let (x₁, ..., xₙ) = t in u : w]] :=
          Let ( [(s₁, w₁); ...; (sₙ, wₙ)],
          ( [[t : wt]] ∧ wt = (w₁ * ... * wₙ) ),
          [[u : w]](E[x₁ ↦ s₁; ...; xₙ ↦ sₙ]) ) *)
      let wts = List.map fresh_var_name xs in
      let sch_vars = List.map fresh_scheme_name xs in
      let wt = fresh_var "wt" in

      let inner_env = List.fold_right2 Env.add_scheme xs sch_vars env in

      let+ t', (u', bindings) =
        let_
          (List.combine sch_vars wts)
          (exist wt (Some (Prod wts)) @@ has_type env t wt)
        @@
        let+ u' = has_type inner_env u w
        and+ bindings =
          List.fold_left2
            begin
              fun acc x s ->
                let+ s = decode_scheme s
                and+ acc = acc in
                (x, s) :: acc
            end
            (Ret (fun _sol -> []))
            xs sch_vars
        in
        (u', List.rev bindings)
      in

      Typed.LetTuple (bindings, t', u')
    | Do p -> Do (T.map (fun t -> has_type env t w) p)


  (** Transform a given program we want to type into a constraint, using a
      let-constraint as a wrapper *)
  let let_wrapper (term : Untyped.term) :
    (Typed.term * Typed.scheme, err) Constraint.t =
    let s = Constraint.SVar.fresh "final_scheme" in
    let w = Constraint.Var.fresh "final_term" in
    Let ([ (s, w) ], has_type Env.empty term w, decode_scheme s)
end
