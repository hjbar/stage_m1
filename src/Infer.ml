(** Infer contains the logic to generate an inference constraint from an untyped
    term, that will elaborate to an explicitly-typed term or fail with a type
    error. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  module Untyped = Untyped.Make (T)
  open Constraint

  (* Type definitions *)

  (** The "environment" of the constraint generator maps each program variable
      to an inference variable representing its (monomorphic) type.

      For example, to infer the type of the term [lambda x. t], we will
      eventually call [has_type env t] with an environment mapping [x] to a
      local inference variable representing its type. *)
  module Env = struct
    module Map = Untyped.Var.Map

    type t = variable Map.t

    let empty = Map.empty

    let find_variable x env = Map.find x env

    let add_variable x v env = Map.add x v env
  end

  type env = Env.t

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  (* Helper functions for constraint constructors *)

  let eq v1 v2 = Eq (v1, v2)

  let exist v s c = Exist (v, s, c)

  let decode v = MapErr (Decode v, fun e -> Cycle e)

  (* Helper functions for generating fresh variable names *)

  let fresh_var = Constraint.Var.fresh

  let fresh_var_name x = Constraint.Var.fresh (Untyped.Var.name x)

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
  let rec bind (Constr ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) :
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

      Precondition: when calling [has_type env t], [env] must map each term
      variable that is free in [t] to an inference variable. *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) :
    (STLC.term, err) t =
    match t with
    | Untyped.Loc (loc, t) -> Constraint.Loc (loc, has_type env t w)
    | Untyped.Var x ->
      (* [[x : w]] := (E(x) = w) *)
      let+ () = eq w (Env.find_variable x env) in
      STLC.Var x
    | Untyped.App (t, u) ->
      (* [[t u : w]] := ∃wu. [[t : wu -> w]] ∧ [[u : wu]] *)
      let wu = fresh_var "wu" in
      let wt = fresh_var "wt" in

      exist wu None
      @@ exist wt (Some (Arrow (wu, w)))
      @@ let+ t' = has_type env t wt
         and+ u' = has_type env u wu in
         STLC.App (t', u')
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
         STLC.Abs (x, tyx, t')
    | Untyped.Let (x, t, u) ->
      (* [[let x = t in u : w]] := ∃wt. [[t : wt]] ∧ [[u : w]](E,x↦wt) *)
      let wt = Constraint.Var.fresh (Untyped.Var.name x) in

      exist wt None
      @@ let+ t' = has_type env t wt
         and+ tyx = decode wt
         and+ u' = has_type (Env.add_variable x wt env) u w in
         STLC.Let (x, tyx, t', u')
    | Untyped.Annot (t, ty) ->
      (* [[(t : ty) : w]] := ∃(wt = ty). [[t : wt]] /\ [[wt = w]] *)
      bind ty @@ fun wt ->
      let+ () = eq wt w
      and+ t' = has_type env t wt in
      STLC.Annot (t', ty)
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

      STLC.Tuple ts
    | Untyped.LetTuple (xs, t, u) ->
      (* [[let (x₁, x₂ ... xₙ) = t in u : w]] :=
         ∃w₁, w₂... wₙ.
         ∃(wt = (w₁ * w₂ ... wₙ)).
         [[t : wt]](E)
         [[u : w]](E, x₁↦x₁, x₂↦w₂... xₙ↦wₙ) *)
      let rec loop xs k =
        match xs with
        | [] -> k []
        | x :: xs ->
          let wx = fresh_var_name x in
          exist wx None @@ loop xs @@ fun ws -> k (wx :: ws)
      in

      loop xs @@ fun ws ->
      let wt = fresh_var "wt" in

      exist wt (Some (Prod ws))
      @@
      let+ bindings =
        let rec loop = function
          | [] -> Ret (fun _sol -> [])
          | (x, w) :: ws ->
            let+ bindings = loop ws
            and+ ty = decode w in
            (x, ty) :: bindings
        in

        loop (List.combine xs ws)
      and+ t' = has_type env t wt
      and+ u' =
        let env = List.fold_right2 Env.add_variable xs ws env in
        has_type env u w
      in

      STLC.LetTuple (bindings, t', u')
    | Do p -> Do (T.map (fun t -> has_type env t w) p)


  (** Transform a given program we want to type into a constraint, using a
      exist-constraint as a wrapper *)
  let exist_wrapper (term : Untyped.term) :
    (STLC.term * STLC.ty, err) Constraint.t =
    let open Constraint in
    let w = Var.fresh "final_type" in
    Exist (w, None, Conj (has_type Untyped.Var.Map.empty term w, decode w))
end
