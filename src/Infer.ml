(** Infer contains the logic to generate an inference constraint from an untyped
    term, that will elaborate to an explicitly-typed term or fail with a type
    error. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  open Constraint
  module Untyped = Untyped.Make (T)

  (* Type definitions *)

  (** OLD : The "environment" of the constraint generator maps each program
      variable to an inference variable representing its (monomorphic) type.

      For example, to infer the type of the term [lambda x. t], we will
      eventually call [has_type env t] with an environment mapping [x] to a
      local inference variable representing its type.

      TODO : new doc *)
  module Env = Untyped.Var.Map

  type mono = variable Env.t

  type poly = scheme_variable Env.t

  type env = mono * poly

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  (* Helper functions *)

  let eq v1 v2 = Eq (v1, v2)

  let exist v s c = Exist (v, s, c)

  let let_ s v c1 c2 = Let (s, v, c1, c2)

  let decode v = MapErr (Decode v, fun e -> Cycle e)

  let decode_scheme s = MapErr (DecodeScheme s, fun e -> Cycle e)

  let fresh_name = Constraint.Var.fresh

  let fresh_var x = fresh_name @@ Untyped.Var.name x

  let fresh_scheme = Constraint.SVar.fresh

  let findopt_mono x (mono_env, _poly_env) = Env.find_opt x mono_env

  let add_mono x v (mono_env, poly_env) = (Env.add x v mono_env, poly_env)

  let find_poly x (_mono_env, poly_env) = Env.find x poly_env

  let add_poly x v (mono_env, poly_env) = (mono_env, Env.add x v poly_env)

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
      let w = fresh_name v.name in
      exist w (Some (Var v)) @@ k w
    | Arrow (ty1, ty2) ->
      let warr = fresh_name "arr" in

      bind ty1 @@ fun w1 ->
      bind ty2 @@ fun w2 -> exist warr (Some (Arrow (w1, w2))) @@ k warr
    | Prod tys ->
      let wprod = fresh_name "prod" in

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
          ( v
          , None
          , Map
              ( Conj
                  ( Exist (arr, Some (Arrow (v, v)), Eq (arr, w))
                  , MapErr (Decode v, fun e -> Cycle e) )
              , fun ((), ty) -> Abs (x, ty, Var x) ) )
      ]}
      but the actually generated constraint may be more complex/verbose.

      Precondition: when calling [has_type env t], [env] must map each term
      variable that is free in [t] to an inference variable. *)
  let rec has_type (env : env) (t : Untyped.term) (w : variable) :
    (STLC.term, err) t =
    match t with
    | Untyped.Loc (loc, t) -> Constraint.Loc (loc, has_type env t w)
    | Untyped.Var x -> begin
      (* OLD : [[x : w]] := (E(x) = w) TODO : new doc *)
      match findopt_mono x env with
      | Some wx ->
        let+ () = eq w wx in
        STLC.Var x
      | None ->
        let sch = find_poly x env in

        let+ tys = Instance (sch, w) in
        STLC.VarApp (x, tys)
    end
    | Untyped.App (t, u) ->
      (* [[t u : w]] := ∃wu. [[t : wu -> w]] ∧ [[u : wu]] *)
      let wu = fresh_name "wu" in
      let wt = fresh_name "wt" in

      exist wu None
      @@ exist wt (Some (Arrow (wu, w)))
      @@ let+ t' = has_type env t wt
         and+ u' = has_type env u wu in
         STLC.App (t', u')
    | Untyped.Abs (x, t) ->
      (* [[fun x -> t : w]] := ∃wx wt. w = wx -> wt ∧ [[t : wt]](E,x↦wx) *)
      let wx = fresh_var x in
      let wt = fresh_name "wt" in
      let warr = fresh_name "warr" in
      let env = add_mono x wx env in

      exist wx None @@ exist wt None
      @@ exist warr (Some (Arrow (wx, wt)))
      @@ let+ () = eq w warr
         and+ t' = has_type env t wt
         and+ tyx = decode wx in
         STLC.Abs (x, tyx, t')
    | Untyped.Let (x, t, u) ->
      (* OLD : [[let x = t in u : w]] := ∃wt. [[t : wt]] ∧ [[u : w]](E,x↦wt)
         TODO : new doc *)
      let wt = fresh_var x in
      let s = fresh_scheme "s" in

      let inner_env = add_poly x s env in

      let+ t', (u', scheme) =
        let_ s wt (has_type env t wt)
        @@ let+ u' = has_type inner_env u w
           and+ scheme = decode_scheme s in
           (u', scheme)
      in
      STLC.Let (x, scheme, t', u')
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
             w = (w₁ * w₂ * ... * wₙ)
      *)
      let rec loop i ts k =
        match ts with
        | [] -> k []
        | t :: ts ->
          let w = Printf.ksprintf fresh_name "w%d" (i + 1) in

          exist w None
          @@ let+ t' = has_type env t w
             and+ ts' = loop (i + 1) ts @@ fun ws -> k (w :: ws) in
             t' :: ts'
      in

      let+ ts =
        loop 0 ts @@ fun ws ->
        let wprod = fresh_name "wprod" in

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
          let wx = fresh_var x in
          exist wx None @@ loop xs @@ fun ws -> k (wx :: ws)
      in

      loop xs @@ fun ws ->
      let wt = fresh_name "wt" in

      exist wt (Some (Prod ws))
      @@
      let+ bindings =
        let rec loop = function
          | [] -> Ret []
          | (x, w) :: ws ->
            let+ bindings = loop ws
            and+ ty = decode w in
            (x, ty) :: bindings
        in

        loop @@ List.combine xs ws
      and+ t' = has_type env t wt
      and+ u' =
        let env = List.fold_right2 add_mono xs ws env in
        has_type env u w
      in

      STLC.LetTuple (bindings, t', u')
    | Do p -> Do (T.map (fun t -> has_type env t w) p)
end
