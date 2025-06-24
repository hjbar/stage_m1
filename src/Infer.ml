(** Infer contains the logic to generate an inference constraint from an untyped
    term, that will elaborate to an explicitly-typed term or fail with a type
    error. *)

module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  open Constraint
  module Untyped = Untyped.Make (T)

  (** The "environment" of the constraint generator maps each program variable
      to an inference variable representing its (monomorphic) type.

      For example, to infer the type of the term [lambda x. t], we will
      eventually call [has_type env t] with an environment mapping [x] to a
      local inference variable representing its type. *)
  module Env = Untyped.Var.Map

  type env = variable Env.t

  type err = eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  let eq v1 v2 = Eq (v1, v2)

  let decode v = MapErr (Decode v, fun e -> Cycle e)

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
  let rec bind (ty : STLC.ty) (k : Constraint.variable -> ('a, 'e) t) :
    ('a, 'e) t =
    match ty with
    | Constr s -> (
      match s with
      | Var v ->
        let w = Constraint.Var.fresh v.name in
        Exist (w, Some (Var v), k w)
      | Arrow (ty1, ty2) ->
        let warr = Constraint.Var.fresh "arr" in
        bind ty1 @@ fun w1 ->
        bind ty2 @@ fun w2 -> Exist (warr, Some (Arrow (w1, w2)), k warr)
      | Prod tys ->
        let wprod = Constraint.Var.fresh "prod" in
        let rec loop tys k =
          match tys with
          | [] -> k []
          | ty :: tys -> bind ty @@ fun w -> loop tys (fun ws -> k (w :: ws))
        in
        loop tys (fun ws -> Exist (wprod, Some (Prod ws), k wprod)) )


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
    | Untyped.Var x ->
      (* [[x : w]] := (E(x) = w) *)
      let+ () = eq w (Env.find x env) in
      STLC.Var x
    | Untyped.App (t, u) ->
      (* [[t u : w]] := ∃wu. [[t : wu -> w]] ∧ [[u : wu]] *)
      let wu, wt = (Constraint.Var.fresh "wu", Constraint.Var.fresh "wt") in
      Exist
        ( wu,
          None,
          Exist
            ( wt,
              Some (Arrow (wu, w)),
              let+ t' = has_type env t wt
              and+ u' = has_type env u wu in
              STLC.App (t', u') ) )
    | Untyped.Abs (x, t) ->
      (* [[fun x -> t : w]] := ∃wx wt. w = wx -> wt ∧ [[t : wt]](E,x↦wx) *)
      let wx, wt, warr =
        ( Constraint.Var.fresh (Untyped.Var.name x),
          Constraint.Var.fresh "wt",
          Constraint.Var.fresh "warr" )
      in
      Exist
        ( wx,
          None,
          Exist
            ( wt,
              None,
              Exist
                ( warr,
                  Some (Arrow (wx, wt)),
                  let+ () = eq w warr
                  and+ tyx = decode wx
                  and+ t' = has_type (Env.add x wx env) t wt in
                  STLC.Abs (x, tyx, t') ) ) )
    | Untyped.Let (x, t, u) ->
      (* [[let x = t in u : w]] := ∃wt. [[t : wt]] ∧ [[u : w]](E,x↦wt) *)
      let wt = Constraint.Var.fresh (Untyped.Var.name x) in
      Exist
        ( wt,
          None,
          let+ t' = has_type env t wt
          and+ tyx = decode wt
          and+ u' = has_type (Env.add x wt env) u w in
          STLC.Let (x, tyx, t', u') )
    | Untyped.Annot (t, ty) ->
      (* [[(t : ty) : w]] := ∃(wt = ty). [[t : wt]] /\ [[wt = w]] *)
      bind ty @@ fun wt ->
      let+ t' = has_type env t wt
      and+ () = eq wt w in
      t'
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
          let w = Printf.ksprintf Constraint.Var.fresh "w%d" (i + 1) in
          Exist
            ( w,
              None,
              let+ t' = has_type env t w
              and+ ts' = loop (i + 1) ts (fun ws -> k (w :: ws)) in
              t' :: ts' )
      in
      let+ ts =
        loop 0 ts (fun ws ->
          let wprod = Constraint.Var.fresh "wprod" in
          Exist
            ( wprod,
              Some (Prod ws),
              let+ () = eq w wprod in
              [] ) )
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
          let wx = Constraint.Var.fresh (Untyped.Var.name x) in
          Exist (wx, None, loop xs (fun ws -> k (wx :: ws)))
      in
      loop xs (fun ws ->
        let wt = Constraint.Var.fresh "wt" in
        Exist
          ( wt,
            Some (Prod ws),
            let+ bindings =
              let rec loop = function
                | [] -> Ret (fun _sol -> [])
                | (x, w) :: ws ->
                  let+ ty = decode w
                  and+ bindings = loop ws in
                  (x, ty) :: bindings
              in
              loop (List.combine xs ws)
            and+ t' = has_type env t wt
            and+ u' =
              let env = List.fold_right2 Env.add xs ws env in
              has_type env u w
            in
            STLC.LetTuple (bindings, t', u') ) )
    | Do p -> Do (T.map (fun t -> has_type env t w) p)
end
