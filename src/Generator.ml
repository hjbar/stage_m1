open Constraint

module Env = Untyped.Var.Map
type env = variable Env.t

let rec has_type (env : env) (t : Untyped.term) (w : variable) : (STLC.term, _) t =
  match t with
  | Untyped.Var x ->
    (* [[x : w]] := (E(x) = w) *)
    let+ () = Eq(w, Env.find x env)
    in STLC.Var x
  | Untyped.App (t, u) ->
     (* [[t u : w]] := ∃wu. [[t : wu -> w]] ∧ [[u : wu]] *)
    let wu, wt = Constraint.Var.fresh "wu", Constraint.Var.fresh "wt" in
    Exist (wu, None,
      Exist (wt, Some (Arrow (wu, w)),
        let+ t' = has_type env t wt
        and+ u' = has_type env u wu
        in STLC.App(t', u')
      ))
  | Untyped.Abs (x, t) ->
    (* [[fun x -> t : w]] := ∃wx wt. w = wx -> wt ∧ [[t : wt]](E,x↦wx) *)
    let wx, wt, warr =
      Constraint.Var.fresh (Untyped.Var.name x),
      Constraint.Var.fresh "wt",
      Constraint.Var.fresh "warr" in
    Exist (wx, None, Exist (wt, None, Exist (warr, Some (Arrow (wx, wt)),
      let+ () = Eq(w, warr)
      and+ tyx = Decode wx
      and+ t' = has_type (Env.add x wx env) t wt
      in STLC.Abs(x, tyx, t')
    )))
  | Untyped.Let (x, t, u) ->
    (* [[let x = t in u : w]] := ∃wt. [[t : wt]] ∧ [[u : w]](E,x↦wt) *)
    let wt = Constraint.Var.fresh (Untyped.Var.name x) in
    Exist (wt, None,
      let+ t' = has_type env t wt
      and+ tyx = Decode wt
      and+ u' = has_type (Env.add x wt env) u w
      in STLC.Let(x, tyx, t', u')
   )
