module Make(T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)
  module Constraint := Constraint.Make(T)

  type err =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type env = Constraint.variable Untyped.Var.Map.t

  val has_type : env ->
    Untyped.term -> Constraint.variable -> (STLC.term, err) Constraint.t
end
