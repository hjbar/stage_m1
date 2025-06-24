module Make (T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)
  module Constraint := Constraint.Make(T)

  type err =
    | Clash of STLC.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  type env = Constraint.variable Untyped.Var.Map.t

  val has_type :
    env -> Untyped.term -> Constraint.variable -> STLC.term constraint_

  val exist_wrapper : Untyped.term -> (STLC.term * STLC.ty, err) Constraint.t
end
