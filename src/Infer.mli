module Make (T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)
  module Constraint := Constraint.Make(T)

  type err =
    | Clash of F.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  val eq : Constraint.variable -> Constraint.variable -> unit constraint_

  val decode : Constraint.variable -> F.ty constraint_

  val decode_scheme : Constraint.scheme_variable -> F.scheme constraint_

  type env =
    Constraint.variable Untyped.Var.Map.t
    * Constraint.scheme_variable Untyped.Var.Map.t

  val has_type :
    env -> Untyped.term -> Constraint.variable -> F.term constraint_
end
