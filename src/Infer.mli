module Make (T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)
  module Constraint := Constraint.Make(T)

  module Env : sig
    type t

    val empty : t
  end

  type err =
    | Clash of Typed.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  val has_type :
    Env.t -> Untyped.term -> Constraint.variable -> Typed.term constraint_

  val let_wrapper :
    Untyped.term -> (Typed.term * Typed.scheme, err) Constraint.t
end
