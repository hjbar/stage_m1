module Make (T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)
  module Constraint := Constraint.Make(T)

  module Env : sig
    type t

    val empty : t
  end

  type err =
    | Clash of F.ty Utils.clash
    | Cycle of Constraint.variable Utils.cycle

  type 'a constraint_ = ('a, err) Constraint.t

  val has_type :
    Env.t -> Untyped.term -> Constraint.variable -> F.term constraint_

  val let_wrapper : Untyped.term -> (F.term * F.scheme, err) Constraint.t
end
