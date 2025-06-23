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

  val decode : Constraint.variable -> F.ty constraint_

  val decode_scheme : Constraint.scheme_variable -> F.scheme constraint_

  val has_type :
    Env.t -> Untyped.term -> Constraint.variable -> F.term constraint_
end
