module Make (T : Utils.Functor) : sig
  module Constraint := Constraint.Make(T)
  module SatConstraint := SatConstraint.Make(T)

  val print_sat_constraint : SatConstraint.t -> PPrint.document

  val print_constraint : ('a, 'e) Constraint.t -> PPrint.document
end
