module Make (T : Utils.Functor) : sig
  module Constraint := Constraint.Make(T)
  module SatConstraint := SatConstraint.Make(T)

  val print_sat_constraint : SatConstraint.t -> PPrint.document

  val print_constraint : ('a, 'e) Constraint.t -> PPrint.document

  val print_constraint_in_context :
    env:PPrint.document ->
    ('a1, 'e1) Constraint.t ->
    ('a1, 'e1, 'a, 'e) Constraint.cont ->
    PPrint.document
end
