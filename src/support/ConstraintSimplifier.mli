module Make(T : Utils.Functor) : sig
  type env = Unif.Env.t
  module SatConstraint := SatConstraint.Make(T)

  val simplify : env -> SatConstraint.t -> SatConstraint.t
end
