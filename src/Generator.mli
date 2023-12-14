module Make(M : Utils.MonadPlus) : sig
  module Untyped := Untyped.Make(M)
  module Constraint := Constraint.Make(M)
  module Infer := Infer.Make(M)

  val untyped : Untyped.term

  val constraint_ : (STLC.term, Infer.err) Constraint.t

  val typed : depth:int -> STLC.term M.t
end
