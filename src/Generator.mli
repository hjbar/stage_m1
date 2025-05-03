module Make(M : Utils.MonadPlus) : sig
  module Untyped := Untyped.Make(M)
  module Constraint := Constraint.Make(M)
  module Infer := Infer.Make(M)

  val untyped_gasche : Untyped.term
  val untyped_vanille : Untyped.term

  val constraint_ : Untyped.term -> (STLC.term, Infer.err) Constraint.t

  val typed : size:int -> Untyped.term -> STLC.term M.t
end
