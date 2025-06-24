module Make (M : Utils.MonadPlus) : sig
  module Untyped := Untyped.Make(M)
  module Constraint := Constraint.Make(M)
  module Infer := Infer.Make(M)

  val untyped_gasche : Untyped.term

  val untyped_vanille : Untyped.term

  val cut_size : size:int -> Untyped.term -> Untyped.term

  val constraint_ : Untyped.term -> (STLC.term, Infer.err) Constraint.t

  val typed_cut_early : size:int -> Untyped.term -> STLC.term M.t

  val typed_cut_late : size:int -> Untyped.term -> STLC.term M.t
end
