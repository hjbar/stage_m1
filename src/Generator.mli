module Make (M : Utils.MonadPlus) : sig
  module Untyped := Untyped.Make(M)
  module Constraint := Constraint.Make(M)
  module Infer := Infer.Make(M)

  val untyped_gasche : Untyped.term

  val untyped_vanille : Untyped.term

  val constraint_ : Untyped.term -> (F.term * F.scheme, Infer.err) Constraint.t

  val typed_cut_early : size:int -> Untyped.term -> (F.term * F.scheme) M.t

  val typed_cut_late : size:int -> Untyped.term -> (F.term * F.scheme) M.t
end
