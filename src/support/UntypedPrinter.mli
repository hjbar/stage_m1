module Make (T : Utils.Functor) : sig
  module Untyped := Untyped.Make(T)

  val print_term : Untyped.term -> PPrint.document
end
