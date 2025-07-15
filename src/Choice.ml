(** A signature for search monads, that represent computations that enumerate
    zero, one or several values. *)
module type Intf = sig
  include Utils.Functor

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val sum : 'a t list -> 'a t

  val fail : 'a t

  (** [fail] and [one_of] can be derived from [sum], but they typically have
      simpler and more efficient specialized implementations. *)
  val one_of : 'a array -> 'a t

  (** Many search monad implementations perform their computation on-demand,
      when elements are requested, instead of forcing computation already to
      produce the ['a t] value.

      In a strict language, it is easy to perform computation too early in this
      case, for example [M.sum [foo; bar]] will compute [foo] and [bar] eagerly
      even though [bar] may not be needed if we only observe the first element.

      The [delay] combinator makes this on-demand nature explicit, for example
      one can write [M.delay (fun () -> M.sum [foo; bar])] to avoid computing
      [foo] and [bar] too early. Of course, if the underlying implementation is
      in fact eager, then this may apply the function right away.*)
  val delay : (unit -> 'a t) -> 'a t

  (** ['a Seq.t] is a type of on-demand sequences from the OCaml standard
      library: https://v2.ocaml.org/api/Seq.html *)
  val run : 'a t -> 'a Seq.t
end
