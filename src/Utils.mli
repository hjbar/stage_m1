type 'a clash = 'a * 'a

type 'v cycle = Cycle of 'v [@@unboxed]

type loc = Lexing.position * Lexing.position

val string_of_loc : loc -> string

val string_of_doc : PPrint.document -> string

module Variables () : sig
  type t = private {
    name : string;
    stamp : int;
  }

  val compare : t -> t -> int

  val eq : t -> t -> bool

  val fresh : string -> t

  val namegen : string array -> unit -> t

  val name : t -> string

  val print : t -> PPrint.document

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end

module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** A signature for search monads, that represent computations that enumerate
    zero, one or several values. *)
module type MonadPlus = sig
  include Functor

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

module Empty : sig
  (* the empty type *)
  type 'a t = |

  val map : ('a -> 'b) -> 'a t -> 'b t
end
