type 'a clash = 'a * 'a

type 'v cycle = Cycle of 'v [@@unboxed]

type loc = Lexing.position * Lexing.position

val string_of_loc : loc -> string

exception Located of loc * exn * Printexc.raw_backtrace

val at_loc : loc option -> (unit -> 'a) -> 'a

val print_loco : loc option -> unit

val string_of_doc : PPrint.document -> string

val get_section : string -> PPrint.document -> PPrint.document

val print_section : string -> PPrint.document -> unit

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

module Empty : sig
  (* the empty type *)
  type 'a t = |

  val map : ('a -> 'b) -> 'a t -> 'b t
end
