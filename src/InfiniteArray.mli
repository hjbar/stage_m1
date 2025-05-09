(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

(** This module implements infinite arrays, that is, arrays that grow
    transparently upon demand. *)

(** An infinite array of type ['a t] is a (mutable) array whose indices range
    over the interval of zero to infinity. **)
type 'a t
(*@ mutable model view: integer -> 'a *)

(**[make n x] creates an infinite array where every slot contains [x]. The
   parameter [n] is the initial physical size of the underlying OCaml array. It
   must be nonzero. **)
val make : int -> 'a -> 'a t
(*@ a = make n x
    requires 0 < n
    ensures  forall i. a.view(i) = x *)
(*  ensures  a.view = (fun i -> x)   *)

(**[get a i] reads the value stored at index [i] in the infinite array [a]. **)
val get : 'a t -> int -> 'a
(*@ x = get a i
    requires 0 <= i
    ensures  x = a.view(integer_of_int i) *)

(** [set a i x] stores the value [x] at index [i] in the infinite array [a]. **)
val set : 'a t -> int -> 'a -> unit
(*@ set a i x
    modifies a
    requires 0 <= i
    ensures  a.view = (old a.view)[i -> x] *)

(** Syntactic sugar for InfiniteArray **)
module SyntacticSugar : sig
  val ( .*() ) : 'a t -> int -> 'a

  val ( .*()<- ) : 'a t -> int -> 'a -> unit
end
