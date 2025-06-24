module TyVar : module type of Utils.Variables ()

type ('v, 'a) t_ =
  | Var of 'v
  | Arrow of 'a * 'a
  | Prod of 'a list

type 'a raw = (string, 'a) t_

type 'a t = (TyVar.t, 'a) t_

val iter : ('a -> unit) -> ('v, 'a) t_ -> unit

val map : ('a -> 'b) -> ('v, 'a) t_ -> ('v, 'b) t_

val merge : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t option

val freshen : ('a -> 'b) -> 'a raw -> 'b t

val print : ('a -> PPrint.document) -> 'a t -> PPrint.document
