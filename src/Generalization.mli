(* Type of scheme *)

type scheme

(* Functions on schemes *)

val body : scheme -> Unif.var

val quantifiers : scheme -> Unif.var list

val debug_scheme : scheme -> PPrint.document

(* Generalization environment *)

type var := Constraint.variable

module Env : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val debug : Unif.Env.t -> t -> PPrint.document
end

val add_flexible :
  Unif.Env.t -> Env.t -> var -> Unif.structure -> Unif.Env.t * Env.t

(* Functions for generalization *)

val enter : Env.t -> Env.t

val exit : Unif.Env.t -> Env.t -> var list -> Unif.Env.t * Env.t * scheme list

val instantiate :
  Unif.Env.t -> Env.t -> scheme -> Unif.Env.t * Env.t * Unif.var * Unif.var list
