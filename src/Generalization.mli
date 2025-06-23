(* Type of scheme *)

type scheme

(* Functions on schemes *)

val body : scheme -> Unif.var

val quantifiers : scheme -> Unif.var list

val debug_scheme : scheme -> PPrint.document

(* Functions for generalization *)

val fresh_flexible :
  ?name:string -> Unif.structure -> Unif.Env.t -> Unif.Env.t * Unif.var

val enter : Unif.Env.t -> Unif.Env.t

val exit : Unif.var list -> Unif.Env.t -> Unif.Env.t * scheme list

val instantiate :
     scheme
  -> Unif.var
  -> Unif.Env.t
  -> Unif.Env.t * (Unif.var list, Unif.err) result
