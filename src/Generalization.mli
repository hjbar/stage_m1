(* Import some types *)

type variable = Unif.var

type structure = Unif.structure

type data = Unif.repr

type env = Unif.Env.t

type err = Unif.err

(* Type of scheme *)

type scheme

and root = variable

and roots = root list

and quantifiers = variable list

and schemes = scheme list

(* Function on schemes *)

val body : scheme -> root

val quantifiers : scheme -> quantifiers

val trivial : variable -> scheme

val debug_scheme : scheme -> PPrint.document

(* Functions on generalization *)

val fresh_flexible : ?name:string -> structure -> env -> env * variable

val enter : env -> env

val exit : roots -> env -> env * quantifiers * schemes

val instantiate : scheme -> variable -> env -> env * (quantifiers, err) result
