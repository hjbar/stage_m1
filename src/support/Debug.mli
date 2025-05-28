(* One-lien functions *)

val print_document : PPrint.document -> unit

val print_header : string -> PPrint.document -> unit

val print_sub_header : string -> PPrint.document -> unit

val print_message : string -> unit

(* Others functions *)

val debug_what_rank : Constraint.variable -> Unif.Env.t -> unit

val debug_what_repr_assoc : Constraint.variable -> Unif.Env.t -> unit

val debug_what_pool_assoc : Unif.rank -> Unif.Env.t -> unit
