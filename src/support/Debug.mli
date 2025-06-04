(* One-lien functions *)

val print_document : PPrint.document -> unit

val print_header : string -> PPrint.document -> unit

val print_sub_header : string -> PPrint.document -> unit

val print_message : string -> unit

(* Others functions *)

val debug_what : Unif.Env.t -> unit

val debug_what_pool_assoc : Unif.rank -> Unif.Env.t -> unit
