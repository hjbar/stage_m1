module Make (T : Utils.Functor) : sig
  module Constraint := Constraint.Make(T)

  type env = Unif.Env.t
  type log = PPrint.document list

  val eval :
    log:bool -> env ->
    ('a, 'e) Constraint.t ->
    log * env * ('a, 'e) Constraint.t

  val solve :
    log:bool -> env -> ('a, 'e) Constraint.t -> log * ('a, 'e) result
end
