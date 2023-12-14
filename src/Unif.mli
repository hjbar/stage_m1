(* TODO document *)

type var = Constraint.variable
type repr = {
  var: var;
  structure: Constraint.structure option;
}

module Env : sig
  type t

  val empty : unit -> t

  val mem : var -> t -> bool

  val add : var -> Constraint.structure option -> t -> t

  (** [repr x env] gets the representant of [x] in [env].

      @raise [Not_found] if [x] is not bound in [env]. *)
  val repr : var -> t -> repr
end

type err =
  | Clash of var Utils.clash
  | Cycle of var Utils.cycle

val unify : Env.t -> var -> var -> (Env.t, err) result

val unifiable : Env.t -> var -> var -> bool
