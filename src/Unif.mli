(** The Unif module provides unification, which is a key ingredient of type
    inference. This is exposed as a persistent "equation environment"
    [Unif.Env.t] that stores the current knowledge on inference variables
    obtained by constraint evaluation:

    - which inference variables are equal to each other
    - for each inference variable, its known structure (if any) *)

(* Variable type *)

type var = Constraint.variable

(* Structure type *)

type structure = var Structure.t option

(* Status type *)

type status =
  | Flexible (* Variable not yet generalized *)
  | Generic (* Variable already generalized *)

(* Rank type *)

type rank = int

val base_rank : int (* The top-level rank *)

(* Pool type *)

module IntMap : Map.S with type key = int

type pool = var list IntMap.t

(** [repr] represents all the knowledge so far about an inference variable, or
    rather an equivalence class of inference variables that are equal to each
    other:
    - [var] is a choice of canonical representant for the equivalence class
    - [structure] is the known structure (if any) of these variables *)
type repr =
  { var : var
  ; structure : structure
  ; status : status
  ; rank : rank
  }

(* Signature of the Environment module *)

module Env : sig
  (* type [t] *)

  type t

  (* Empty environment *)

  val empty : unit -> t

  (* Functions to check whether parts of the environment are empty *)

  val map_is_empty : t -> bool

  val pool_is_empty : t -> bool

  val pool_k_is_empty : rank -> t -> bool

  (* Membership test functions *)

  val mem : var -> t -> bool

  (* Getter functions for environment data *)

  val get_young : t -> rank

  val get_pool : rank -> t -> var list

  (** [repr x env] gets the representant of [x] in [env].
      @raise [Not_found] if [x] is not bound in [env]. *)
  val repr : var -> t -> repr

  (* Functions to add or register variables to the environment *)

  val add : repr -> t -> t

  val register : var -> rank:int -> t -> t

  val add_flexible : var -> structure -> t -> t

  (* Functions to remove variables from the environment *)

  val unbind : var -> t -> t

  (* Setter functions *)

  val set : repr -> t -> t

  (* Functions to manipulate the environment's young rank *)

  val incr_young : t -> t

  val decr_young : t -> t

  (* Functions to manipulate the pool *)

  val clean_pool : rank -> t -> t

  (* Functions on representatives *)

  val is_representative : var -> t -> bool

  (* Debugging functions *)

  val debug_var : var -> t -> PPrint.document

  val debug_env : t -> PPrint.document

  val debug_pool : t -> PPrint.document
end

(** Unification errors:
    - [Clash] indicates that we tried to unify two variables with incompatible
      structure -- equating them would make the context inconsistent. It returns
      the pair of variables with incompatible structure.

    - [Cycle] indicates that unifying two variables would introduce a cyclic,
      infinite type. It returns one variable belonging to the prospective cycle.
*)

type err =
  | Clash of var Utils.clash
  | Cycle of var Utils.cycle

(** [unify env v1 v2] takes the current equation environment [env], and tries to
    update it with the knowledge that [v1], [v2] must be equal. If this equality
    would introduce an error, we fail with the error report, otherwise we return
    the updated equation environment. Can raise Invalid_argument if v1 or v2 are
    not is the original environment. *)
val unify : Env.t -> var -> var -> (Env.t, err) result

(** [unifiable env v1 v2] tests if unifying [v1] and [v2] in the equation
    environment [env] would succeed. *)
val unifiable : Env.t -> var -> var -> bool
