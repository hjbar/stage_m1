module Make (T : Utils.Functor) : sig
  module Constraint := Constraint.Make(T)

  module Env : sig
    module SMap : Map.S with type key = Constraint.SVar.t

    type unif = Unif.Env.t

    type schemes = Generalization.scheme SMap.t

    type t =
      { unif : unif
      ; schemes : schemes
      }

    val empty : t

    val debug : t -> PPrint.document
  end

  type log = PPrint.document list

  (** Normal constraints are the result of solving constraints without computing
      inside [Do p] nodes. *)
  type ('a, 'e) normal_constraint =
    | NRet of 'a Constraint.on_sol
      (** A succesfully elaborated value. (See Constraint.ml for exaplanations
          on [on_sol].) *)
    | NErr of 'e  (** A failed/false constraint. *)
    | NDo of ('a, 'e) Constraint.t T.t
      (** A constraint whose evaluation encountered an effectful constraint in a
          [Do p] node.

          We propose an evaluation rule of the form [eval E[Do p] = NDo E[p]]
          where a [Do (p : ('a1, 'e1) Constraint.t T.t)] node placed inside an
          evaluation context [E] bubbles "all the way to the top" in the result.
          [E[p]] is defined by using [T.map] to lift the context-plugging
          operation [E[_] : ('a1, 'e1) Constraint.t -> ('a2, 'e2) Constraint.t]
      *)

  (** If [~log:true] is passed in input, print to stderr a list of intermediate
      steps (obtained from the solver environment and the original constraint by
      constraint simplification) as the constraint-solving progresses. *)
  val eval :
    log:bool -> Env.t -> ('a, 'e) Constraint.t -> Env.t * ('a, 'e) normal_constraint
end
