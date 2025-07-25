module Make (T : Utils.Functor) : sig
  module Constraint := Constraint.Make(T)

  module Env : sig
    module SMap : Map.S with type key = Constraint.SVar.t

    type t = {
      unif : Unif.Env.t;
      gen : Generalization.Env.t;
      schemes : Generalization.scheme SMap.t;
    }

    val empty : unit -> t

    val debug : t -> PPrint.document
  end

  type log = PPrint.document list

  (** Normal constraints are the result of solving constraints without computing
      inside [Do p] nodes. *)
  type ('ok, 'err) normal_constraint =
    | NRet : Env.t * 'a Constraint.on_sol -> ('a, 'e) normal_constraint
      (** A succesfully elaborated value. (See Constraint.ml for exaplanations
          on [on_sol].) *)
    | NErr : Utils.loc option * 'e -> ('a, 'e) normal_constraint
      (** A failed/false constraint. *)
    | NDo : ('a, 'e) normal_constraint T.t -> ('a, 'e) normal_constraint
      (** A constraint whose evaluation encountered an effectful constraint in a
          [Do p] node.

          We propose an evaluation rule of the form
          {[
            eval E[Do p] = NDo E[p]
          ]}
          where a [Do (p : ('a1, 'e1) Constraint.t T.t)] node placed inside an
          evaluation context [E] bubbles "all the way to the top" in the result.
      *)

  (** If [~log:true] is passed in input, print to stderr a list of intermediate
      steps (obtained from the solver environment and the original constraint by
      constraint simplification) as the constraint-solving progresses. *)
  val eval :
    log:bool -> Env.t -> ('a, 'e) Constraint.t -> ('a, 'e) normal_constraint
end
