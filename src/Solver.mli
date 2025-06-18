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
      inside [Do (p, k)] nodes. *)
  type ('ok, 'err) normal_constraint =
    | NRet : 'a Constraint.on_sol -> ('a, 'e) normal_constraint
      (** A succesfully elaborated value. (See Constraint.ml for exaplanations
          on [on_sol].) *)
    | NErr : 'e -> ('a, 'e) normal_constraint  (** A failed/false constraint. *)
    | NDo :
        ('a, 'e) Constraint.t T.t * ('a, 'e, 'ok, 'err) Constraint.cont
        -> ('ok, 'err) normal_constraint
      (** A constraint whose evaluation encountered an effectful constraint in a
          [Do (p, k)] node.

          We propose an evaluation rule of the form [eval E[Do p] = NDo (p, E)]
          where a [Do (p : ('a1, 'e1) Constraint.t T.t) (k : cont)] node placed
          inside an constraint context [E] bubbles "all the way to the top" in
          the result. *)

  (** If [~log:true] is passed in input, print to stderr a list of intermediate
      steps (obtained from the solver environment and the original constraint by
      constraint simplification) as the constraint-solving progresses. *)
  val eval :
       log:bool
    -> Env.t
    -> ('a1, 'e1) Constraint.t
    -> ('a1, 'e1, 'a, 'e) Constraint.cont
    -> Env.t * ('a, 'e) normal_constraint
end
