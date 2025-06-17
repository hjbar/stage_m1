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
  type ('ok, 'err) constraint_result =
    | RRet : 'a -> ('a, 'e) constraint_result
      (** A succesfully elaborated value. (See Constraint.ml for exaplanations
          on [on_sol].) *)
    | RErr : 'e -> ('a, 'e) constraint_result  (** A failed/false constraint. *)
    | RDo :
        ('a, 'e) Constraint.t T.t * ('a, 'e, 'ok, 'err) cont
        -> ('ok, 'err) constraint_result
      (** A constraint whose evaluation encountered an effectful constraint in a
          [Do (p, k)] node.

          We propose an evaluation rule of the form
          [eval E[Do (p, k)] = NDo E[(p, k)]] where a
          [Do (p : ('a1, 'e1) Constraint.t T.t) (k : cont)] node placed inside
          an evaluation context [E] bubbles "all the way to the top" in the
          result. [E[p]] is defined by using [T.map] to lift the
          context-plugging operation
          [E[_] : ('a1, 'e1) Constraint.t -> ('a2, 'e2) Constraint.t] *)

  and ('a, 'e, 'ok, 'err) cont

  val cont_done : ('ok, 'err, 'ok, 'err) cont

  (** If [~log:true] is passed in input, print to stderr a list of intermediate
      steps (obtained from the solver environment and the original constraint by
      constraint simplification) as the constraint-solving progresses. *)
  val eval :
       log:bool
    -> Env.t
    -> ('a1, 'e1) Constraint.t
    -> ('a1, 'e1, 'a, 'e) cont
    -> Env.t * ('a, 'e) constraint_result
end
