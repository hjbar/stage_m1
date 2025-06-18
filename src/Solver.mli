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
        ('a, 'e) Constraint.t T.t * ('a, 'e, 'ok, 'err) cont
        -> ('ok, 'err) normal_constraint
      (** A constraint whose evaluation encountered an effectful constraint in a
          [Do (p, k)] node.

          We propose an evaluation rule of the form
          [eval E[Do p] = NDo (p, E)] where a
          [Do (p : ('a1, 'e1) Constraint.t T.t) (k : cont)] node placed inside
          an constraint context [E] bubbles "all the way to the top" in the
          result. *)

  and ('ok1, 'err1, 'ok, 'err) cont_frame =
    | KMap : ('ok1 -> 'ok2) -> ('ok1, 'err, 'ok2, 'err) cont_frame
    | KMapErr : ('err1 -> 'err2) -> ('ok, 'err1, 'ok, 'err2) cont_frame
    | KConj1 :
        ('ok2, 'err) Constraint.t
        -> ('ok1, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KConj2 :
        'ok1 Constraint.on_sol
        -> ('ok2, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KExist : Constraint.variable -> ('ok, 'err, 'ok, 'err) cont_frame
    | KLet1 :
        Constraint.scheme_variable
        * Constraint.variable
        * ('ok2, 'err) Constraint.t
        -> ('ok1, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KLet2 :
        'ok1 Constraint.on_sol
        -> ('ok2, 'err, 'ok1 * 'ok2, 'err) cont_frame

  and ('ok1, 'err1, 'ok, 'err) cont =
    | Done : ('ok, 'err, 'ok, 'err) cont
    | Next :
        ('ok1, 'err1, 'ok2, 'err2) cont_frame * ('ok2, 'err2, 'ok, 'err) cont
        -> ('ok1, 'err1, 'ok, 'err) cont

  (** If [~log:true] is passed in input, print to stderr a list of intermediate
      steps (obtained from the solver environment and the original constraint by
      constraint simplification) as the constraint-solving progresses. *)
  val eval :
       log:bool
    -> Env.t
    -> ('a1, 'e1) Constraint.t
    -> ('a1, 'e1, 'a, 'e) cont
    -> Env.t * ('a, 'e) normal_constraint
end
