(** Constraint defines the type of type-inference constraints that our
    solver understands -- see Solver.ml.

    In theory this can let you perform type inference for many
    different languages, as long as their typing rules can be
    expressed by the constraints we have defined. In practice most
    non-trivial language features will require extending the language
    of constraints (and the solver) with new constructs. *)

(* We found it convenient to include some type definitions both inside
   and outside the Make functor. Please don't let this small quirk
   distract you. *)
module Types = struct
  module Var = Utils.Variables()

  type variable = Var.t
  type structure = variable Structure.t

  type ty =
    | Var of variable
    | Constr of structure
end
include Types

module Make (T : Utils.Functor) = struct
  include Types

  type eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of variable Utils.cycle

  (** A value of type [('a, 'e) t] is a constraint
      whose resolution will either succeed, and produce
      a witness of type ['a], or fail and produce
      error information at type ['e].

      In particular, type inference from an untyped language
      can be formulated as a function of type
      [untyped_term -> (typed_term, type_error) t].

      This is a GADT (Generalized Algebraic Datatype). If
      you are unfamiliar with function declarations of the
      form
      [let rec foo : type a e . (a, e) t -> ...]
      then you should read the GADT chapter of the OCaml manual:
        https://v2.ocaml.org/releases/5.1/htmlman/gadts-tutorial.html
  *)
  type ('ok, 'err) t =
    | Ret : 'a on_sol -> ('a, 'e) t
    | Err : 'e -> ('a, 'e) t
    | Map : ('a, 'e) t * ('a -> 'b) -> ('b, 'e) t
    | MapErr : ('a, 'e) t * ('e -> 'f) -> ('a, 'f) t
    | Conj : ('a, 'e) t * ('b, 'e) t -> ('a * 'b, 'e) t
    | Eq : variable * variable -> (unit, eq_error) t
    | Exist : variable * structure option * ('a, 'e) t -> ('a, 'e) t
    | Decode : variable -> (STLC.ty, variable Utils.cycle) t
    | Do : ('a, 'e) t T.t -> ('a, 'e) t

  and 'a on_sol = (variable -> STLC.ty) -> 'a
  (** A value of type [('a, 'e) t] represents a part of an
      inference constraint, but the value of type ['a] that
      it produces on success may depend on the solution to
      the whole constraint, not just for this part of the
      constraint.

      For example, consider the untyped term
        [(lambda y. 42) 0]
      The sub-constraint generated for [lambda y. 42] could
      be resolved first by the constraint solver and found
      satisfiable, but at this point we don't know what the
      final type for [y] will be, it will be
      a still-undetermined inference variable [?w]. The
      actual type for [?w] will only be determined when
      resolving other parts of the whole constraint that
      handle the application of [0]. We have to solve the
      whole constraint, and then come back to elaborate an
      explictly-typed term [lambda (y : int). 42].

      The solution to the whole constraint is represented by
      a mapping from inference variables to elaborated
      types.
  *)

  let (let+) c f = Map(c, f)
  let (and+) c1 c2 = Conj(c1, c2)
  (** These are "binding operators". Usage example:
      {[
        let+ ty1 = Decode w1
        and+ ty2 = Decode w2
        in Constr (Arrow (ty1, ty2))
      ]}

      After desugaring the binding operators, this is equivalent to
      {[
      Map(Conj(Decode w1, Decode w2), fun (ty1, ty2) ->
        Constr (Arrow (ty1, ty2)))
      ]}

      For more details on binding operators, see
        https://v2.ocaml.org/releases/5.1/manual/bindingops.html
  *)
end
