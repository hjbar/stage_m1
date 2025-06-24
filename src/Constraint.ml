(** Constraint defines the type of type-inference constraints that our solver
    understands -- see Solver.ml.

    In theory this can let you perform type inference for many different
    languages, as long as their typing rules can be expressed by the constraints
    we have defined. In practice most non-trivial language features will require
    extending the language of constraints (and the solver) with new constructs.
*)

(* We found it convenient to include some type definitions both inside
   and outside the Make functor. Please don't let this small quirk
   distract you. *)

module Types = struct
  (* Constraint variables *)

  module Var = Utils.Variables ()

  type variable = Var.t

  type structure = variable Structure.t

  type ty =
    | Var of variable
    | Constr of structure

  (* Scheme variables *)

  module SVar = Utils.Variables ()

  type scheme_variable = SVar.t
end

include Types

module Make (T : Utils.Functor) = struct
  include Types

  type eq_error =
    | Clash of F.ty Utils.clash
    | Cycle of variable Utils.cycle

  (** A value of type [('a, 'e)) t] is a constraint whose resolution will either
      succeed, and produce a witness of type ['a], or fail and produce error
      information at type ['e].

      In particular, type inference from an untyped language can be formulated
      as a function of type [untyped_term -> (typed_term, type_error) t].

      This is a GADT (Generalized Algebraic Datatype). If you are unfamiliar
      with function declarations of the form
      [let rec foo : type a e . (a, e) t -> ...] then you should read the GADT
      chapter of the OCaml manual:
      https://v2.ocaml.org/releases/5.1/htmlman/gadts-tutorial.html *)
  type ('ok, 'err) t =
    | Loc : Utils.loc * ('a, 'e) t -> ('a, 'e) t
    | Ret : 'a on_sol -> ('a, 'e) t
    | Err : 'e -> ('a, 'e) t
    | Map : ('a, 'e) t * ('a -> 'b) -> ('b, 'e) t
    | MapErr : ('a, 'e) t * ('e -> 'f) -> ('a, 'f) t
    | Conj : ('a, 'e) t * ('b, 'e) t -> ('a * 'b, 'e) t
    | Eq : variable * variable -> (unit, eq_error) t
    | Exist : variable * structure option * ('a, 'e) t -> ('a, 'e) t
    | Decode : variable -> (F.ty, variable Utils.cycle) t
    | Do : ('a, 'e) t T.t -> ('a, 'e) t
    | DecodeScheme : scheme_variable -> (F.scheme, variable Utils.cycle) t
    | Instance : scheme_variable * variable -> (F.ty list, eq_error) t
    | Let :
        (scheme_variable * variable) list * ('a, 'e) t * ('b, 'e) t
        -> ('a * 'b, 'e) t

  (** ['a on_sol] represents an elaborated witness of type ['a] that depends on
      the solution to the whole constraint -- represented by a mapping from
      inference variables to elaborated types, [variable -> F.ty].

      This is used in the success constraint above
      [Ret : 'a on_sol -> ('a, 'e) t]; using just [Ret : 'a -> ('a, 'e) t] would
      not work, as the witness we want to produce may depend on the solution to
      the whole constraint.

      For example, consider the untyped term [(lambda y. 42) 0] The
      sub-constraint generated for [lambda y. 42] could be resolved first by the
      constraint solver and found satisfiable, but at this point we don't know
      what the final type for [y] will be, it will be a still-undetermined
      inference variable [?w]. The actual type for [?w] will only be determined
      when resolving other parts of the whole constraint that handle the
      application of [0]. We have to solve the whole constraint, and then come
      back to elaborate an explictly-typed term [lambda (y : int). 42]. *)
  and 'a on_sol = (variable -> F.ty) -> 'a

  let ( let+ ) c f = Map (c, f)

  (** These are "binding operators". Usage example:
      {[
        let+ ty1 = Decode w1
        and+ ty2 = Decode w2 in
        Constr (Arrow (ty1, ty2))
      ]}

      After desugaring the binding operators, this is equivalent to
      {[
        Map
          ( Conj (Decode w1, Decode w2),
            fun (ty1, ty2) -> Constr (Arrow (ty1, ty2)) )
      ]}

      For more details on binding operators, see
      https://v2.ocaml.org/releases/5.1/manual/bindingops.html *)
  let ( and+ ) c1 c2 = Conj (c1, c2)

  (* The continuation of a constraint (used mostly in the solver) *)
  type ('ok1, 'err1, 'ok, 'err) cont =
    | Done : ('ok, 'err, 'ok, 'err) cont
    | Next :
        ('ok1, 'err1, 'ok2, 'err2) cont_frame * ('ok2, 'err2, 'ok, 'err) cont
        -> ('ok1, 'err1, 'ok, 'err) cont

  and ('ok1, 'err1, 'ok, 'err) cont_frame =
    | KMap : ('ok1 -> 'ok2) -> ('ok1, 'err, 'ok2, 'err) cont_frame
    | KMapErr : ('err1 -> 'err2) -> ('ok, 'err1, 'ok, 'err2) cont_frame
    | KConj1 : ('ok2, 'err) t -> ('ok1, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KConj2 : 'ok1 on_sol -> ('ok2, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KExist : variable -> ('ok, 'err, 'ok, 'err) cont_frame
    | KLet1 :
        (scheme_variable * variable) list * ('ok2, 'err) t
        -> ('ok1, 'err, 'ok1 * 'ok2, 'err) cont_frame
    | KLet2 : 'ok1 on_sol -> ('ok2, 'err, 'ok1 * 'ok2, 'err) cont_frame
end
