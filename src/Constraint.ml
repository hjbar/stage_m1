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

  (* TODO document [t] and [on_sol]. *)
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
