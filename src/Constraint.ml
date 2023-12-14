module Types = struct
  module Var = Utils.Variables()

  type variable = Var.t
  type structure = variable Structure.t

  type ty =
    | Var of variable
    | Constr of structure
end
include Types

module Make (T : Utils.Applicative) = struct
  include Types

  type eq_error =
    | Clash of STLC.ty Utils.clash
    | Cycle of variable Utils.cycle

  type 'a on_sol = (variable -> STLC.ty) -> 'a

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

  let (let+) c f = Map(c, f)
  let (and+) c1 c2 = Conj(c1, c2)
end
