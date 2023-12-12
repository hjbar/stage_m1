module Var = Utils.Variables()
type variable = Var.t

type structure = variable Structure.t

type ty =
  | Var of variable
  | Constr of structure

type deep_ty =
  | Var of variable
  | Constr of deep_ty Structure.t

type ('ok, 'err) t =
  | Ret : 'a -> ('a, 'e) t
  | Err : 'e -> ('a, 'e) t
  | Map : ('a, 'e) t * ('a -> 'b) -> ('b, 'e) t
  | MapErr : ('a, 'e) t * ('e -> 'f) -> ('a, 'f) t
  | Conj : ('a, 'e) t * ('b, 'e) t -> ('a * 'b, 'e) t
  | Eq : variable * variable -> (unit, STLC.ty Utils.clash) t
  | Exist : variable * structure option * ('a, 'e) t -> ('a, 'e) t
  | Decode : variable -> (STLC.ty, 'e) t

let (and+) c1 c2 = Conj(c1, c2)
let (let+) c f = Map(c, f)
let (let$) c f = MapErr(c, f)
