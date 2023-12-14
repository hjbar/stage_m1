module Var = Utils.Variables()

type variable = Var.t
type structure = variable Structure.t

type ty =
  | Var of variable
  | Constr of structure
