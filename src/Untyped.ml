module Var = STLC.TeVar

type term =
  | Var of Var.t
  | App of term * term
  | Abs of Var.t * term
  | Let of Var.t * term * term
  | Annot of term * STLC.ty
