module TyVar = Structure.TyVar

type 'a structure = 'a Structure.t

type ty =
  | Constr of ty structure

module TeVar = Utils.Variables ()

type term =
  | Var of TeVar.t
  | App of term * term
  | Abs of TeVar.t * ty * term
  | Let of TeVar.t * ty * term * term
  | Annot of term * ty
  | Tuple of term list
  | LetTuple of (TeVar.t * ty) list * term * term
