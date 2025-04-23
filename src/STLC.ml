(* A type of explicitly-typed terms. *)

module TyVar = Structure.TyVar

type 'v ty_ = Constr of ('v, 'v ty_) Structure.t_

type raw_ty = string ty_

type ty = TyVar.t ty_

let rec freshen_ty (Constr s) = Constr (Structure.freshen freshen_ty s)

module TeVar = Utils.Variables ()

type term =
  | Var of TeVar.t
  | App of term * term
  | Abs of TeVar.t * ty * term
  | Let of TeVar.t * ty * term * term
  | Annot of term * ty
  | Tuple of term list
  | LetTuple of (TeVar.t * ty) list * term * term
