(* A type of explicitly-typed terms. *)

module TyVar = Structure.TyVar

type 'v ty_ = Constr of ('v, 'v ty_) Structure.t_

type raw_ty = string ty_

type ty = TyVar.t ty_

type scheme = TyVar.t list * ty

let rec freshen_ty (Constr s) = Constr (Structure.freshen freshen_ty s)

module TeVar = Utils.Variables ()

type term =
  | Var of TeVar.t
  | App of term * term
  | Abs of TeVar.t * ty * term
  | TyApp of term * ty list
  | TyAbs of TyVar.t list * term
  | Let of TeVar.t * scheme * term * term
  | Annot of term * ty
  | Tuple of term list
  | LetTuple of (TeVar.t * scheme) list * term * term
