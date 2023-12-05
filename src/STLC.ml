module TyVar = Utils.Variables()

module Structure = struct
  type 'a t =
    | Var of TyVar.t
    | Arrow of 'a * 'a
    | Prod of 'a list

  let map f = function
    | Var alpha -> Var alpha
    | Arrow (t1, t2) -> Arrow (f t1, f t2)
    | Prod ts -> Prod (List.map f ts)

  let map2 f s1 s2 = match s1, s2 with
    | Var alpha, Var beta ->
      if TyVar.compare alpha beta = 0 then Some s1
      else None
    | Var _, _ | _, Var _ -> None

    | Arrow (a1, a2), Arrow (b1, b2) ->
      let c1 = f a1 b1 in
      let c2 = f a2 b2 in
      Some (Arrow (c1, c2))
    | Arrow _, _ | _, Arrow _ -> None

    | Prod as1, Prod as2 ->
      if List.length as1 <> List.length as2
      then None
      else Some (Prod (List.map2 f as1 as2))

  let print p = function
    | Var v -> TyVar.print v
    | Prod ts -> Printer.product (List.map p ts)
    | Arrow (t1, t2) -> Printer.arrow (p t1) (p t2)
end

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
