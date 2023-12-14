module TyVar = Utils.Variables()

type ('v, 'a) t_ =
  | Var of 'v
  | Arrow of 'a * 'a
  | Prod of 'a list

type 'a raw = (string, 'a) t_
type 'a t = (TyVar.t, 'a) t_

let iter f = function
  | Var _alpha -> ()
  | Arrow (t1, t2) -> f t1; f t2
  | Prod ts -> List.iter f ts

let map f = function
  | Var alpha -> Var alpha
  | Arrow (t1, t2) -> Arrow (f t1, f t2)
  | Prod ts -> Prod (List.map f ts)

let merge f s1 s2 = match s1, s2 with
  | Var alpha, Var beta ->
    if TyVar.eq alpha beta then Some (Var alpha)
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

let global_tyvar : string -> TyVar.t =
  (* There are no binders for type variables, which are scoped
     globally for the whole term. *)
  let tenv = Hashtbl.create 5 in
  fun alpha ->
    match Hashtbl.find tenv alpha with
    | alpha_var -> alpha_var
    | exception Not_found ->
      let alpha_var = TyVar.fresh alpha in
      Hashtbl.add tenv alpha alpha_var;
      alpha_var

let freshen freshen = function
  | Var alpha -> Var (global_tyvar alpha)
  | Arrow (t1, t2) -> Arrow (freshen t1, freshen t2)
  | Prod ts -> Prod (List.map freshen ts)

let print p = function
  | Var v -> TyVar.print v
  | Prod ts -> Printer.product (List.map p ts)
  | Arrow (t1, t2) -> Printer.arrow (p t1) (p t2)

