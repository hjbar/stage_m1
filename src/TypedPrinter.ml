open Typed

let print_quantifier (quantifiers : TyVar.t list) : PPrint.document =
  PPrint.group
    (quantifiers |> List.map Typed.TyVar.print |> Printer.print_quantifier)


let print_ty (ty : ty) : PPrint.document =
  let rec print t =
    let print_self = print
    and print_next = print_atom in

    PPrint.group
    @@
    match t with
    | Constr (Arrow (t1, t2)) -> Printer.arrow (print_next t1) (print_self t2)
    | other -> print_next other
  and print_atom = function
    | Constr (Var alpha) -> TyVar.print alpha
    | Constr (Prod ts) -> Printer.product (List.map print ts)
    | Constr (Arrow _) as other -> PPrint.parens (print other)
  in

  print ty


let print_scheme ((quantifiers, ty) : scheme) : PPrint.document =
  Printer.scheme (print_quantifier quantifiers) (print_ty ty)


let print_term : term -> PPrint.document =
  let print_binding x tau = Printer.annot (TeVar.print x) (print_ty tau) in
  let print_let_binding x scheme =
    Printer.let_binding (TeVar.print x) (print_scheme scheme)
  in
  let print_ty_app t = function
    | [] -> t
    | tys -> Printer.ty_app t (List.map print_ty tys)
  in

  let rec print_top t = print_left_open t
  and print_left_open t =
    let print_self = print_left_open
    and print_next = print_app in

    PPrint.group
    @@
    match t with
    | Abs (x, tau, t) ->
      Printer.lambda ~input:(print_binding x tau) ~body:(print_self t)
    | TyAbs (alphas, t) ->
      Printer.big_lambda
        ~input:(List.map TyVar.print alphas)
        ~body:(print_self t)
    | Let (x, scheme, t, u) ->
      Printer.let_
        ~var:(print_let_binding x scheme)
        ~def:(print_top t) ~body:(print_self u)
    | LetTuple (xtaus, t, u) ->
      Printer.let_
        ~var:(Printer.tuple (fun (x, tau) -> print_let_binding x tau) xtaus)
        ~def:(print_top t) ~body:(print_self u)
    | other -> print_next other
  and print_app t =
    let print_self = print_app
    and print_next = print_atom in

    PPrint.group
    @@
    match t with
    | App (t, u) -> Printer.app (print_self t) (print_next u)
    | TyApp (t, tys) -> print_ty_app (print_next t) tys
    | other -> print_next other
  and print_atom t =
    PPrint.group
    @@
    match t with
    | Var x -> TeVar.print x
    | TyApp (t, []) -> print_top t
    | Annot (t, ty) -> Printer.annot (print_top t) (print_ty ty)
    | Tuple ts -> Printer.tuple print_top ts
    | (App _ | Abs _ | Let _ | LetTuple _ | TyApp _ | TyAbs _) as other ->
      PPrint.parens (print_top other)
  in

  print_top
