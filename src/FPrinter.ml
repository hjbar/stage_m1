open F

let print_quantifier (quantifiers : TyVar.t list) : PPrint.document =
  PPrint.group
    (quantifiers |> List.map F.TyVar.print |> Printer.print_quantifier)

let print_ty (ty : ty) : PPrint.document =
  let rec print t =
    let print_self = print
    and print_next = print_atom in

    match t with
    | Constr (Arrow (t1, t2)) -> Printer.arrow (print_next t1) (print_self t2)
    | other -> print_next other
  and print_atom = function
    | Constr (Var alpha) -> TyVar.print alpha
    | Constr (Prod ts) -> Printer.product (List.map print ts)
    | Constr (Arrow _) as other -> PPrint.parens (print other)
  in

  PPrint.group (print ty)

let print_scheme ((quantifiers, ty) : scheme) : PPrint.document =
  Printer.scheme (print_quantifier quantifiers) (print_ty ty)

let print_term : term -> PPrint.document =
  let print_binding x tau = Printer.annot (TeVar.print x) (print_ty tau) in
  let print_let_binding x (quantifiers, ty) =
    Printer.let_binding
      (List.map F.TyVar.print quantifiers)
      (TeVar.print x) (print_ty ty)
  in
  let print_var_app x = function
    | [] -> TeVar.print x
    | tys -> Printer.var_app (TeVar.print x) (List.map print_ty tys)
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
    | Let (x, scheme, t, u) ->
      Printer.let_
        ~var:(print_let_binding x scheme)
        ~def:(print_top t) ~body:(print_self u)
    | LetTuple (xtaus, t, u) ->
      Printer.let_
        ~var:(Printer.tuple (fun (x, tau) -> print_binding x tau) xtaus)
        ~def:(print_top t) ~body:(print_self u)
    | other -> print_next other
  and print_app t =
    let print_self = print_app
    and print_next = print_atom in

    PPrint.group
    @@
    match t with
    | App (t, u) -> Printer.app (print_self t) (print_next u)
    | other -> print_next other
  and print_atom t =
    PPrint.group
    @@
    match t with
    | Var x -> TeVar.print x
    | VarApp (x, tys) -> print_var_app x tys
    | Annot (t, ty) -> Printer.annot (print_top t) (print_ty ty)
    | Tuple ts -> Printer.tuple print_top ts
    | (App _ | Abs _ | Let _ | LetTuple _) as other ->
      PPrint.parens (print_top other)
  in

  print_top
