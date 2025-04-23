open STLC

let print_ty : ty -> PPrint.document =
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
  print

let print_term : term -> PPrint.document =
  let print_binding x tau = Printer.annot (TeVar.print x) (print_ty tau) in
  let rec print_top t = print_left_open t
  and print_left_open t =
    let print_self = print_left_open
    and print_next = print_app in
    PPrint.group
    @@
    match t with
    | Abs (x, tau, t) ->
      Printer.lambda ~input:(print_binding x tau) ~body:(print_self t)
    | Let (x, tau, t, u) ->
      Printer.let_ ~var:(print_binding x tau) ~def:(print_top t)
        ~body:(print_self u)
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
    | Annot (t, ty) -> Printer.annot (print_top t) (print_ty ty)
    | Tuple ts -> Printer.tuple print_top ts
    | (App _ | Abs _ | Let _ | LetTuple _) as other ->
      PPrint.parens (print_top other)
  in
  print_top
