module Make (T : Utils.Functor) = struct
  open Untyped.Make (T)

  let print_term : term -> PPrint.document =
    let rec print_top t = print_left_open t
    and print_left_open t =
      let print_self = print_left_open
      and print_next = print_app in

      PPrint.group
      @@
      match t with
      | Abs (x, t) -> Printer.lambda ~input:(Var.print x) ~body:(print_self t)
      | Let (x, t, u) ->
        Printer.let_ ~var:(Var.print x) ~def:(print_top t) ~body:(print_self u)
      | LetTuple (xs, t, u) ->
        Printer.let_
          ~var:(Printer.tuple Var.print xs)
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
      | Var x -> Var.print x
      | Annot (t, ty) -> Printer.annot (print_top t) (FPrinter.print_ty ty)
      | Tuple ts -> Printer.tuple print_top ts
      | (App _ | Abs _ | Let _ | LetTuple _) as other ->
        PPrint.parens (print_top other)
      | Do _p -> Printer.do_
      | Loc (_loc, t) -> print_top t
    in

    print_top
end
