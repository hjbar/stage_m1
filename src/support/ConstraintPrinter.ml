module Make (T : Utils.Functor) = struct
  open Constraint.Make (T)

  open SatConstraint.Make (T)

  let print_var v = Printer.inference_variable (Constraint.Var.print v)

  let print_sch_var sch_var =
    Printer.scheme_variable (Constraint.SVar.print sch_var)

  let print_sat_constraint (c : sat_constraint) : PPrint.document =
    let rec print_top = fun c -> print_left_open c
    and print_left_open =
      let _print_self = print_left_open
      and print_next = print_conj in

      fun ac ->
        let rec peel = function
          | Exist (v, s, c) ->
            let binding =
              (print_var v, Option.map (Structure.print print_var) s)
            in
            let bindings, body = peel c in

            (binding :: bindings, body)
          | other -> ([], print_next other)
        in

        let bindings, body = peel ac in
        Printer.exist bindings body
    and print_conj =
      let _print_self = print_conj
      and print_next = print_atom in

      function
      | Conj cs -> Printer.conjunction @@ List.map print_next cs
      | Let (sch_var, var, c1, c2) ->
        Printer.let_sch (print_sch_var sch_var) (print_var var) (print_next c1)
          (print_next c2)
      | other -> print_next other
    and print_atom = function
      | Loc (_loc, c) -> print_top c
      | Decode v -> Printer.decode @@ print_var v
      | False -> Printer.false_
      | Eq (v1, v2) -> Printer.eq (print_var v1) (print_var v2)
      | Do _ -> Printer.do_
      | DecodeScheme sch_var -> Printer.decode_scheme (print_sch_var sch_var)
      | Instance (sch_var, var) ->
        Printer.instance (print_sch_var sch_var) (print_var var)
      | (Exist _ | Conj _ | Let _) as other -> PPrint.parens @@ print_top other
    in
    print_top c

  let print_constraint (type a e) (c : (a, e) Constraint.t) : PPrint.document =
    print_sat_constraint (erase c)
end
