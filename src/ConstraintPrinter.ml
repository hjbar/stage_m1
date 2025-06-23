module Make (T : Utils.Functor) = struct
  open Constraint.Make (T)

  open SatConstraint.Make (T)

  let print_var (v : Constraint.variable) : PPrint.document =
    Printer.inference_variable (Constraint.Var.print v)

  let print_sch_var (sch_var : Constraint.scheme_variable) : PPrint.document =
    Printer.scheme_variable (Constraint.SVar.print sch_var)

  let print_sat_constraint (c : sat_constraint) : PPrint.document =
    let rec print_top = fun c -> print_left_open c
    and print_left_open =
      let print_next = print_conj in

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
        if List.is_empty bindings then body else Printer.exist bindings body
    and print_conj =
      let print_next = print_atom in

      function
      | Conj cs -> Printer.conjunction (List.map print_next cs)
      | Let (sch_var, var, c1, c2) ->
        Printer.let_sch (print_sch_var sch_var) (print_var var) (print_next c1)
          (print_next c2)
      | other -> print_next other
    and print_atom = function
      | Loc (_loc, c) -> print_top c
      | Decode v -> Printer.decode (print_var v)
      | False -> Printer.false_
      | Eq (v1, v2) -> Printer.eq (print_var v1) (print_var v2)
      | Do _ -> Printer.do_
      | DecodeScheme sch_var -> Printer.decode_scheme (print_sch_var sch_var)
      | Instance (sch_var, var) ->
        Printer.instance (print_sch_var sch_var) (print_var var)
      | (Exist _ | Conj _ | Let _) as other -> PPrint.parens (print_top other)
    in
    print_top c

  let print_sat_constraint_in_context ~(env : PPrint.document)
    (c : sat_constraint) (k : sat_cont) : PPrint.document =
    let rec print_cont = function
      | frame :: rest -> begin
        let rest = print_cont rest in

        match frame with
        | KConj1 c2 -> Printer.conjunction [ rest; print_sat_constraint c2 ]
        | KConj2 -> Printer.conjunction [ Printer.true_; rest ]
        | KExist v -> Printer.exist [ (print_var v, None) ] rest
        | KLet1 (s, v, c2) ->
          Printer.let_sch (print_sch_var s) (print_var v) rest
            (print_sat_constraint c2)
        | KLet2 -> Printer.let_sch_2 rest
      end
      | [] -> Printer.hole ~env (print_sat_constraint c)
    in
    print_cont k

  let print_constraint (type a e) (c : (a, e) Constraint.t) : PPrint.document =
    print_sat_constraint (erase c)

  let print_constraint_in_context (type a1 e1 a e) ~(env : PPrint.document)
    (c : (a1, e1) Constraint.t) (k : (a1, e1, a, e) Constraint.cont) =
    print_sat_constraint_in_context ~env (erase c) (erase_cont k)
end
