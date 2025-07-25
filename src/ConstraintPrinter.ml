module Make (T : Utils.Functor) = struct
  open Constraint.Make (T)

  open SatConstraint.Make (T)

  let print_var (v : Constraint.variable) : PPrint.document =
    v |> Constraint.Var.print |> Printer.inference_variable


  let print_sch_var (sch_var : Constraint.scheme_variable) : PPrint.document =
    sch_var |> Constraint.SVar.print |> Printer.scheme_variable


  let print_sat_constraint (c : sat_constraint) : PPrint.document =
    let rec print_top = fun c -> print_left_open c
    and print_left_open =
      let print_self = print_left_open in
      let print_next = print_conj in
      function
      | Loc (_, c) -> print_self c
      | Exist _ as ac ->
        let rec peel = function
          | Loc (_, c) -> peel c
          | Exist (v, s, c) ->
            let binding =
              (print_var v, Option.map (Structure.print print_var) s)
            in
            let bindings, body = peel c in

            (binding :: bindings, body)
          | other -> ([], print_self other)
        in
        let bindings, body = peel ac in
        Printer.exist bindings body
      | Let (bindings, c1, c2) ->
        let print_binding (sch_var, var) =
          (print_sch_var sch_var, print_var var)
        in
        Printer.let_sch
          (List.map print_binding bindings)
          (print_top c1) (print_self c2)
      | other -> print_next other
    and print_conj =
      let print_self = print_conj in
      let print_next = print_atom in

      function
      | Loc (_loc, c) -> print_self c
      | Conj cs -> Printer.conjunction (List.map print_next cs)
      | other -> print_next other
    and print_atom =
      let print_self = print_atom in
      function
      | Loc (_loc, c) -> print_self c
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


  let print_sat_constraint_in_context
    ~(env : PPrint.document) (c : sat_constraint) (k : sat_cont) :
    PPrint.document =
    let rec print_cont ctx = function
      | frame :: rest -> begin
        let ctx =
          match frame with
          | KConj1 c2 -> Printer.conjunction [ ctx; print_sat_constraint c2 ]
          | KConj2 -> Printer.conjunction [ Printer.true_; ctx ]
          | KExist v -> Printer.exist [ (print_var v, None) ] ctx
          | KLet1 (bindings, c2) ->
            let print_binding (sch_var, var) =
              (print_sch_var sch_var, print_var var)
            in
            Printer.let_sch
              (List.map print_binding bindings)
              ctx (print_sat_constraint c2)
          | KLet2 sch_vars ->
            Printer.let_sch_2 (List.map print_sch_var sch_vars) ctx
        in
        print_cont ctx rest
      end
      | [] -> ctx
    in
    print_cont (Printer.hole ~env (print_sat_constraint c)) k


  let print_constraint (type a e) (c : (a, e) Constraint.t) : PPrint.document =
    print_sat_constraint (erase c)


  let print_constraint_in_context
    (type a1 e1 a e)
    ~(env : PPrint.document)
    (c : (a1, e1) Constraint.t)
    (k : (a1, e1, a, e) Constraint.cont) =
    print_sat_constraint_in_context ~env (erase c) (erase_cont k)
end
