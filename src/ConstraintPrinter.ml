open Constraint

open struct
  type any_t = Any : ('a, 'e) t -> any_t [@@unboxed]
end

let print_constraint (type a e) (c : (a, e) t) : PPrint.document =
  let rec peel_maps = function
    | Any (Map (c, _)) -> peel_maps (Any c)
    | Any (MapErr (c, _)) -> peel_maps (Any c)
    | Any other -> Any other
  in
  let rec print_top =
    fun c -> print_left_open c

  and print_left_open =
    let _print_self = print_left_open
    and print_next = print_conj in
    fun ac ->
      let rec peel ac = match peel_maps ac with
        | Any (Exist (v, s, c)) ->
          let binding =
            (Constraint.Var.print v,
             Option.map (STLC.Structure.print Constraint.Var.print) s)
          in
          let (bindings, body) = peel (Any c) in
          (binding :: bindings, body)
        | Any other -> ([], print_next (Any other))
      in
      let (bindings, body) = peel ac in
      Printer.exist bindings body

  and print_conj =
    let _print_self = print_conj
    and print_next = print_atom in
    fun ac ->
      let rec peel ac = match peel_maps ac with
        | Any (Conj (c1, c2)) ->
          peel (Any c1) @ peel (Any c2)
        | Any other -> [print_next (Any other)]
      in
      Printer.conjunction (peel ac)

  and print_atom =
    fun ac ->
      match peel_maps ac with
      | Any (Decode v) -> Printer.decode (Constraint.Var.print v)
      | Any True -> Printer.true_
      | Any False -> Printer.false_
      | Any (Map _ | MapErr _) -> assert false (* peel_maps *)
      | Any (Eq (v1, v2)) ->
        Printer.eq
          (Constraint.Var.print v1)
          (Constraint.Var.print v2)
      | Any (Exist _ | Conj _) as other ->
        PPrint.parens (print_top other)

  in print_top (Any c)
