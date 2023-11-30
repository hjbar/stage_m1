let infer (t : Untyped.term) =
  let cst =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None,
      Conj(Generator.has_type Untyped.Var.Map.empty t w,
           Decode w)))
  in
  cst,
  Solver.solve (ref (Unif.Env.empty ())) cst

let print_result =
  let open PPrint in
  function
  | Ok (term, ty) ->
    string "TERM:" ^^ break 1 ^^ group (STLCPrinter.print_term term)
    ^^ hardline
    ^^ string "TYPE:" ^^ break 1 ^^ group (STLCPrinter.print_ty ty)
  | Error (ty1, ty2) ->
    string "ERROR:"
    ^^ break 1
    ^^ group (STLCPrinter.print_ty ty1)
    ^^ break 1
    ^^ string "INCOMPATIBLE WITH"
    ^^ break 1
    ^^ group (STLCPrinter.print_ty ty2)
