let infer (t : Untyped.term) =
  let cst =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None,
      Conj(Generator.has_type Untyped.Var.Map.empty t w,
           Decode w)))
  in
  cst,
  Solver.solve (ref (Unif.Env.empty ())) cst
