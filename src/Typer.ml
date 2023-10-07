let infer (t : Untyped.term) : (STLC.term * STLC.ty, STLC.ty Utils.clash) result =
  let cst =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None,
      Conj(Generator.has_type Untyped.Var.Map.empty t w,
           Decode w)))
  in
  Solver.solve (ref (Unif.Env.empty ())) cst
