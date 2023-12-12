module Make(T : Utils.Applicative) = struct
  module Untyped = Untyped.Make(T)
  module Constraint = struct
    include Constraint
    include Constraint.Make(T)
  end
  module Generator = Generator.Make(T)
  module Solver = Solver.Make(T)

  let infer ~log (t : Untyped.term) =
    let cst =
      let w = Constraint.Var.fresh "final_type" in
      Constraint.(Exist (w, None,
        Conj(Generator.has_type Untyped.Var.Map.empty t w,
             Decode w)))
    in
    cst,
    Solver.solve ~log (Unif.Env.empty ()) cst
end
