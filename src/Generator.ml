module Make(M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make(M)
  module Constraint = Constraint.Make(M)
  module Infer = Infer.Make(M)
  module Solver = Solver.Make(M)

  (* just in case... *)
  module TeVar = Untyped.Var
  module TyVar = STLC.TyVar

let untyped : Untyped.term =
  Do (M.delay (Utils.not_yet "Generator.untyped"))

let constraint_ : (STLC.term, Infer.err) Constraint.t =
  Do (M.delay (Utils.not_yet "Generator.constraint_"))

let typed ~depth =
  Utils.not_yet "Generator.typed" depth

end
