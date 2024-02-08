let () =
  let module Constraint = Constraint.Make(MSeq) in
  let module Solver = Solver.Make(MSeq) in
  let c : _ Constraint.t =
    let open Constraint in
    let a = Var.fresh "a" in 
    let b = Var.fresh "b" in 
    Exist (a, None, 
    Exist (b, None, 
      Conj ( 
        Eq (a, b),
        Do MSeq.fail )))
  in
  let (logs, _env, _result) =
    Solver.eval ~log:true Unif.Env.empty c in
  PPrint.(separate hardline logs)
  |> Utils.string_of_doc
  |> print_endline
  |> print_newline

