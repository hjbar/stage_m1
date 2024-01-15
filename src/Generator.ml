module Make(M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make(M)
  module Constraint = Constraint.Make(M)
  module Infer = Infer.Make(M)
  module Solver = Solver.Make(M)

  (* just in case... *)
  module TeVar = Untyped.Var
  module TyVar = STLC.TyVar


let untyped : Untyped.term =
  let rec gen (fv : TeVar.t list) : Untyped.term =
    (* The partial terms constructed at this iteration will occur in distinct
       complete terms. Thus it's fine if we generate fresh variables once and
       use them several times in different terms, it cannot produce collisions. *)
    let x = TeVar.fresh "x" in
    let y = TeVar.fresh "y" in
    Untyped.(Do (M.delay @@ fun () ->
      M.sum [
        (* One of the existing available variables *)
        M.sum (fv |> List.map (fun v -> M.return (Var v)));
        (* or any term constructor recursively filled in. *)
        M.one_of [|
            App (gen fv, gen fv);
            Abs (x, gen (x::fv));
            Let (x, gen fv, gen (x::fv));
            LetTuple ([x; y], gen fv, gen (x::y::fv));
            Tuple [gen fv; gen fv]
        |]
      ]
    ))
  in gen []

let constraint_ : (STLC.term, Infer.err) Constraint.t =
  let w = Constraint.Var.fresh "final_type" in
  Constraint.(Exist (w, None,
    Infer.has_type
      Untyped.Var.Map.empty
      untyped
      w))

let typed ~(depth:int) : STLC.term M.t =
    let rec expand (depth:int) (env:Unif.Env.t) (constr: (STLC.term, Infer.err) Constraint.t) : STLC.term M.t =
        (* [Solver.eval] will push [NDo] nodes towards the root.
           depending on the depth and the status we determine when to stop. *)
        let (_log, env', res) = Solver.eval ~log:false env constr in
        match res with
            (* Do-free term at the correct depth: this is a solution *)
            | NRet map when depth = 1 -> M.return (map (Decode.decode env'))
            (* Do-expandable term with insufficient depth: we can expand further *)
            | NDo ts when depth > 1 -> M.bind ts (expand (depth-1) env')
            (* Anything else (terms that are
               - ill-typed, or
               - do-free and too shallow, or
               - incompletely expanded) are dropped *)
            | _ -> M.fail
    in expand depth Unif.Env.empty constraint_
end
