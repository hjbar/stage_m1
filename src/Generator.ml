module Make(M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make(M)
  module Constraint = Constraint.Make(M)
  module Infer = Infer.Make(M)
  module Solver = Solver.Make(M)

  (* just in case... *)
  module TeVar = Untyped.Var
  module TyVar = STLC.TyVar


let untyped : Untyped.term =
  (* This definition is *not* a good solution,
     but it could give you a flavor of possible definitions. *)
  let rec gen () : Untyped.term =
    let open Untyped in
    Do (M.delay @@ fun () ->
      M.sum [
        M.return (App(gen (), gen ())); (* try to generate applications... *)
        M.delay (Utils.not_yet "Generator.untyped"); (* ... or fail *)
      ]
    )
  in gen ()

let constraint_ : (STLC.term, Infer.err) Constraint.t =
  let w = Constraint.Var.fresh "final_type" in
  Constraint.(Exist (w, None,
    Infer.has_type
      Untyped.Var.Map.empty
      untyped
      w))

let typed ~depth =
  (* This definition uses [constraint_] to generate well-typed terms.
     An informal description of a possible way to do this is described
     in the README, Section "Two or three effect instances", where
     the function is valled [gen]:

     > it is possible to define a function
     >
     >     val gen : depth:int -> ('a, 'e) constraint -> ('a, 'e) result M.t
     >
     > on top of `eval`, that returns all the results that can be reached by
     > expanding `Do` nodes using `M.bind`, recursively, exactly `depth`
     > times. (Another natural choice would be to generate all the terms that
     > can be reached by expanding `Do` nodes *at most* `depth` times, but
     > this typically gives a worse generator.)
  *)
  Utils.not_yet "Generator.typed" depth

end
