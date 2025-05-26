let debug = false

let run_test f = if debug then f ()

(* ===== src/support/ConstraintSimplifier.ml ===== *)

(* Show the rank of unif_env's variables and the variable we want to know the rank *)

let debug_what_rank (v : Constraint.variable) (env : Unif.Env.t) : unit =
  let open PPrint in
  run_test @@ fun () ->
  let doc =
    string "===== PRINT ENV RANK ====="
    ^^ break 1 ^^ Unif.Env.debug_rank env
    ^^ string "=========================="
    ^^ break 1
    ^^ string "--------------------------"
    ^^ break 1
    ^^ string "Get rank of variable "
    ^^ Constraint.Var.print v ^^ break 1
    ^^ string "--------------------------"
    ^^ break 1
  in
  PPrint.ToChannel.pretty 80. 1 stdout doc

(* Show the assoc between variable and repr in unif_env and the variable we want to know the repr *)

let debug_what_repr_assoc (v : Constraint.variable) (env : Unif.Env.t) : unit =
  let open PPrint in
  run_test @@ fun () ->
  let doc =
    string "===== PRINT ENV REPR ASSOC ====="
    ^^ break 1
    ^^ Unif.Env.debug_repr_assoc env
    ^^ string "================================"
    ^^ break 1
    ^^ string "--------------------------------"
    ^^ break 1 ^^ string "Test variable " ^^ Constraint.Var.print v ^^ break 1
    ^^ string "--------------------------------"
    ^^ break 1
  in
  PPrint.ToChannel.pretty 80. 1 stdout doc
