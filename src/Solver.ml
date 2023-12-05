open Constraint

type env = Unif.Env.t

let solve env c0 =
  let env = ref env in
  let decode v = Decode.decode !env v in
  let c0_erased = SatConstraint.erase c0 in
  let log () =
    c0_erased
    |> ConstraintSimplifier.simplify !env
    |> ConstraintPrinter.print_sat_constraint
    |> Printer.string_of_doc
    |> prerr_endline
  in
  let rec solve
    : type a e . (a, e) t -> (a, e) result
  = function
    | True -> Ok ()
    | False -> Error ()
    | Map (c, f) -> Result.map f (solve c)
    | MapErr (c, f) -> Result.map_error f (solve c)
    | Conj (c, d) ->
      begin
        Result.bind (solve c) @@ fun vc ->
        Result.bind (solve d) @@ fun vd ->
        Ok (vc, vd)
      end
    | Eq (v1, v2) ->
      Unif.unify !env v1 v2
      |> begin function
        | Ok new_env ->
          env := new_env;
          log ();
          Ok ()
        | Error (w1, w2) ->
          Error (decode w1, decode w2)
      end
    | Exist (v, s, c) ->
      env := Unif.Env.add v s !env;
      solve c
    | Decode v ->
      Ok (decode v)
  in
  log ();
  Fun.protect ~finally:log (fun () ->
    solve c0
  )
