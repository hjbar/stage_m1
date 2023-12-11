open Constraint

type env = Unif.Env.t

let solve ~log env c0 =
  let env = ref env in
  let decode v = Decode.decode !env v in
  let logs = Queue.create () in
  let log =
    if not log then ignore
    else begin
      let c0_erased = SatConstraint.erase c0 in
      fun () ->
        let doc =
          c0_erased
          |> ConstraintSimplifier.simplify !env
          |> ConstraintPrinter.print_sat_constraint
        in
        Queue.add doc logs
    end
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
      let result = Unif.unify !env v1 v2 in
      begin match result with
        | Ok new_env ->
          env := new_env;
          log ();
          Ok ()
        | Error (w1, w2) ->
          Error (decode w1, decode w2)
      end
    | Exist (v, s, c) ->
      env := Unif.Env.add v s !env;
      log ();
      solve c
    | Decode v ->
      Ok (decode v)
  in
  log ();
  let result = solve c0 in
  let logs = logs |> Queue.to_seq |> List.of_seq in
  logs, result
