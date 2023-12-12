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
  let rec eval
    : type a e . (a, e) t -> (a, e) t
  = function
    | Ret v -> Ret v
    | Err e -> Err e
    | Map (c, f) ->
      begin match eval c with
      | Ret v -> Ret (f v)
      | Err e -> Err e
      | c -> Map (c, f)
      end
    | MapErr (c, f) ->
      begin match eval c with
      | Ret v -> Ret v
      | Err e -> Err (f e)
      | c -> MapErr (c, f)
      end
    | Conj (c, d) ->
      begin match eval c with
        | Err e -> Err e
        | c ->
        match c, eval d with
        | _, Err e -> Err e
        | Ret v, Ret w -> Ret (v, w)
        | c, d -> Conj (c, d)
      end
    | Eq (x1, x2) ->
      let result = Unif.unify !env x1 x2 in
      begin match result with
        | Ok new_env ->
          env := new_env;
          log ();
          Ret ()
        | Error (y1, y2) ->
          Err (decode y1, decode y2)
      end
    | Exist (x, s, c) ->
      env := Unif.Env.add x s !env;
      log ();
      begin match eval c with
      | Ret v -> Ret v
      | Err e -> Err e
      | c -> Exist (x, s, c)
      end
    | Decode v ->
      Ret (decode v)
  in
  let solve c =
    match eval c with
    | Ret v -> Ok v
    | Err e -> Error e
    | _other ->
      failwith "[eval] did not return a normal form!"
  in
  log ();
  let result = solve c0 in
  let logs = logs |> Queue.to_seq |> List.of_seq in
  logs, result
