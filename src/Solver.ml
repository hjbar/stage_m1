module Make (T : Utils.Functor) = struct
  open Constraint.Make(T)
  module SatConstraint = SatConstraint.Make(T)
  module ConstraintSimplifier = ConstraintSimplifier.Make(T)
  module ConstraintPrinter = ConstraintPrinter.Make(T)

  type env = Unif.Env.t
  type log = PPrint.document list
             
  let eval ~log (env : env) c0 =
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
        | Ret v -> Ret (fun sol -> f (v sol))
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
          | Ret v, Ret w -> Ret (fun sol -> (v sol, w sol))
          | c, d -> Conj (c, d)
        end
      | Eq (x1, x2) ->
        let result = Unif.unify !env x1 x2 in
        begin match result with
          | Ok new_env ->
            env := new_env;
            log ();
            Ret (fun _sol -> ())
          | Error (Cycle cy) ->
            Err (Cycle cy)
          | Error (Clash (y1, y2)) ->
            Err (Clash (decode y1, decode y2))
        end
      | Exist (x, s, c) ->
        (* Our solver may re-enter existentials
           that it has already traversed. In this
           case we do not want to re-bind them in the
           environment, but reuse the previous binding
           which accumulated information through unifications. *)
        if not (Unif.Env.mem x !env) then
          env := Unif.Env.add x s !env;
        log ();
        begin match eval c with
        | Ret v -> Ret v
        | Err e -> Err e
        | c -> Exist (x, s, c)
        end
      | Decode v -> Ret (fun sol -> sol v)
      | Do p ->
        Do p
    in
    let logs () = logs |> Queue.to_seq |> List.of_seq in
    log ();
    let result = eval c0 in
    logs (), !env, result

  let solve ~log env c =
    let logs, env, result = eval ~log env c in
    let sol = Decode.decode env in
    logs,
    match result with
    | Ret v -> Ok (v sol)
    | Err e -> Error e
    | _other ->
      failwith "[eval] did not return a normal form!"
end
