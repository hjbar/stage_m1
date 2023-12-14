module Make (T : Utils.Functor) = struct
  open Constraint.Make(T)
  module SatConstraint = SatConstraint.Make(T)
  module ConstraintSimplifier = ConstraintSimplifier.Make(T)
  module ConstraintPrinter = ConstraintPrinter.Make(T)

  type env = Unif.Env.t
  type log = PPrint.document list

  let make_logger c0 =
    let logs = Queue.create () in
    let c0_erased = SatConstraint.erase c0 in
    let add_to_log env =
      let doc =
        c0_erased
        |> ConstraintSimplifier.simplify env
        |> ConstraintPrinter.print_sat_constraint
      in
      Queue.add doc logs
    in
    let get_log () =
      logs |> Queue.to_seq |> List.of_seq
    in
    add_to_log, get_log
             
  let eval ~log (env : env) c0 =
    let add_to_log, get_log =
      if log then make_logger c0
      else ignore, (fun _ -> [])
    in
(*sujet
    (* We recommend calling the function [add_to_log] above
       whenever you get an updated environment. Then call
       [get_log] at the end to get a list of log message.

       $ dune exec -- minihell --log-solver foo.test

       will show a log that will let you see the evolution
       of your input constraint (after simplification) as
       the solver progresses, which is useful for debugging.

       (You can also tweak this code temporarily to print stuff on
       stderr right away if you need dirtier ways to debug.)
    *)
    Utils.not_yet "Solver.eval" (env, c0, add_to_log, get_log)
/sujet*)
(*corrige*)
    let env = ref env in
    let decode v = Decode.decode !env v in
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
            add_to_log !env;
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
        add_to_log !env;
        begin match eval c with
        | Ret v -> Ret v
        | Err e -> Err e
        | c -> Exist (x, s, c)
        end
      | Decode v -> Ret (fun sol -> sol v)
      | Do p ->
        Do p
    in
    add_to_log !env;
    let result = eval c0 in
    get_log (), !env, result
(*/corrige*)

  let solve ~log env c =
    let logs, env, result = eval ~log env c in
    let sol = Decode.decode env in
(*sujet
    Utils.not_yet "Solver.solve" (logs, env, result, sol)
/sujet*)
(*corrige*)
    logs,
    match result with
    | Ret v -> Ok (v sol)
    | Err e -> Error e
    | _other ->
      failwith "[eval] did not return a normal form!"
(*/corrige*)
end
