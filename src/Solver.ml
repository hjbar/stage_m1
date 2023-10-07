open Constraint

type env = Unif.Env.t ref

let decode (env : env) v = Decode.decode !env v

let rec solve
  : type a e . env -> (a, e) t -> (a, e) result
= fun env -> function
| True -> Ok ()
| False -> Error ()
| Map (c, f) -> Result.map f (solve env c)
| MapErr (c, f) -> Result.map_error f (solve env c)
| Conj (c, d) ->
  begin
    Result.bind (solve env c) @@ fun vc ->
    Result.bind (solve env d) @@ fun vd ->
    Ok (vc, vd)
  end
| Eq (v1, v2) ->
  Unif.unify !env v1 v2
  |> begin function
    | Ok new_env -> env := new_env; Ok ()
    | Error (w1, w2) -> Error (decode env w1, decode env w2)
    end
| Exist (v, s, c) ->
  env := Unif.Env.add v s !env;
  solve env c
| Decode v ->
  Ok (decode env v)
