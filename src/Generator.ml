module Make (M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make (M)
  module Constraint = Constraint.Make (M)
  module Infer = Infer.Make (M)
  module Solver = Solver.Make (M)
  module TeVarSet = Untyped.Var.Set
  module TyVarSet = STLC.TyVar.Set

  (* Helpers *)

  let make_do t = Untyped.Do t

  let ( let+ ) s f = M.map f s

  module Env = struct
    type t =
      { tevars : TeVarSet.t
      ; tyvars : TyVarSet.t ref
      }

    let empty () = { tevars = TeVarSet.empty; tyvars = ref TyVarSet.empty }

    let bind_tevar x env = { env with tevars = TeVarSet.add x env.tevars }
  end

  let untyped : Untyped.term =
    let open Untyped in
    let new_var = Var.namegen [| "x"; "y"; "z"; "u"; "v"; "w" |] in

    let rec gen env =
      let fvars =
        env.Env.tevars |> TeVarSet.to_seq |> Array.of_seq |> M.one_of
      in

      make_do @@ M.delay
      @@ fun () ->
      let rule_var =
        let+ x = fvars in
        Var x
      in

      let rule_app = M.return @@ App (gen env, gen env) in

      let rule_abs =
        M.delay @@ fun () ->
        let x = new_var () in
        let env = Env.bind_tevar x env in

        M.return @@ Abs (x, gen env)
      in

      let rule_let =
        M.delay @@ fun () ->
        let x = new_var () in
        let inner_env = Env.bind_tevar x env in

        M.return @@ Let (x, gen env, gen inner_env)
      in

      let tuple_size = M.one_of [| 2 |] in

      let rule_tuple =
        M.delay @@ fun () ->
        M.bind tuple_size @@ fun size ->
        let ts = List.init size @@ fun _ -> gen env in
        M.return @@ Tuple ts
      in

      let rule_lettuple =
        M.delay @@ fun () ->
        M.bind tuple_size @@ fun size ->
        let xs = List.init size @@ fun _ -> new_var () in
        let inner_env = List.fold_right Env.bind_tevar xs env in

        M.return @@ LetTuple (xs, gen env, gen inner_env)
      in

      M.sum
        [ rule_var; rule_app; rule_abs; rule_let; rule_tuple; rule_lettuple ]
    in

    gen (Env.empty ())

  let constraint_ : (STLC.term, Infer.err) Constraint.t =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None, Infer.has_type Untyped.Var.Map.empty untyped w))

  (* This definition uses [constraint_] to generate well-typed terms.
     An informal description of a possible way to do this is described
     in the README, Section "Two or three effect instances", where
     the function is valled [gen]:

     > it is possible to define a function
     >
     >     val gen : size:int -> ('a, 'e) constraint -> ('a, 'e) result M.t
     >
     > on top of `eval`, that returns all the results that can be reached by
     > expanding `Do` nodes using `M.bind`, recursively, exactly `size`
     > times. (Another natural choice would be to generate all the terms that
     > can be reached by expanding `Do` nodes *at most* `size` times, but
     > this typically gives a worse generator.)
  *)
  let typed ~size : STLC.term M.t =
    let open struct
      type env = Solver.env
    end in
    let rec loop : type a e r. fuel:int -> env -> (a, e) Constraint.t -> a M.t =
     fun ~fuel env cstr ->
      if fuel = -1 then M.fail
      else
        let _logs, env, nf = Solver.eval ~log:false env cstr in

        match nf with
        | NRet v when fuel = 0 -> M.return @@ v (Decode.decode env ())
        | NDo p when fuel > 0 -> M.bind p @@ loop ~fuel:(fuel - 1) env
        | _ -> M.fail
    in

    loop ~fuel:size Unif.Env.empty constraint_
end
