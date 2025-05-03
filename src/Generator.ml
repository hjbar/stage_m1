module Make(M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make(M)
  module Constraint = Constraint.Make(M)
  module Infer = Infer.Make(M)
  module Solver = Solver.Make(M)

  module TeVarSet = Untyped.Var.Set
  module TyVarSet = STLC.TyVar.Set

  let ret = M.return
  let (let+) s f = M.map f s

  module Env = struct
    type t = {
      tevars : TeVarSet.t;
      tyvars : TyVarSet.t ref;
    }
    let empty () = {
      tevars = TeVarSet.empty;
      tyvars = ref TyVarSet.empty;
    }

    let bind_tevar x env =
      { env with tevars = TeVarSet.add x env.tevars }
  end

  let untyped : Untyped.term =
    let open Untyped in
    let new_var =
      Var.namegen [|"x"; "y"; "z"; "u"; "v"; "w"|]
    in
    let rec gen env =
      let fvars =
        env.Env.tevars
        |> TeVarSet.to_seq
        |> Array.of_seq
        |> M.one_of
      in
      Do (M.delay @@ fun () ->
          let rule_var =
            let+ x = fvars in Var x in
          let rule_app =
            M.return (App(gen env, gen env)) in
          let rule_abs =
            M.delay @@ fun () ->
            let x = new_var () in
            ret (Abs(x, gen (Env.bind_tevar x env))) in
          let rule_let =
            M.delay @@ fun () ->
            let x = new_var () in
            ret (Let(x, gen env, gen (Env.bind_tevar x env))) in
          let tuple_size =
            M.one_of [|2|]
          in
          let rule_tuple =
            M.delay @@ fun () ->
            M.bind tuple_size @@ fun size ->
            let ts = List.init size (fun _ -> gen env) in
            ret (Tuple ts)
          in
          let rule_lettuple =
            M.delay @@ fun () ->
            M.bind tuple_size @@ fun size ->
            let xs = List.init size (fun _ -> new_var ()) in
            let env' = List.fold_right Env.bind_tevar xs env in
            ret (LetTuple(xs, gen env, gen env'))
          in
          M.sum [
            rule_var;
            rule_app;
            rule_abs;
            rule_let;
            rule_tuple;
            rule_lettuple;
          ])
    in gen (Env.empty ())

  let constraint_ : (STLC.term, Infer.err) Constraint.t =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None,
      Infer.has_type
        Untyped.Var.Map.empty
        untyped
        w))

  let typed ~size : STLC.term M.t =
    let open struct
      type env = Solver.env
    end in
    let rec loop : type a e r . fuel:int -> env -> (a, e) Constraint.t -> a M.t =
    fun ~fuel env cstr ->
      if fuel = 0 then M.fail else
      let _logs, env, nf =
        Solver.eval ~log:false env cstr
      in
      match nf with
      | (NRet _ | NErr _) when fuel > 1 -> M.fail
      | NRet v ->
        let decoder = Decode.decode env () in
        M.return (v decoder)
      | NErr _ -> M.fail
      | NDo p ->
        M.bind p (fun cstr ->
          loop ~fuel:(fuel - 1) env cstr
        )
    in
    loop ~fuel:size Unif.Env.empty constraint_
end
