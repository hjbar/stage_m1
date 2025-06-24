module Make (M : Utils.MonadPlus) = struct
  module Untyped = Untyped.Make (M)
  module Constraint = Constraint.Make (M)
  module Infer = Infer.Make (M)
  module Solver = Solver.Make (M)
  module TeVar = Untyped.Var
  module TeVarSet = TeVar.Set
  module TyVarSet = STLC.TyVar.Set

  let ret = M.return

  let ( let+ ) s f = M.map f s

  module Env = struct
    type t = {
      tevars : TeVarSet.t;
      tyvars : TyVarSet.t ref;
    }

    let empty () = { tevars = TeVarSet.empty; tyvars = ref TyVarSet.empty }

    let bind_tevar x env = { env with tevars = TeVarSet.add x env.tevars }
  end

  let untyped_gasche : Untyped.term =
    let open Untyped in
    let new_var = Var.namegen [| "x"; "y"; "z"; "u"; "v"; "w" |] in
    let rec gen env =
      let fvars =
        env.Env.tevars |> TeVarSet.to_seq |> Array.of_seq |> M.one_of
      in
      Do
        ( M.delay @@ fun () ->
          let rule_var =
            let+ x = fvars in
            Var x
          in
          let rule_app = M.return (App (gen env, gen env)) in
          let rule_abs =
            M.delay @@ fun () ->
            let x = new_var () in
            ret (Abs (x, gen (Env.bind_tevar x env)))
          in
          let rule_let =
            M.delay @@ fun () ->
            let x = new_var () in
            ret (Let (x, gen env, gen (Env.bind_tevar x env)))
          in
          let tuple_size = M.one_of [| 2 |] in
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
            ret (LetTuple (xs, gen env, gen env'))
          in
          M.sum
            [
              rule_var; rule_app; rule_abs; rule_let; rule_tuple; rule_lettuple;
            ] )
    in
    gen (Env.empty ())


  let untyped_vanille : Untyped.term =
    let rec gen (fv : TeVar.t list) : Untyped.term =
      (* The partial terms constructed at this iteration will occur in distinct
         complete terms. Thus it's fine if we generate fresh variables once and
         use them several times in different terms, it cannot produce collisions. *)
      let x = TeVar.fresh "x" in
      let y = TeVar.fresh "y" in
      Untyped.(
        Do
          ( M.delay @@ fun () ->
            M.sum
              [
                (* One of the existing available variables *)
                M.sum (fv |> List.map (fun v -> M.return (Var v)));
                (* or any term constructor recursively filled in. *)
                M.one_of
                  [|
                    App (gen fv, gen fv);
                    Abs (x, gen (x :: fv));
                    Let (x, gen fv, gen (x :: fv));
                    LetTuple ([ x; y ], gen fv, gen (x :: y :: fv));
                    Tuple [ gen fv; gen fv ];
                  |];
              ] ) )
    in
    gen []


  let rec cut_size ~size (term : Untyped.term) : Untyped.term =
    let un ~size t f : Untyped.term =
      let size = size - 1 in
      if size < 1 then Do M.fail else f (cut_size ~size t)
    in
    let bin ~size ta tb f : Untyped.term =
      let size = size - 1 in
      if size < 2 then Do M.fail
      else
        Do
          ( M.sum
          @@ List.init (size - 1) (fun idx ->
               (* { i, j | i > 0, j > 0, i+j = size } *)
               let i = idx + 1 in
               let j = size - i in
               let ta' = cut_size ~size:i ta in
               let tb' = cut_size ~size:j tb in
               M.return (f ta' tb') ) )
    in
    if size <= 0 then Do M.fail
    else
      match term with
      | Var _ -> if size = 1 then term else Do M.fail
      | App (t, u) -> bin ~size t u (fun t' u' -> App (t', u'))
      | Abs (x, t) -> un ~size t (fun t' -> Abs (x, t'))
      | Let (x, t, u) -> bin ~size t u (fun t' u' -> Let (x, t', u'))
      | Tuple ts ->
        let t, u =
          match ts with
          | [ t; u ] -> (t, u)
          | _ -> assert false
        in
        bin ~size t u (fun t' u' -> Tuple [ t'; u' ])
      | LetTuple (xs, t, u) ->
        bin ~size t u (fun t' u' -> LetTuple (xs, t', u'))
      | Annot (t, ty) -> un ~size t (fun t' -> Annot (t', ty))
      | Do m -> Do (M.map (cut_size ~size) m)
      | Loc (loc, t) -> un ~size t @@ fun t' -> Loc (loc, t')


  let constraint_ untyped : (STLC.term * STLC.ty, Infer.err) Constraint.t =
    Infer.exist_wrapper untyped


  let typed_cut_early ~size untyped : (STLC.term * STLC.ty) M.t =
    let open struct type env = Solver.env end in
    let rec loop : type a e. env -> (a, e) Constraint.t -> a M.t =
     fun env cstr ->
      let _logs, env, nf = Solver.eval ~log:false env cstr in
      match nf with
      | NRet v ->
        let decoder = Decode.decode env () in
        M.return (v decoder)
      | NErr _ -> M.fail
      | NDo p -> M.bind p (fun cstr -> loop env cstr)
    in
    untyped |> cut_size ~size |> constraint_ |> loop Unif.Env.empty


  let typed_cut_late ~size untyped : (STLC.term * STLC.ty) M.t =
    let open struct type env = Solver.env end in
    let rec loop : type a e. fuel:int -> env -> (a, e) Constraint.t -> a M.t =
     fun ~fuel env cstr ->
      if fuel < 0 then M.fail
      else
        let _logs, env, nf = Solver.eval ~log:false env cstr in
        match nf with
        | (NRet _ | NErr _) when fuel > 0 -> M.fail
        | NRet v ->
          let decoder = Decode.decode env () in
          M.return (v decoder)
        | NErr _ -> M.fail
        | NDo p -> M.bind p (fun cstr -> loop ~fuel:(fuel - 1) env cstr)
    in
    loop ~fuel:size Unif.Env.empty (constraint_ untyped)
end
