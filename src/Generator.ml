module Make (M : Utils.MonadPlus) = struct
  (* Instantiate modules *)

  module Untyped = Untyped.Make (M)
  module Constraint = Constraint.Make (M)
  module Infer = Infer.Make (M)
  module Solver = Solver.Make (M)
  module TeVar = Untyped.Var
  module TeVarSet = TeVar.Set
  module TyVarSet = STLC.TyVar.Set

  (* Define untyped terms for generation *)

  let do_ p = Untyped.Do p

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

      do_ @@ M.delay
      @@ fun () ->
      let rule_var =
        let+ x = fvars in
        Var x
      in

      let rule_app = M.return @@ App (gen env, gen env) in

      let rule_abs =
        M.delay @@ fun () ->
        let x = new_var () in
        M.return @@ Abs (x, gen (Env.bind_tevar x env))
      in

      let rule_let =
        M.delay @@ fun () ->
        let x = new_var () in
        M.return @@ Let (x, gen env, gen (Env.bind_tevar x env))
      in

      let tuple_size = M.one_of [| 2 |] in

      let rule_tuple =
        M.delay @@ fun () ->
        M.bind tuple_size @@ fun size ->
        let ts = List.init size (fun _ -> gen env) in
        M.return @@ Tuple ts
      in

      let rule_lettuple =
        M.delay @@ fun () ->
        M.bind tuple_size @@ fun size ->
        let xs = List.init size (fun _ -> new_var ()) in
        M.return
        @@ LetTuple (xs, gen env, gen (List.fold_right Env.bind_tevar xs env))
      in

      M.sum
        [ rule_var; rule_app; rule_abs; rule_let; rule_tuple; rule_lettuple ]
    in

    gen (Env.empty ())


  let untyped_vanille : Untyped.term =
    let rec gen fv =
      let open Untyped in
      (* The partial terms constructed at this iteration will occur in distinct
         complete terms. Thus it's fine if we generate fresh variables once and
         use them several times in different terms, it cannot produce collisions. *)
      let x = TeVar.fresh "x" in
      let y = TeVar.fresh "y" in

      do_ @@ M.delay
      @@ fun () ->
      M.sum
        [
          (* One of the existing available variables *)
          M.sum @@ List.map (fun v -> M.return @@ Var v) fv;
          (* or any term constructor recursively filled in. *)
          M.one_of
            [|
              App (gen fv, gen fv);
              Abs (x, gen (x :: fv));
              Let (x, gen fv, gen (x :: fv));
              Tuple [ gen fv; gen fv ];
              LetTuple ([ x; y ], gen fv, gen (x :: y :: fv));
            |];
        ]
    in

    gen []


  (* Generate a monadic untyped term of size [~size]
     from a descriptive monadic untyped term *)

  let rec cut_size ~size (term : Untyped.term) : Untyped.term =
    let un ~size t f : Untyped.term =
      let size = size - 1 in

      if size < 1 then Do M.fail else f (cut_size ~size t)
    in

    let bin ~size ta tb f : Untyped.term =
      let size = size - 1 in

      if size < 2 then Do M.fail
      else
        do_
        @@ M.sum
        @@ List.init (size - 1)
             begin
               fun idx ->
                 (* { i, j | i > 0, j > 0, i+j = size } *)
                 let i = idx + 1 in
                 let j = size - i in

                 let ta' = cut_size ~size:i ta in
                 let tb' = cut_size ~size:j tb in

                 M.return @@ f ta' tb'
             end
    in

    if size <= 0 then Do M.fail
    else
      match term with
      | Var _ -> if size = 1 then term else Do M.fail
      | App (t, u) -> bin ~size t u @@ fun t' u' -> App (t', u')
      | Abs (x, t) -> un ~size t @@ fun t' -> Abs (x, t')
      | Let (x, t, u) -> bin ~size t u @@ fun t' u' -> Let (x, t', u')
      | Tuple ts ->
        let t, u =
          match ts with
          | [ t; u ] -> (t, u)
          | _ -> assert false
        in
        bin ~size t u @@ fun t' u' -> Tuple [ t'; u' ]
      | LetTuple (xs, t, u) ->
        bin ~size t u @@ fun t' u' -> LetTuple (xs, t', u')
      | Annot (t, ty) -> un ~size t @@ fun t' -> Annot (t', ty)
      | Do m -> Do (M.map (cut_size ~size) m)
      | Loc (loc, t) -> un ~size t @@ fun t' -> Loc (loc, t')


  (* Generate the initial constraint to type a given untyped term *)

  let constraint_ untyped : (STLC.term * STLC.ty, Infer.err) Constraint.t =
    Infer.exist_wrapper untyped


  (* Generate a typed term of a given size from a monadic untyped term *)

  let typed_cut_early ~size untyped : (STLC.term * STLC.ty) M.t =
    let rec loop : type a e. (a, e) Solver.normal_constraint -> a M.t = function
      | NRet (env, v) -> M.return @@ v (Decode.decode env ())
      | NErr _ -> M.fail
      | NDo m -> M.bind m loop
    in

    let constraint_ = untyped |> cut_size ~size |> constraint_ in
    let nf = Solver.eval ~log:false (Solver.Env.empty ()) constraint_ in
    loop nf


  let typed_cut_late ~size untyped : (STLC.term * STLC.ty) M.t =
    let rec loop : type a e.
      fuel:int -> (a, e) Solver.normal_constraint -> a M.t =
     fun ~fuel nf ->
      if fuel < 0 then M.fail
      else
        match nf with
        | (NRet _ | NErr _) when fuel > 0 -> M.fail
        | NRet (env, v) -> M.return @@ v (Decode.decode env ())
        | NErr _ -> M.fail
        | NDo m -> M.bind m (loop ~fuel:(fuel - 1))
    in

    let constraint_ = constraint_ untyped in
    let nf = Solver.eval ~log:false (Solver.Env.empty ()) constraint_ in
    loop ~fuel:size nf
end
