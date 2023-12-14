module Make(M : Utils.MonadPlus) = struct
  let ret = M.return
  let (let+) s f = M.map f s
  
  module Untyped = Untyped.Make(M)
  
  module TeVarSet = Untyped.Var.Set
  module TyVarSet = STLC.TyVar.Set
  
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
      Utils.namegen Var.fresh
        [|"x"; "y"; "z"; "u"; "v"; "w"|]
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
  
  module Constraint = Constraint.Make(M)
  module Infer = Infer.Make(M)
  
  let constraint_ : (STLC.term, Infer.err) Constraint.t =
    let w = Constraint.Var.fresh "final_type" in
    Constraint.(Exist (w, None,
      Infer.has_type
        Untyped.Var.Map.empty
        untyped
        w))
      
  module Solver = Solver.Make(M)
  module ConstraintPrinter = ConstraintPrinter.Make(M)
  
  let typed ~depth : STLC.term M.t =
    let open struct
      type env = Solver.env
    end in
    let rec loop : type a e r . fuel:int -> env -> (a, e) Constraint.t -> a M.t =
    fun ~fuel env cstr ->
      if fuel = 0 then M.fail else
      let _logs, env, cstr =
        Solver.eval ~log:false env cstr
      in
      let open Constraint in
      let ( let+ ) g f = M.map f g in
      let ( let++ ) r f =
        match r with
        | Ok g -> Ok (M.map f g)
        | Error c -> Error (f c)
      in
      let rec expand : type a e . (a, e) Constraint.t -> ((a, e) Constraint.t M.t, (a, e) Constraint.t) result =
        function
        | Ret v -> Error (Ret v)
        | Err e -> Error (Err e)
        | Map (c, f) ->
          let++ c = expand c in Map (c, f)
        | MapErr (c, f) ->
          let++ c = expand c in MapErr (c, f)
        | Conj (c1, c2) ->
          begin match expand c1 with
          | Ok c1 ->
            Ok (let+ c1 in Conj (c1, c2))
          | Error c1 ->
            let++ c2 = expand c2 in Conj (c1, c2)
          end
        | Exist (x, s, c) ->
          let++ c = expand c in
          Exist (x, s, c)
        | Eq _ -> failwith "not a normal form"
        | Decode _ -> failwith "not a normal form"
        | Do p ->
          Ok p
      in
      match expand cstr with
      | Error cstr ->
        (* We have reached a normal form. If [fuel = 1],
           we are happy with that. If [fuel > 1] we
           were expecting a larger program, so we fail. *)
        if fuel > 1 then M.fail
        else begin match Solver.solve ~log:false env cstr |> snd with
          | Ok v -> M.return v
          | Error _ -> M.fail
        end
      | Ok cstr ->
        M.bind cstr (fun cstr ->
          loop ~fuel:(fuel - 1) env cstr
        )
    in
    loop ~fuel:depth (Unif.Env.empty ()) constraint_

end
