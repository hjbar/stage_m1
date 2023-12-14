(*
module RandGen = struct
  type 'a t = 'a Seq.t
  let map = Seq.map

  let pure x = Seq.return x
  let pair sa sb = Seq.product sa sb
     
  let sum seqs = seqs |> List.to_seq |> Seq.concat
  let bind = Seq.concat_map
end
module _ = (RandGen : Utils.Applicative)
*)

let shuffle arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = arr.(j) in
    arr.(j) <- arr.(i);
    arr.(i) <- t
  done

module RandGen = struct
  type 'a t = unit -> 'a option
  let map f v = fun () -> Option.map f (v ())
  let pure x : 'a t = fun () -> Some x
  let pair v1 v2 : ('a * 'b) t = fun () ->
    match v1 (), v2 () with
    | Some a1, Some a2 -> Some (a1, a2)
    | None, _ | _, None -> None

  let fail = fun () -> None

  let one_of (arr : 'a array) : 'a t =
    fun () ->
    if arr = [| |] then None
    else Some arr.(Random.int (Array.length arr))


  let sum (li : 'a t list) : 'a t =
    let choices = Array.of_list li in
    fun () ->
      shuffle choices;
      Array.find_map (fun f -> f ()) choices

  let delay (f : unit -> 'a t) : 'a t =
    fun () -> f () ()

  let bind f v =
    fun () ->
    match v () with
    | None -> None
    | Some a -> f a ()

  let run ~limit (gen : 'a t) : 'a list =
    Seq.forever (fun () -> gen)
    |> Seq.filter_map (fun f -> f ())
    |> Seq.take limit
    |> List.of_seq
end

let ret = RandGen.pure
let (let+) s f = RandGen.map f s
let (and+) sa sb = RandGen.pair sa sb
let ( let* ) s f = RandGen.bind f s

module Untyped = Untyped.Make(RandGen)

module TeVarSet = Set.Make(Untyped.Var)
module TyVarSet = Set.Make(STLC.TyVar)

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
      |> RandGen.one_of
    in
    Do (RandGen.delay @@ fun () ->
        let rule_var =
          let+ x = fvars in Var x in
        let rule_app =
          RandGen.pure (App(gen env, gen env)) in
        let rule_abs =
          RandGen.delay @@ fun () ->
          let x = new_var () in
          ret (Abs(x, gen (Env.bind_tevar x env))) in
        let rule_let =
          RandGen.delay @@ fun () ->
          let x = new_var () in
          ret (Let(x, gen env, gen (Env.bind_tevar x env))) in
        let tuple_size () =
          if Random.bool () then 2
          else 1 + Random.int 4
        in
        let rule_tuple =
          RandGen.delay @@ fun () ->
          let size = tuple_size () in
          let ts = List.init size (fun _ -> gen env) in
          ret (Tuple ts)
        in
        let rule_lettuple =
          RandGen.delay @@ fun () ->
          let size = tuple_size () in
          let xs = List.init size (fun _ -> new_var ()) in
          let env' = List.fold_right Env.bind_tevar xs env in
          ret (LetTuple(xs, gen env, gen env'))
        in
        RandGen.sum [
          rule_var;
          rule_app;
          rule_abs;
          rule_let;
          rule_tuple;
          rule_lettuple;
        ])
  in gen (Env.empty ())

module Constraint = Constraint.Make(RandGen)
module Infer = Infer.Make(RandGen)

let constraint_ : (STLC.term, Infer.err) Constraint.t =
  let w = Constraint.Var.fresh "final_type" in
  Constraint.(Exist (w, None,
    Infer.has_type
      Untyped.Var.Map.empty
      untyped
      w))
    
module ConstraintSolver = Solver.Make(RandGen)
module ConstraintPrinter = ConstraintPrinter.Make(RandGen)

let typed ~depth : STLC.term RandGen.t =
  let open struct
    type env = ConstraintSolver.env
  end in
  let rec loop : type a e r . fuel:int -> env -> (a, e) Constraint.t -> a RandGen.t =
  fun ~fuel env cstr ->
    if fuel = 0 then RandGen.fail else
    let _logs, env, cstr =
      ConstraintSolver.eval ~log:false env cstr
    in
    match cstr with
    | Ret v -> ret (v (Decode.decode env))
    | Err _ -> RandGen.fail
    | _ ->
    let open Constraint in
    let ( let+ ) g f = RandGen.map f g in
    let ( let++ ) r f =
      match r with
      | Ok g -> Ok (RandGen.map f g)
      | Error c -> Error (f c)
    in
    let rec expand : type a e . (a, e) Constraint.t -> ((a, e) Constraint.t RandGen.t, (a, e) Constraint.t) result =
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
    | Error _ ->
      (* If this normal form constraint has no Do,
         then it must be a Ret/Err and we have
         taken the Ret/Err branch earlier. *)
      assert false
    | Ok cstr ->
      RandGen.bind (fun cstr ->
        let env = Unif.Env.copy env in
        loop ~fuel:(fuel - 1) env cstr
      ) cstr
  in
  loop ~fuel:depth (Unif.Env.empty ()) constraint_

