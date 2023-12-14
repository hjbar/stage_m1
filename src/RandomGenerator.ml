module RandGen = struct
  type 'a t = 'a Seq.t
  let map = Seq.map

  let pure x = Seq.return x
  let pair sa sb = Seq.product sa sb

  let fail = Seq.empty
  let plus = Seq.append

  let bind = Seq.concat_map
end
module _ = (RandGen : Utils.Applicative)

let ret = RandGen.pure
let fail = RandGen.fail
let (++) = RandGen.plus
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
  let rec gen env = Do (fun () -> () |>
    (let+ x = TeVarSet.to_seq env.Env.tevars in Var x)
    ++
    ret (App(gen env, gen env))
    ++
    ret (
      let x = Var.fresh "x" in
      Abs(x, gen (Env.bind_tevar x env))
    )
  ) in
  gen (Env.empty ())

module Constraint = struct
  include Constraint
  include Constraint.Make(RandGen)
end
module ConstraintGenerator = Generator.Make(RandGen)

let constraint_ : (STLC.term, ConstraintGenerator.err) Constraint.t =
  let w = Constraint.Var.fresh "final_type" in
  Constraint.(Exist (w, None,
    ConstraintGenerator.has_type
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
    if fuel = 0 then fail else
    let _logs, env, cstr =
      ConstraintSolver.eval ~log:false env cstr
    in
    match cstr with
    | Ret v -> ret (v (Decode.decode env))
    | Err _ -> fail
    | _ ->
    let open Constraint in
    let ( let+* ) g f = RandGen.bind f g in
    let ( let++ ) g f = RandGen.map f g in
    let ( and++ ) g1 g2 = RandGen.pair g1 g2 in
    let rec complete : type a e . (a, e) Constraint.t -> (a, e) Constraint.t RandGen.t =
      function
      | Ret v -> ret (Ret v)
      | Err e -> ret (Err e)
      | Map (c, f) ->
        let++ c = complete c in
        Map (c, f)
      | MapErr (c, f) ->
        let++ c = complete c in
        MapErr (c, f)
      | Conj (c1, c2) ->
        let++ c1 = complete c1
        and++ c2 = complete c2
        in Conj (c1, c2)
      | Exist (x, s, c) ->
        let++ c = complete c in
        Exist (x, s, c)
      | Eq _ -> failwith "not a normal form"
      | Decode _ -> failwith "not a normal form"
      | Do p ->
        p
    in
    let+* cstr = complete cstr in
    let env = Unif.Env.copy env in
    loop ~fuel:(fuel - 1) env cstr
  in
  loop ~fuel:depth (Unif.Env.empty ()) constraint_

