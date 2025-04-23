type config =
  { exhaustive : bool
  ; types : bool
  ; size : int
  ; count : int
  ; seed : int option
  }

let config =
  let exhaustive = ref false in
  let types = ref false in
  let size = ref 10 in
  let count = ref 1 in
  let seed = ref None in

  let usage = Printf.sprintf "Usage: %s [options]" Sys.argv.(0) in
  let spec =
    Arg.align
      [ ("--exhaustive", Arg.Set exhaustive, " Exhaustive enumeration")
      ; ("--types", Arg.Set types, " Display types of terms")
      ; ("--size", Arg.Set_int size, "<int> Depth of generated terms")
      ; ("--count", Arg.Set_int count, "<int> Number of terms to generate")
      ; ( "--seed"
        , Arg.Int (fun s -> seed := Some s)
        , "<int> Fixed seed for the random number generator" )
      ]
  in
  Arg.parse spec (fun s -> raise @@ Arg.Bad s) usage;

  { exhaustive = !exhaustive
  ; types = !types
  ; size = !size
  ; count = !count
  ; seed = !seed
  }

let generate (module M : Utils.MonadPlus) =
  let module Gen = Generator.Make (M) in
  M.run @@ Gen.typed ~size:config.size

let get_type (t : STLC.term) : STLC.ty =
  let module Untyped = Untyped.Make (Utils.Empty) in
  let module Constraint = Constraint.Make (Utils.Empty) in
  let module Infer = Infer.Make (Utils.Empty) in
  let module Solver = Solver.Make (Utils.Empty) in
  let t = Untyped.of_typed t in

  let cst =
    let w = Constraint.Var.fresh "final_type" in

    Constraint.(
      Exist
        ( w
        , None
        , Conj (Infer.has_type Untyped.Var.Map.empty t w, Infer.decode w) ) )
  in

  let _, env, nf = Solver.eval ~log:false Unif.Env.empty cst in
  match nf with
  | NRet v -> snd @@ v @@ Decode.decode env ()
  | NErr _ | NDo _ -> assert false

let () =
  match config.seed with None -> Random.self_init () | Some s -> Random.init s

let () =
  let map_function =
    let f =
      if config.types then fun t ->
        [ STLCPrinter.print_term t; STLCPrinter.print_ty @@ get_type t ]
      else fun t -> [ STLCPrinter.print_term t ]
    in
    Seq.map f
  in
  let separate_function =
    PPrint.(
      if config.types then
        separate @@ hardline ^^ hardline ^^ hardline ^^ hardline
      else separate (hardline ^^ hardline) )
  in

  generate (if config.exhaustive then (module MSeq) else (module MRand))
  |> Seq.take config.count |> map_function
  |> Seq.map PPrint.(separate @@ hardline ^^ hardline)
  |> List.of_seq |> separate_function |> Utils.string_of_doc |> print_endline
