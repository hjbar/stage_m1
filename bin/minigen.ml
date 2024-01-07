type config = {
  exhaustive : bool;
  depth : int;
  count : int;
  seed : int option;
}

let config =
  let exhaustive = ref false in
  let depth = ref 10 in
  let count = ref 1 in
  let seed = ref None in
  let usage =
    Printf.sprintf
      "Usage: %s [options]"
      Sys.argv.(0) in
  let spec = Arg.align [
    "--exhaustive", Arg.Set exhaustive,
      " Exhaustive enumeration";
    "--depth", Arg.Set_int depth,
      "<int> Depth of generated terms";
    "--count", Arg.Set_int count,
      "<int> Number of terms to generate";
    "--seed", Arg.Int (fun s -> seed := Some s),
      "<int> Fixed seed for the random number generator";
  ] in
  Arg.parse spec (fun s -> raise (Arg.Bad s)) usage;
  {
    exhaustive = !exhaustive;
    depth = !depth;
    count = !count;
    seed  = !seed;
  }

let () =
  match config.seed with
  | None -> Random.self_init ()
  | Some s -> Random.init s

let generate (module M : Utils.MonadPlus) =
  let module Gen = Generator.Make(M) in
  M.run @@ Gen.typed ~depth:config.depth

let () =
  generate
    (if config.exhaustive
     then (module MSeq)
     else (module MRand))
  |> Seq.take config.count
  |> Seq.map STLCPrinter.print_term
  |> List.of_seq
  |> PPrint.(separate (hardline ^^ hardline))
  |> Utils.string_of_doc
  |> print_endline
