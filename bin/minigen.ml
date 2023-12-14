type config = {
  depth : int;
  count : int;
  seed : int option;
}

let config =
  let depth = ref 10 in
  let count = ref 1 in
  let seed = ref None in
  let usage =
    Printf.sprintf
      "Usage: %s [options]"
      Sys.argv.(0) in
  let spec = Arg.align [
    "--depth", Arg.Set_int depth,
      "<int> Depth of generated terms";
    "--count", Arg.Set_int count,
      "<int> Number of terms to generate";
    "--seed", Arg.Int (fun s -> seed := Some s),
      "<int> Fixed seed for the random number generator";
  ] in
  Arg.parse spec (fun s -> raise (Arg.Bad s)) usage;
  {
    depth = !depth;
    count = !count;
    seed  = !seed;
  }

let () =
  match config.seed with
  | None -> Random.self_init ()
  | Some s -> Random.init s

let () =
  Generator.typed ~depth:config.depth
  |> Generator.RandGen.run ~limit:config.count
  |> List.map STLCPrinter.print_term
  |> PPrint.(separate (hardline ^^ hardline))
  |> Printer.string_of_doc
  |> print_endline
