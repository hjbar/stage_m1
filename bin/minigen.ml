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
  M.run @@ Gen.typed_cut_early Gen.untyped_gasche ~size:config.size

let () =
  match config.seed with None -> Random.self_init () | Some s -> Random.init s

let () =
  let scheme_to_doc (term, scheme) =
    let term_doc = FPrinter.print_term term in

    match config.types with
    | false -> term_doc
    | true ->
      PPrint.(
        term_doc ^^ hardline ^^ hardline ^^ string "Inferred type : "
        ^^ FPrinter.print_scheme scheme )
  in

  (if config.exhaustive then (module MSeq) else (module MRand))
  |> generate |> Seq.take config.count |> Seq.map scheme_to_doc |> List.of_seq
  |> PPrint.(separate (hardline ^^ hardline ^^ hardline ^^ hardline))
  |> Utils.string_of_doc |> print_endline
