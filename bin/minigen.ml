type config =
  { exhaustive : bool
  ; exhaustive_path : ChoicePath.t option
  ; log_choice_path : bool
  ; types : bool
  ; size : int
  ; count : int
  ; seed : int option
  }

let config =
  let exhaustive = ref false in
  let exhaustive_path = ref None in
  let log_choice_path = ref false in
  let types = ref false in
  let size = ref 10 in
  let count = ref 1 in
  let seed = ref None in

  let choice_path str =
    let path =
      try
        str
        |> Lexing.from_string
        |> ChoicePathParser.path_eof ChoicePathLexer.read
      with _ ->
        Printf.ksprintf (fun s -> raise (Arg.Bad s))
          "--start-choice-path: invalid choice path string %S"
          str
    in
    exhaustive_path := Some path
  in

  let usage = Printf.sprintf "Usage: %s [options]" Sys.argv.(0) in
  let spec =
    Arg.align
      [ ("--exhaustive", Arg.Set exhaustive, " Exhaustive enumeration")
      ; ("--start-choice-path", Arg.String choice_path,
         "<string> Starting choice path for enumeration \
           (only supported in --exhaustive mode)")
      ; ("--log-choice-path", Arg.Set log_choice_path,
         " Log the choice path of each enumerated element \
           (only supported in --exhaustive mode)")
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
  ; exhaustive_path = !exhaustive_path
  ; log_choice_path = !log_choice_path
  ; types = !types
  ; size = !size
  ; count = !count
  ; seed = !seed
  }

let generate (module M : Choice.Intf) =
  let module Gen = Generator.Make (M) in
  M.run @@ Gen.typed ~size:config.size

let () =
  match config.seed with None -> Random.self_init () | Some s -> Random.init s

let () =
  let scheme_to_doc (term, scheme) =
    let term_doc = STLCPrinter.print_term term in

    match config.types with
    | false -> term_doc
    | true ->
      PPrint.(
        term_doc ^^ hardline ^^ hardline ^^ string "Inferred type : "
        ^^ STLCPrinter.print_scheme scheme )
  in

  let choice_module : (module Choice.Intf) =
    if not config.exhaustive then (module MRand)
    else
      let module MSeq' = struct
        include MSeq
        let run s =
          let start_path = match config.exhaustive_path with
            | None -> ChoicePath.Nil
            | Some p -> p
          in
          let log_path = config.log_choice_path in
          run' s start_path |> Seq.map (fun (path, v) ->
            if log_path then begin
              path
              |> ChoicePathPrinter.print
              |> Utils.string_of_doc
              |> Printf.printf "Choice path: %s\n%!"
            end;
            v
          )
      end in
      (module MSeq')
  in
  choice_module
  |> generate
  |> Seq.take config.count
  |> Seq.iter (fun scheme ->
    scheme
    |> scheme_to_doc
    |> Utils.string_of_doc
    |> print_endline
    |> print_newline
  )
