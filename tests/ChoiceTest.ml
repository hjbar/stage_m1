type input =
  | String of string

let process = function
  | String str ->
    try
      str
      |> Lexing.from_string
      |> ChoicePathParser.path_eof ChoicePathLexer.read
      |> ChoicePathPrinter.print
      |> Utils.string_of_doc
      |> print_endline
    with ChoicePathParser.Error ->
      Printf.ksprintf failwith
        "ChoiceTest: parse error on input %S" str

let inputs = Queue.create ()

let spec =
  Arg.align [
    ("--choice-path",
     Arg.String (fun str -> Queue.push (String str) inputs),
     "<string> parse and reprint a choice path");
  ]

let () =
  Arg.parse spec (fun s -> raise (Arg.Bad s))
    "a test utility for choice paths";
  Queue.iter process inputs
