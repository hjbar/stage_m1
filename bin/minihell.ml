(* We instantiate the machinery with the Empty functor,
   which forbids any use of the Do constructor. *)

module Untyped = Untyped.Make (Utils.Empty)
module UntypedPrinter = UntypedPrinter.Make (Utils.Empty)
module Constraint = Constraint.Make (Utils.Empty)
module Infer = Infer.Make (Utils.Empty)
module ConstraintPrinter = ConstraintPrinter.Make (Utils.Empty)
module Solver = Solver.Make (Utils.Empty)

type config =
  { show_source : bool
  ; show_constraint : bool
  ; log_solver : bool
  ; show_type : bool
  ; show_typed_term : bool
  }

module LexUtil = MenhirLib.LexerUtil

let print_section header doc =
  doc |> Printer.with_header header |> Utils.string_of_doc |> print_endline
  |> print_newline

let call_parser ~config parser_fun input_path =
  let ch = open_in input_path in

  Fun.protect ~finally:(fun () -> close_in ch) @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  let lexbuf = LexUtil.init input_path lexbuf in

  match parser_fun UntypedLexer.read lexbuf with
  | term ->
    let term = Untyped.freshen term in

    if config.show_source then
      print_section "Input term" (UntypedPrinter.print_term term);

    term
  | exception UntypedParser.Error ->
    let open Lexing in
    let start = lexbuf.lex_start_p in
    let stop = lexbuf.lex_curr_p in

    let loc =
      let line pos = pos.pos_lnum in
      let column pos = pos.pos_cnum - pos.pos_bol in

      if start.pos_lnum = stop.pos_lnum then
        Printf.sprintf "%d.%d-%d" (line start) (line stop) (column stop)
      else
        Printf.sprintf "%d.%d-%d.%d" (line start) (column start) (line stop)
          (column stop)
    in

    Printf.ksprintf failwith "%s:%s: syntax error"
      (Filename.quote input_path)
      loc

let call_typer ~config (term : Untyped.term) =
  let cst =
    let open Constraint in
    let s = SVar.fresh "final_scheme" in
    let w = Var.fresh "final_term" in

    Let
      ( s
      , w
      , Infer.has_type (Untyped.Var.Map.empty, Untyped.Var.Map.empty) term w
      , Infer.decode_scheme s )
  in

  if config.show_constraint then
    print_section "Generated constraint"
      (ConstraintPrinter.print_constraint cst);

  let env, nf =
    if config.log_solver then prerr_endline "Constraint solving log:";
    let p =
      Solver.eval ~log:config.log_solver Solver.Env.empty cst Constraint.Done
    in
    if config.log_solver then prerr_newline ();
    p
  in

  match nf with
  | NRet v -> Ok (v (Decode.decode env.unif ()))
  | NErr e -> Error e
  | NDo _ -> .

let print_result ~config result =
  match result with
  | Ok (term, scheme) ->
    if config.show_type then
      print_section "Inferred type" (STLCPrinter.print_scheme scheme);

    if config.show_typed_term then
      print_section "Elaborated term" (STLCPrinter.print_term term)
  | Error err -> begin
    print_section "Error"
    @@
    match err with
    | Infer.Clash (ty1, ty2) ->
      Printer.incompatible (STLCPrinter.print_ty ty1) (STLCPrinter.print_ty ty2)
    | Infer.Cycle (Utils.Cycle v) ->
      Printer.cycle @@ Printer.inference_variable @@ Constraint.Var.print v
  end

let process ~config input_path =
  let ast = call_parser ~config UntypedParser.term_eof input_path in
  let result = call_typer ~config ast in
  print_result ~config result

let parse_args () =
  let show_source = ref false in
  let show_constraint = ref false in
  let log_solver = ref false in
  let show_type = ref true in
  let show_typed_term = ref true in

  let inputs = Queue.create () in
  let add_input path = Queue.add path inputs in

  let usage = Printf.sprintf "Usage: %s [options] <filenames>" Sys.argv.(0) in
  let spec =
    Arg.align
      [ ("--show-source", Arg.Set show_source, " Show input source")
      ; ( "--show-constraint"
        , Arg.Set show_constraint
        , " Show the generated constraint" )
      ; ( "--log-solver"
        , Arg.Set log_solver
        , " Log intermediate constraints during solving" )
      ; ("--show-type", Arg.Set show_type, " Show the inferred type (or error)")
      ; ( "--show-typed-term"
        , Arg.Set show_typed_term
        , " Show the inferred type (or error)" )
      ]
  in

  Arg.parse spec add_input usage;

  let config =
    { show_source = !show_source
    ; show_constraint = !show_constraint
    ; log_solver = !log_solver
    ; show_type = !show_type
    ; show_typed_term = !show_typed_term
    }
  in
  let input_paths = inputs |> Queue.to_seq |> List.of_seq in

  (config, input_paths)

let () =
  let config, input_paths = parse_args () in

  List.iteri
    begin
      fun i input ->
        if i > 0 then print_newline ();
        process ~config input
    end
    input_paths
