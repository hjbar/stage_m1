type config = {
  exhaustive : bool;
  vanille : bool;
  size : int;
  count : int;
  seed : int option;
  benchmark : int option;
  msg : string option;
}

let pp_config ppf config =
  let pp_opt pp ppf = function
    | None -> Printf.fprintf ppf "None"
    | Some v -> Printf.fprintf ppf "Some %a" pp v
  in
  let pp_int ppf = Printf.fprintf ppf "%d" in
  let pp_string ppf = Printf.fprintf ppf "%S" in
  Printf.fprintf ppf 
    "{ exhaustive = %b;\n\
    \  vanille = %b;\n\
    \  size = %d;\n\
    \  count = %d;\n\
    \  seed = %a;\n\
    \  benchmark = %a;\n\
    \  msg = %a; }\n"
    config.exhaustive
    config.vanille
    config.size
    config.count
    (pp_opt pp_int) config.seed
    (pp_opt pp_int) config.benchmark
    (pp_opt pp_string) config.msg

let config =
  let exhaustive = ref false in
  let vanille = ref false in
  let size = ref 10 in
  let count = ref 1 in
  let seed = ref None in
  let benchmark = ref None in
  let msg = ref None in
  let usage =
    Printf.sprintf
      "Usage: %s [options]"
      Sys.argv.(0) in
  let spec = Arg.align [
    "--exhaustive", Arg.Set exhaustive,
      " Exhaustive enumeration";
    "--vanille", Arg.Set vanille,
      " Use Néven's MRand implementation";
    "--size", Arg.Set_int size,
      "<int> Depth of generated terms";
    "--count", Arg.Set_int count,
      "<int> Number of terms to generate";
    "--seed", Arg.Int (fun s -> seed := Some s),
      "<int> Fixed seed for the random number generator";
    "--benchmark", Arg.Int (fun s -> benchmark := Some s),
      "<int> Run several times and provide performance metrics.";
    "--msg", Arg.String (fun s -> msg := Some s),
      "<string> Describe the benchmark to get a self-describing log file.";
  ] in
  Arg.parse spec (fun s -> raise (Arg.Bad s)) usage;
  {
    exhaustive = !exhaustive;
    vanille = !vanille;
    size = !size;
    count = !count;
    seed  = !seed;
    benchmark = !benchmark;
    msg = !msg;
  }

let () =
  match config.seed with
  | None -> Random.self_init ()
  | Some s -> Random.init s

let generate (module M : Utils.MonadPlus) =
  let module Gen = Generator.Make(M) in
  M.run @@ Gen.typed ~size:config.size

let produce_terms config =
  generate
    (if config.exhaustive
     then (module MSeq)
     else if config.vanille
     then (module VanilleRand)
     else (module MRand))
  |> Seq.take config.count
  |> Seq.map STLCPrinter.print_term
  |> List.of_seq

let run_display config =
  produce_terms config
  |> PPrint.(separate (hardline ^^ hardline))
  |> Utils.string_of_doc
  |> print_endline

let pp_s ppf s = Printf.fprintf ppf "%.2fs" s

let run_benchmark config ~nb_iters =
  pp_config stdout config;
  let times = ref [] in
  for i = 1 to nb_iters do
    Printf.eprintf "Iteration % 3d/%-3d: %!" i nb_iters;
    let before = Unix.gettimeofday () in
    ignore (produce_terms config);
    let after = Unix.gettimeofday () in
    let duration = after -. before in
    Printf.eprintf "%a\n%!" pp_s duration;
    times := duration :: !times;
  done;
  let times = !times in
  let minv = List.fold_left min infinity times in
  let maxv = List.fold_left max neg_infinity times in
  let arith_average =
    List.fold_left (+.) 0. times /. float nb_iters
  in
  let geom_average =
    List.fold_left ( *.) 1. times ** (1. /. float nb_iters)
  in
  Printf.printf "\
    Min: %a\n\
    Max: %a\n\
    Arithmetic average: %a\n\
    Geometric  average: %a\n%!"
    pp_s minv
    pp_s maxv
    pp_s arith_average
    pp_s geom_average

let () =
  match config.benchmark with
  | None -> run_display config
  | Some nb_iters -> run_benchmark config ~nb_iters

