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

let get_tries config =
  if config.exhaustive then failwith "tries"
  else if config.vanille then !VanilleRand.tries
  else !MRand.tries

let print_stats pp_val series =
  let minv = List.fold_left min infinity series in
  let maxv = List.fold_left max neg_infinity series in
  let len = List.length series in
  let arith_average =
    List.fold_left (+.) 0. series /. float len
  in
  let geom_average =
    List.fold_left ( *.) 1. series ** (1. /. float len)
  in
  Printf.printf "\
    Min: %a\n\
    Max: %a\n\
    Arithmetic average: %a\n\
    Geometric  average: %a\n%!"
    pp_val minv
    pp_val maxv
    pp_val arith_average
    pp_val geom_average

let run_benchmark config ~nb_iters =
  pp_config stdout config;
  let time_series = ref [] in
  let tries_series = ref [] in
  for i = 1 to nb_iters do
    Printf.eprintf "Iteration % 3d/%-3d: %!" i nb_iters;
    let time_before = Unix.gettimeofday () in
    let tries_before = get_tries config in
    ignore (produce_terms config);
    let time_after = Unix.gettimeofday () in
    let tries_after = get_tries config in
    let time = time_after -. time_before in
    let tries = tries_after - tries_before in
    Printf.eprintf "%a\n%!" pp_s time;
    time_series := time :: !time_series;
    tries_series := float tries :: !tries_series;
  done;
  Printf.printf "## Times\n%!";
  print_stats pp_s !time_series;
  Printf.printf "## Tries\n%!";
  print_stats (fun ppf v -> Printf.fprintf ppf "%.2f" v) !tries_series;
  ()

let () =
  match config.benchmark with
  | None -> run_display config
  | Some nb_iters -> run_benchmark config ~nb_iters

