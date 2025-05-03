type search_impl =
  | Exhaustive
  | Naive
  | Vanille

let search_impl_dict = [
  ("exhaustive", Exhaustive);
  ("naive", Naive);
  ("vanille", Vanille);
]

module Dict = struct
  type ('k, 'v) t = ('k * 'v) list

  let keys dict = List.map fst dict

  let find_val dict k =
    List.assoc k dict

  let find_key (dict : ('k, 'v) t) (v : 'v) : 'k =
    List.assoc v (List.map (fun (k, v) -> (v, k)) dict)
end

let pp_dict_val dict ppf v =
  Printf.fprintf ppf "%s"
    (Dict.find_key dict v)

type config = {
  search_impl : search_impl;
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
    "{ search_impl = %a;\n\
    \  size = %a;\n\
    \  count = %a;\n\
    \  seed = %a;\n\
    \  benchmark = %a;\n\
    \  msg = %a; }\n"
    (pp_dict_val search_impl_dict) config.search_impl
    pp_int config.size
    pp_int config.count
    (pp_opt pp_int) config.seed
    (pp_opt pp_int) config.benchmark
    (pp_opt pp_string) config.msg

let arg_from_dict ~option ~doc dict ref : Arg.(key * spec * doc) =
  let valid_keys =
    Dict.keys dict |> String.concat " | "
  in
  let on_string s =
    match Dict.find_val dict s with
    | v -> ref := v
    | exception Not_found -> 
      Printf.ksprintf (fun msg -> raise (Arg.Bad msg))
        "%s: invalid value '%s', expected one of [%s]"
        option s valid_keys
  in
  (option,
   Arg.String on_string,
   Printf.sprintf "<name> %s (default '%s', available [%s])"
     doc (Dict.find_key dict !ref) valid_keys)

let config =
  let search_impl = ref Naive in
  let size = ref 10 in
  let count = ref 1 in
  let seed = ref None in
  let benchmark = ref None in
  let msg = ref None in
  let fmt fmt_str = Printf.sprintf fmt_str in
  let usage = fmt "Usage: %s [options]" Sys.argv.(0) in
  let spec = Arg.align [
    arg_from_dict
      ~option:"--search"
      ~doc:"search implementation"
      search_impl_dict
      search_impl;
    "--count", Arg.Set_int count,
      fmt "<int> Number of terms to generate. (default '%d')" !count;
    "--benchmark", Arg.Int (fun s -> benchmark := Some s),
      "<int> Run several times and provide performance metrics. (optional)";
    "--seed", Arg.Int (fun s -> seed := Some s),
      "<int> Fixed seed for the random number generator. (optional)";
    "--msg", Arg.String (fun s -> msg := Some s),
      "<string> Describe the benchmark to get a self-describing log file. (optional)";
  ] in
  Arg.parse spec (fun s -> raise (Arg.Bad s)) usage;
  {
    search_impl = !search_impl;
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

module type SearchImpl = sig
  include Utils.MonadPlus
  val tries : int ref
end

module ExhaustiveSearch = struct
  include MSeq
  let tries = ref 1
end

let get_search_impl config : (module SearchImpl) =
  match config.search_impl with
  | Exhaustive ->
    (module ExhaustiveSearch)
  | Naive ->
    (module MRand)
  | Vanille ->
    (module VanilleRand)

let generate (module M : SearchImpl) =
  let module Gen = Generator.Make(M) in
  M.run @@ Gen.typed ~size:config.size

let produce_terms config =
  get_search_impl config
  |> generate
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
  let module M = (val (get_search_impl config)) in
  !M.tries

let print_stats pp_val series =
  let minv = List.fold_left min infinity series in
  let maxv = List.fold_left max neg_infinity series in
  let len = List.length series in
  let arith_average =
    List.fold_left (+.) 0. series /. float len
  in
  let geom_average =
    series
    |> List.map (fun x -> x ** (1. /. float len))
    |> List.fold_left ( *. ) 1.
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

