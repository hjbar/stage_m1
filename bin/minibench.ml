type search_impl =
  | Exhaustive
  | Sampling

let search_impl_dict = [ ("exhaustive", Exhaustive); ("sampling", Sampling) ]

type filter_impl =
  | Early
  | Late

let filter_impl_dict : (string * filter_impl) list =
  [ ("early", Early); ("late", Late) ]


module Dict = struct
  type ('k, 'v) t = ('k * 'v) list

  let keys dict = List.map fst dict

  let find_val dict k = List.assoc k dict

  let find_key (dict : ('k, 'v) t) (v : 'v) : 'k =
    List.assoc v (List.map (fun (k, v) -> (v, k)) dict)
end

let pp_dict_val dict ppf v = Printf.fprintf ppf "%s" (Dict.find_key dict v)

type config = {
  search_impl : search_impl;
  filter_impl : filter_impl;
  size : int;
  count : int;
  untyped_count : int;
}

let pp_config ppf config =
  let pp_int ppf = Printf.fprintf ppf "%d" in
  begin[@ocamlformat "disable"]
    Printf.fprintf ppf
      "{ filter_impl = %a;\n\
      \  search_impl = %a;\n\
      \  size = %a;\n\
      \  count = %a; }\n"
      (pp_dict_val filter_impl_dict) config.filter_impl
      (pp_dict_val search_impl_dict) config.search_impl
      pp_int config.size
      pp_int config.count
  end


let arg_from_dict ~option ~doc dict ref : Arg.key * Arg.spec * Arg.doc =
  let valid_keys = Dict.keys dict |> String.concat " | " in
  let on_string s =
    match Dict.find_val dict s with
    | v -> ref := v
    | exception Not_found ->
      Printf.ksprintf
        (fun msg -> raise (Arg.Bad msg))
        "%s: invalid value '%s', expected one of [%s]" option s valid_keys
  in
  ( option,
    Arg.String on_string,
    Printf.sprintf "<name> %s (default '%s', available [%s])" doc
      (Dict.find_key dict !ref) valid_keys )


let config =
  let filter_impl = ref (Early : filter_impl) in
  let search_impl = ref (Exhaustive : search_impl) in
  let size = ref 4 in
  let count = ref 1000 in
  let untyped_count = ref 1000 in
  let benchmark = ref None in
  let msg = ref None in
  let fmt fmt_str = Printf.sprintf fmt_str in
  let usage = fmt "Usage: %s [options]" Sys.argv.(0) in

  let spec =
    Arg.align
      [
        ( "--size",
          Arg.Set_int size,
          fmt "<int> Size of generated terms (default %d)" !size );
        arg_from_dict ~option:"--search" ~doc:"searching implementation"
          search_impl_dict search_impl;
        arg_from_dict ~option:"--filter" ~doc:"filtering implementation"
          filter_impl_dict filter_impl;
        ( "--count",
          Arg.Set_int count,
          fmt "<int> Number of terms to generate. (default '%d')" !count );
        ( "--untyped-count",
          Arg.Set_int untyped_count,
          fmt "<int> Number of untyped terms to generate (for [--filter late]).\
               (default '%d')" !untyped_count );
        ( "--benchmark",
          Arg.Int (fun s -> benchmark := Some s),
          "<int> Run several times and provide performance metrics. (optional)"
        );
        ( "--msg",
          Arg.String (fun s -> msg := Some s),
          "<string> Describe the benchmark to get a self-describing log file. \
           (optional)" );
      ]
  in

  Arg.parse spec (fun s -> raise (Arg.Bad s)) usage;

  {
    filter_impl = !filter_impl;
    search_impl = !search_impl;
    size = !size;
    count = !count;
    untyped_count = !untyped_count;
  }


let search_impl config : (module Choice.Intf) =
  match config.search_impl with
  | Exhaustive -> (module MSeq)
  | Sampling -> (module MRand_full_removal_with_reset)

let generate config =
  let module MSearch = (val search_impl config) in
  let module Gen = Generator.Make (MSearch) in
  let untyped = Gen.untyped_gasche in
  let module Unty = Untyped.Make (MSearch) in
  let rec unfold : Unty.term -> Unty.term MSearch.t =
    let ( let* ) = MSearch.bind in
    let ( let+ ) x f = MSearch.map f x in
    let ret = MSearch.return in
    let open Unty in
    function
    | Do m -> MSearch.bind m unfold
    | Var x -> ret (Var x)
    | App (t1, t2) ->
      let* t1 = unfold t1 in
      let+ t2 = unfold t2 in
      App (t1, t2)
    | Abs (x, t) ->
      let+ t = unfold t in
      Abs (x, t)
    | Let (x, t1, t2) ->
      let* t1 = unfold t1 in
      let+ t2 = unfold t2 in
      Let (x, t1, t2)
    | Tuple ts -> begin
      match ts with
      | [ t1; t2 ] ->
        let* t1 = unfold t1 in
        let+ t2 = unfold t2 in
        Tuple [ t1; t2 ]
      | _ -> assert false
    end
    | LetTuple (xs, t1, t2) ->
      let* t1 = unfold t1 in
      let+ t2 = unfold t2 in
      LetTuple (xs, t1, t2)
    | Annot (t, ty) ->
      let+ t = unfold t in
      Annot (t, ty)
    | Loc (_loc, t) -> unfold t
  in
  match config.filter_impl with
  | Early ->
    untyped
    |> Gen.typed_cut_early ~size:config.size
    |> MSearch.run
    |> Seq.map (fun v -> Ok v)
  | Late ->
    let untyped =
      let run =
        untyped
        |> Gen.cut_size ~size:config.size
        |> unfold
        |> MSearch.run
      in
      match config.search_impl with
      | Exhaustive ->
        run
      | Sampling ->
        run
        |> Seq.take 1
        |> Seq.cycle
    in
    untyped
    |> Seq.concat_map (fun untyped ->
         untyped
         |> Gen.constraint_
         |> Gen.solve
         |> MSearch.run
         |> Seq.map (function
              | Ok v -> Ok v
              | Error _ -> Error ()) )

let count_terms config =
  let rec count seq (total, well_typed) =
    if total >= config.untyped_count
    || well_typed >= config.count
    then (total, well_typed)
    else match Seq.uncons seq with
    | None ->
      (total, well_typed)
    | Some (Error (), seq) ->
      count seq (total + 1, well_typed)
    | Some (Ok _, seq) ->
      count seq (total + 1, well_typed + 1)
  in
  count (generate config) (0, 0)

let run_display config =
  let (total, well_typed) = count_terms config in
  match config.filter_impl with
  | Early ->
    Printf.printf "At size %d, found %d well-typed terms out of %d.\n%!"
      config.size well_typed config.untyped_count
  | Late ->
    Printf.printf "At size %d, found %d well-typed terms among %d well-scoped terms (ratio %.4f).\n%!"
      config.size well_typed total (float well_typed /. float total)

let () =
  Random.self_init ();
  run_display config
