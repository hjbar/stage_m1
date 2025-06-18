(* Global variables *)

let debug =
  match Sys.getenv "DEBUG" with
  | "1" | "yes" | "true" | "Y" -> true
  | exception Not_found -> false
  | "0" | "no" | "false" | "N" -> false
  | other ->
    Printf.ksprintf invalid_arg
      "Unknown value for DEBUG environment variable: %S" other

let run_test f = if debug then f ()

(* Utils functions *)

let get_block sep header doc =
  let short_sep = String.make 5 sep in
  let long_sep = String.make (6 + String.length header + 6) sep in

  let open PPrint in
  string (Format.sprintf "%s %s %s" short_sep header short_sep)
  ^^ hardline ^^ doc ^^ hardline ^^ string long_sep

let get_header header doc = get_block '=' header doc

let get_sub_header sub_header doc = get_block '-' sub_header doc

(* One-line functions *)

let print_document doc =
  run_test @@ fun () -> doc |> Utils.string_of_doc |> Format.eprintf "%s\n%!"

let print_header header doc =
  run_test @@ fun () -> doc |> get_header header |> print_document

let print_sub_header sub_header doc =
  run_test @@ fun () -> doc |> get_sub_header sub_header |> print_document

let print_message msg =
  run_test @@ fun () ->
  let sep = String.make 5 '*' in

  let open PPrint in
  string sep ^^ hardline ^^ string msg ^^ hardline ^^ string sep
  |> print_document
