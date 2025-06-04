(* Global variables *)

let debug = true

let run_test f = if debug then f ()

(* Utils functions *)

let get_block sep header doc =
  let short_sep = String.make 5 sep in
  let long_sep = String.make (6 + String.length header + 6) sep in

  let open PPrint in
  string (short_sep ^ " " ^ header ^ " " ^ short_sep)
  ^^ break 1 ^^ doc ^^ string long_sep

let get_header header doc = get_block '=' header doc

let get_sub_header sub_header doc = get_block '-' sub_header doc

(* One-line functions *)

let print_document doc =
  run_test @@ fun () -> doc |> Utils.string_of_doc |> prerr_endline

let print_header header doc =
  run_test @@ fun () -> doc |> get_header header |> print_document

let print_sub_header sub_header doc =
  run_test @@ fun () -> doc |> get_sub_header sub_header |> print_document

let print_message msg =
  run_test @@ fun () ->
  let sep = String.make 5 '*' in

  let open PPrint in
  string sep ^^ break 1 ^^ string msg ^^ break 1 ^^ string sep ^^ break 1
  |> print_document

(* Debug display of the whole unification environment *)
let debug_what (env : Unif.Env.t) : unit =
  let open PPrint in
  run_test @@ fun () ->
  get_header "PRINT ENV" (Unif.Env.debug env)
  ^^ break 1
  |> print_document

(* Debug display of the by-rank pools in the environment. *)
let debug_what_pool_assoc (r : Unif.rank) (env : Unif.Env.t) : unit =
  let open PPrint in
  run_test @@ fun () ->
  get_header "PRINT ENV POOL ASSOC" (Unif.Env.debug_pool_assoc env)
  ^^ break 1
  ^^ get_sub_header "Get pool of rank" (string (string_of_int r) ^^ break 1)
  ^^ break 1
  |> print_document
