type 'a clash = 'a * 'a

type 'v cycle = Cycle of 'v [@@unboxed]

type loc = Lexing.position * Lexing.position

let string_of_loc loc = MenhirLib.LexerUtil.range loc

exception Located of loc * exn * Printexc.raw_backtrace

let at_loc loco f =
  let locate_exn loc exn =
    match exn with
    | Located (_, _, _) as exn -> raise exn
    | base_exn ->
      let bt = Printexc.get_raw_backtrace () in
      raise @@ Located (loc, base_exn, bt)
  in
  match loco with
  | None -> f ()
  | Some loc -> ( try f () with exn -> locate_exn loc exn )


let print_loco = function
  | None -> ()
  | Some loc -> loc |> string_of_loc |> print_string


let string_of_doc doc =
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf


let with_section header doc =
  let open PPrint in
  let with_header header doc =
    string header ^^ colon ^^ nest 2 (group (break 1 ^^ doc))
  in
  with_header header doc ^^ hardline


let print_section header doc =
  doc |> with_section header |> string_of_doc |> prerr_string


module Variables () = struct
  type t = {
    name : string;
    stamp : int;
  }

  let name v = v.name

  let compare n1 n2 =
    let c = Int.compare n1.stamp n2.stamp in
    if c <> 0 then c else String.compare n1.name n2.name


  let eq n1 n2 = compare n1 n2 = 0

  let stamps = Hashtbl.create 16

  let fresh name =
    let stamp =
      match Hashtbl.find_opt stamps name with
      | None -> 0
      | Some n -> n
    in
    Hashtbl.replace stamps name (stamp + 1);
    { name; stamp }


  let namegen names =
    if names = [||] then failwith "namegen: empty names array";

    let counter = ref 0 in
    let wrap n = n mod Array.length names in

    fun () ->
      let idx = !counter in
      counter := wrap (!counter + 1);
      fresh names.(idx)


  let print { name; stamp } =
    if stamp = 0 then PPrint.string name
    else Printf.ksprintf PPrint.string "%s/%x" name stamp


  module Key = struct
    type nonrec t = t

    let compare = compare
  end

  module Set = Set.Make (Key)
  module Map = Map.Make (Key)
end

module type Functor = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Empty = struct
  type 'a t = |

  let map (_ : 'a -> 'b) : 'a t -> 'b t = function
    | _ -> .
end

module _ : Functor = Empty
