type 'a clash = 'a * 'a

type 'v cycle = Cycle of 'v [@@unboxed]

type loc = Lexing.position * Lexing.position

let string_of_doc doc =
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf


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

module type MonadPlus = sig
  include Functor

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val sum : 'a t list -> 'a t

  val fail : 'a t

  val one_of : 'a array -> 'a t

  val delay : (unit -> 'a t) -> 'a t

  val run : 'a t -> 'a Seq.t
end

module Empty = struct
  type 'a t = |

  let map (_ : 'a -> 'b) : 'a t -> 'b t = function
    | _ -> .
end

module _ : Functor = Empty
