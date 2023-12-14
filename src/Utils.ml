type 'a clash = 'a * 'a
type 'v cycle = Cycle of 'v [@@unboxed]

module Variables () : sig
  type t = private {
    name: string;
    stamp: int;
  }

  val compare : t -> t -> int
  val eq : t -> t -> bool

  val fresh : string -> t

  val name : t -> string

  val print : t -> PPrint.document

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end = struct
  type t = {
    name: string;
    stamp: int;
  }

  let name v = v.name

  let compare = Stdlib.compare
  let eq n1 n2 = (compare n1 n2 = 0)

  let stamps = Hashtbl.create 42
  let fresh name =
    let stamp =
      match Hashtbl.find_opt stamps name with
      | None -> 0
      | Some n -> n
    in
    Hashtbl.replace stamps name (stamp + 1);
    { name; stamp; }

  let print { name; stamp } =
    if stamp = 0 then PPrint.string name
    else Printf.ksprintf PPrint.string "%s/%x" name stamp
    
  module Key = struct
    type nonrec t = t
    let compare = compare
  end
  module Set = Set.Make(Key)
  module Map = Map.Make(Key)
end

let namegen fresh names =
  if names = [||] then failwith "namegen: empty names array";
  let counter = ref 0 in
  let wrap n = n mod (Array.length names) in
  fun () ->
    let idx = !counter in
    counter := wrap (!counter + 1);
    fresh names.(idx)

module type Functor = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type MonadPlus = sig
  include Functor
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (* TODO document/explain *)
  val delay : (unit -> 'a t) -> 'a t

  val sum : 'a t list -> 'a t
  (* [fail] and [one_of] can be derived from [sum],
     but they typically have simpler and more efficient
     specialized implementations. *)
  val fail : 'a t
  val one_of : 'a array -> 'a t
end

module Id = struct
  type 'a t = 'a
  let map f v = f v
  let pure v = v
  let pair va vb = (va, vb)
end
module _ = (Id : Functor)
