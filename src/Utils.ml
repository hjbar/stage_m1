type 'a clash = 'a * 'a

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
    else Printf.ksprintf PPrint.string "%s/%d" name stamp
    
  module Key = struct
    type nonrec t = t
    let compare = compare
  end
  module Set = Set.Make(Key)
  module Map = Map.Make(Key)
end
