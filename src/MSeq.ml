type 'a t = unit -> 'a list

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    fun () -> List.map f (s ())

let return (x : 'a) : 'a t =
    fun () -> [x]

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    fun () -> List.concat_map (fun l -> f l ()) (sa ())

let delay (f : unit -> 'a t) : 'a t =
    f ()

let sum (li : 'a t list) : 'a t =
    fun () -> List.concat (List.map (fun l -> l ()) li)

let fail : 'a t =
    fun () -> []

let one_of (vs : 'a array) : 'a t =
    fun () -> Array.to_list vs

let run (s : 'a t) : 'a Seq.t =
    List.to_seq (s ())
