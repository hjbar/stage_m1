type 'a t = 'a list lazy_t

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    lazy (List.map f (Lazy.force s))

let return (x : 'a) : 'a t =
    lazy [x]

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    lazy (List.concat_map (fun l -> Lazy.force (f l)) (Lazy.force sa))

let delay (f : unit -> 'a t) : 'a t =
    f ()

let sum (li : 'a t list) : 'a t =
    lazy (List.concat (List.map Lazy.force li))

let fail : 'a t =
    lazy []

let one_of (vs : 'a array) : 'a t =
    lazy (Array.to_list vs)

let run (s : 'a t) : 'a Seq.t =
    List.to_seq (Lazy.force s)
