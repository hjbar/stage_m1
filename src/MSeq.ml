type 'a t =
    | Done : 'a list -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Delay : (unit -> 'a t) -> 'a t
    | Sum : 'a t list -> 'a t

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    Bind (s, fun x -> Done [f x])

let return (x : 'a) : 'a t =
    Done [x]

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    Bind (sa, f)

let delay (f : unit -> 'a t) : 'a t =
    Delay f

let sum (li : 'a t list) : 'a t =
    Sum li

let fail : 'a t =
    Done []

let one_of (vs : 'a array) : 'a t =
    Done (Array.to_list vs)

let run (s : 'a t) : 'a Seq.t =
    let rec unfold : type a . a t -> a list = function
        | Done l -> l
        | Delay f -> unfold (f ())
        | Bind (s, f) -> List.concat_map (fun x -> unfold (f x)) (unfold s)
        | Sum li -> List.concat_map unfold li
    in
    List.to_seq (unfold s)
