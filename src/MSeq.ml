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
        | Delay f -> f () |> unfold
        | Bind (s, f) -> s |> unfold |> List.concat_map (fun x -> x |> f |> unfold)
        | Sum ls -> ls |> List.concat_map unfold
    in s
    |> unfold
    |> List.to_seq
