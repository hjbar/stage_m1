type 'a t =
    | One of 'a
    | Choose of 'a t list
    | Lazy of (unit -> 'a t)

let possibly_nonempty : 'a t -> bool = function
    | Choose [] -> false
    | One _ -> true
    | Lazy _ -> true
    | Choose _ -> true

let rec compact : 'a t -> 'a t = function
    | One x -> One x
    | Lazy gen -> Lazy gen
    | Choose li -> Choose (li |> List.map compact |> List.filter possibly_nonempty)


let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    let rec aux = function
        | One x -> One (f x)
        | Choose ts -> Choose (ts |> List.map compact |> List.map aux)
        | Lazy gen -> Lazy (fun () -> gen () |> compact |> aux)
    in s |> compact |> aux

let return (x : 'a) : 'a t =
    One x

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    let rec aux = function
        | One x -> f x
        | Choose ts -> Choose (ts |> List.map compact |> List.map aux)
        | Lazy gen -> Lazy (fun () -> gen () |> compact |> aux)
    in aux sa

let delay (f : unit -> 'a t) : 'a t =
    Lazy (fun () -> f () |> compact)

let sum (li : 'a t list) : 'a t =
    Choose (li |> List.map compact)

let fail : 'a t =
    Choose []

let one_of (vs : 'a array) : 'a t =
    Choose (vs |> Array.to_list |> List.map return)

let run (s : 'a t) : 'a Seq.t =
    let s = compact s in
    let rec try_pick = function
        | One x -> Some x
        | Choose [] -> None
        | Choose ts -> (
            ts
            |> List.length
            |> Random.int
            |> List.nth ts
            |> try_pick
        )
        | Lazy gen -> gen () |> compact |> try_pick
    in
    let rec pick miss =
        match try_pick s with
        | Some t -> Format.printf "Found after %d misses\n%!" miss; t
        | None -> pick (miss + 1)
    in
    let rec seq () =
        Seq.Cons (pick 0, seq)
    in
    seq

