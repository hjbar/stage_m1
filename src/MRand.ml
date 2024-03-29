type 'a t = {
    finished : 'a list;
    later : (unit -> 'a t) list;
}

let of_finished (a : 'a list) : 'a t =
    {
      finished = a;
      later = [];
    }

let of_later (a : (unit -> 'a t) list) : 'a t =
    {
      finished = [];
      later = a;
    }

let empty : 'a t =
    of_finished []

let rec map (f : 'a -> 'b) (s : 'a t) : 'b t =
    {
      finished = s.finished |> List.map f;
      later = s.later |> List.map (fun gen () -> gen () |> map f);
    }

let return (x : 'a) : 'a t =
    of_finished [x]

let delay (f : unit -> 'a t) : 'a t =
    of_later [f]

let concat (s : 'a t) (s' : 'a t) : 'a t =
    {
      finished = s.finished @ s'.finished;
      later = s.later @ s'.later;
    }

let rec bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    sa.finished
    |> List.map f
    |> List.fold_left concat (sa |> bind_later f)

and bind_later (f : 'a -> 'b t) (s : 'a t) =
    of_later (s.later |> List.map (fun s () -> bind (s ()) f))

let sum (li : 'a t list) : 'a t =
    li
    |> List.fold_left concat empty

let fail : 'a t =
    empty

let one_of (vs : 'a array) : 'a t =
    vs
    |> Array.to_list
    |> of_finished

type 'a chosen =
    | Picked of 'a
    | Retry
    | Empty

(* @raise: [Not_found] if [i > List.length l] *)
let rec take_nth (l : 'a list) (n : int) : 'a * 'a list =
    match l, n with
    | h::t, i when i <= 0 -> h, t
    | h::t, i -> let out, rem = take_nth t (i - 1) in out, h::rem
    | [], _ -> raise Not_found

let run (s : 'a t) : 'a Seq.t =
    let rec try_pick (sampler : 'a t) : 'a chosen * 'a t =
        let pick_finished () =
            let idx = Random.int (List.length sampler.finished) in
            let x, rest = take_nth sampler.finished idx in
            Picked x, { sampler with finished = rest }
        in
        let pick_later () =
            let idx = Random.int (List.length sampler.later) in
            let gen, trimmed = take_nth sampler.later idx in
            let res, gen = try_pick (gen ()) in
            match res with
                | Picked x -> Picked x, { sampler with later = (fun () -> gen) :: trimmed }
                | Retry -> Retry, { sampler with later = (fun () -> gen) :: trimmed }
                | Empty -> Retry, { sampler with later = trimmed }
        in
        if sampler.finished = [] && sampler.later = [] then (
          Empty, sampler
        ) else if sampler.later = [] then (
          pick_finished ()
        ) else if sampler.finished = [] then (
          pick_later ()
        ) else (
            if Random.bool () then (
              pick_finished ()
            ) else (
              pick_later ()
            )
        )
    in
    let rec pick (miss : int) (sampler : 'a t) : 'a * 'a t =
        let res, trimmed = try_pick sampler in
        match res with
        | Picked t -> t, trimmed
        | Retry -> pick (miss + 1) trimmed
        | Empty -> failwith "This generator is empty; no such term exists"
    in
    let rec seq (sampler : 'a t) () =
        let res, trimmed = pick 0 sampler in
        Seq.Cons (res, seq trimmed)
    in
    seq s

