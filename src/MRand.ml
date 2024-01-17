type 'a t = {
    (* WF invariant :
        len = List.length finished
        && len' = List.length later *)
    len : int;
    len' : int;
    finished : 'a list;
    later : (unit -> 'a t) list;
}

let wf (s : 'a t) : 'a t =
    assert (s.len + s.len' = List.length s.finished + List.length s.later);
    s

let of_finished (a : 'a list) : 'a t =
    wf { len = List.length a;
      len' = 0;
      finished = a;
      later = [];
    }

let of_later (a : (unit -> 'a t) list) : 'a t =
    wf { len = 0;
      len' = List.length a;
      finished = [];
      later = a;
    }

let empty : 'a t =
    of_finished []

let rec map (f : 'a -> 'b) (s : 'a t) : 'b t =
    { s with
      finished = s.finished |> List.map f;
      later = s.later |> List.map (fun gen () -> gen () |> map f);
    }

let return (x : 'a) : 'a t =
    of_finished [x]

let delay (f : unit -> 'a t) : 'a t =
    of_later [f]

let concat (s : 'a t) (s' : 'a t) : 'a t =
    wf { len = s.len + s'.len;
      len' = s.len' + s'.len';
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
        let len = sampler.len + sampler.len' in
        if sampler.len + sampler.len' = 0 then (
            Empty, sampler
        ) else (
            let idx = Random.int len in
            if idx < sampler.len then (
                (* Pick from the finished terms *)
                Picked (List.nth sampler.finished idx), sampler
            ) else (
                let idx = idx - sampler.len in
                let gen, trimmed =
                    idx
                    |> take_nth sampler.later
                in
                let res, gen =
                    gen
                    |> ( |> ) ()
                    |> try_pick
                in
                match res with
                    | Picked x -> Picked x, wf { sampler with later = (fun () -> gen) :: trimmed }
                    | Retry -> Retry, wf { sampler with later = (fun () -> gen) :: trimmed }
                    | Empty -> Retry, wf { sampler with later = trimmed; len' = sampler.len' - 1 }
            )
        )
    in
    let rec pick (miss : int) (sampler : 'a t) : 'a * 'a t =
        let res, trimmed = try_pick sampler in
        match res with
        | Picked t -> (
            Format.printf "Found after %d misses\n%!" miss;
            t, trimmed
        )
        | Retry -> pick (miss + 1) trimmed
        | Empty -> failwith "This generator is empty; no such term exists"
    in
    let rec seq (sampler : 'a t) () =
        let res, trimmed = pick 0 sampler in
        Seq.Cons (res, seq trimmed)
    in
    seq s

