(* The internal representation is a bit subtle here, in
   an effort to get some good performance out of the generator.

   [finished] contains instantiated ground terms, and [later] contains
   lazily computed ones, with holes. As such when we do a random
   sampling if we pick an item from [later] it may be that there
   does not exist a way to fill in the hole. In that case we will
   implement a way to remove the term from [later] so that this
   dead-end will not be picked again. *)
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

(* Takes the nth element out of the list and returns
   it and the list shorter by one.

   @raise: [Not_found] if [i > List.length l] *)
let rec take_nth (l : 'a list) (n : int) : 'a * 'a list =
    match l, n with
    | h::t, i when i <= 0 -> h, t
    | h::t, i -> let out, rem = take_nth t (i - 1) in out, h::rem
    | [], _ -> raise Not_found

let run (s : 'a t) : 'a Seq.t =
    let rec try_pick (sampler : 'a t) : 'a chosen * 'a t =
        (* Attempt to pick an element.
           This might suceed with `Picked x` with `x` the element,
           and it might fail in two different ways:
           - `Retry` is a recoverable failure.
             We found a dead-end and we're asking to restart to not
             skew the probabilities. The generator might be reentered later.
           - `Empty` is a fatal error.
             This generator is provably empty.
             The parent should completely delete this generator
             and never invoke it again. *)
        let pick_finished () =
            (* If we succesfully pick a value, we remove it from the generator.
               We could keep the value in the finished terms if we wanted to be
               able to generate an arbitrary number of terms (with duplicates). *)
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

