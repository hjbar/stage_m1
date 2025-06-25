(* This implementation builds on top of "local retries".

   We count the number of retries. When it exceeds a certain number of
   tries, we redraw from the original, toplevel generator.

   This avoids two related issues:

   1. We avoid getting stuck in local retries in dead parts of the
      search space.

   2. We avoid getting stuck in the cached [tb] value of a
      [Bind(tb, ta, f)] constructor, if it itself is a dead end.

   On the other hand, the behavior relies on a magic constant -- the
   limit after which we reset to the toplevel generator. Setting the
   constant too low makes the generator unable to finish generating
   very large terms. Setting the constant too high makes the reset
   strategy ineffective.
*)
let limit = 1000

type 'a t =
  | Return : 'a -> 'a t
  | Fail : 'a t
  | Delay : 'a t Lazy.t -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Bind : 'b t option * 'a t * ('a -> 'b t) -> 'b t
  | Sum : 'a t list -> 'a t
  | One_of : 'a list -> 'a t

let return v = Return v

let fail = Fail

let delay f = Delay (Lazy.from_fun f)

let map f t = if t = Fail then Fail else Map (t, f)

let sum ts = Sum (List.filter (( <> ) fail) ts)

let sum_cons t ts = if t = Fail then Sum ts else Sum (t :: ts)

let bind ta f = if ta = Fail then Fail else Bind (None, ta, f)

let bind_cons tb ta f = if tb = Fail then bind ta f else Bind (Some tb, ta, f)

let shuffle arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(j) in
    arr.(j) <- arr.(i);
    arr.(i) <- tmp
  done

let one_of arr =
  shuffle arr;
  One_of (Array.to_list arr)

let one_of_list = function [] -> Fail | li -> One_of li

let list_pop_rand = function
  | [] -> None
  | li ->
    let len = List.length li in
    let i = Random.int len in
    Some (List.nth li i, List.filteri (fun j _ -> j <> i) li)

let tries = ref 1

let start = ref !tries

exception Reset

let rec next : type a. a t -> a option * a t =
 fun t ->
  let retry o rest =
    if o <> None || rest = Fail then (o, rest)
    else if !tries - !start > limit then raise Reset
    else (
      incr tries;
      next rest )
  in
  match t with
  | Return x -> (Some x, Fail)
  | Fail -> (None, Fail)
  | Delay f -> next (Lazy.force f)
  | Map (t, f) ->
    let o, t = next t in
    retry (Option.map f o) (map f t)
  | One_of li -> begin
    match list_pop_rand li with
    | None -> (None, Fail)
    | Some (a, li) -> (Some a, one_of_list li)
  end
  | Sum ts -> begin
    match list_pop_rand ts with
    | None -> (None, Fail)
    | Some (t, ts) ->
      let o, t = next t in
      retry o (sum_cons t ts)
  end
  | Bind (Some tb, ta, f) ->
    let ob, tb = next tb in
    retry ob (bind_cons tb ta f)
  | Bind (None, ta, f) ->
    let oa, ta = next ta in
    begin
      match oa with
      | None -> retry None (bind ta f)
      | Some a -> next (bind_cons (f a) ta f)
    end

let rec run orig_gen =
  start := !tries;
  let rec run_ (gen : 'a t) : 'a Seq.t =
   fun () ->
    incr tries;
    if gen = Fail then Seq.Nil
    else
      match next gen with
      | exception Reset -> run orig_gen ()
      | None, gen -> run_ gen ()
      | Some v, gen -> Seq.Cons (v, run gen)
  in
  run_ orig_gen
