(* See [Mrand_full_removal.ml] for the core implementation,
   and [Mrand_local_retries_with_reset.ml] for reset. *)
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

let one_of arr = One_of (Array.to_list arr)

let one_of_list = function
  | [] -> Fail
  | li -> One_of li


let list_pop_rand = function
  | [] -> None
  | li ->
    let len = List.length li in
    let i = Random.int len in
    Some (List.nth li i, List.filteri (fun j _ -> j <> i) li)


let rec next : type a. a t -> a option * a t =
 fun t ->
  match t with
  | Return x -> (Some x, Fail)
  | Fail -> (None, Fail)
  | Delay f -> next (Lazy.force f)
  | Map (t, f) ->
    let o, t = next t in
    (Option.map f o, map f t)
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
      (o, sum_cons t ts)
  end
  | Bind (Some tb, ta, f) ->
    let ob, tb = next tb in
    (ob, bind_cons tb ta f)
  | Bind (None, ta, f) ->
    let oa, ta = next ta in
    begin
      match oa with
      | None -> (None, bind ta f)
      | Some a -> next (bind_cons (f a) ta f)
    end


let tries = ref 1

let rec run orig_gen =
  let start = !tries in
  let rec run_ (gen : 'a t) : 'a Seq.t =
   fun () ->
    incr tries;
    if gen = Fail then Seq.Nil
    else
      match next gen with
      | None, gen ->
        if !tries - start > limit then run orig_gen () else run_ gen ()
      | Some v, gen -> Seq.Cons (v, run gen)
  in
  run_ orig_gen
