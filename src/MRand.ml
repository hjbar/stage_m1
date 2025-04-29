type 'a t =
  | Return : 'a -> 'a t
  | Fail : 'a t
  | Delay : 'a t Lazy.t -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Sum : 'a t list -> 'a t
  | One_of : 'a array -> 'a t

let rec clearly_empty : type a . a t -> bool = function
  | Return _ -> false
  | Fail -> true
  | Delay f ->
    Lazy.is_val f && clearly_empty (Lazy.force f)
  | Map (t, _f) -> clearly_empty t
  | Bind (t, _f) -> clearly_empty t
  | One_of arr -> arr = [| |]
  | Sum li -> li = []

let return v = Return v
let fail = Fail
let delay f = Delay (Lazy.from_fun f)
let map f t = Map (t, f)
let bind t f = Bind (t, f)
let sum ts =
  Sum (List.filter (fun t -> not (clearly_empty t)) ts)

let one_of arr = One_of arr

let rec next : type a . a t -> a option * a t = fun t -> match t with
  | Return x -> Some x, t
  | Fail -> None, t
  | Delay f -> next (Lazy.force f)
  | Map (t, f) ->
    let o, t = next t in Option.map f o, Map (t, f)
  | One_of arr ->
    if arr = [| |] then None, Fail
    else Some arr.(Random.int (Array.length arr)), t
  | Sum ts ->
    if ts = [] then None, Fail
    else
      let ts = List.filter (fun t -> not (clearly_empty t)) ts in
      (* TODO *)
      fst (next (List.nth ts (Random.int (List.length ts)))), Sum ts
  | Bind (tt, f) ->
    let (o, tt) = next tt in
    match o with
    | None -> None, Bind (tt, f)
    | Some t -> fst (next (f t)), Bind (tt, f)

let rec run (gen : 'a t) : 'a Seq.t = fun () ->
  let o, gen = next gen in
  match o with
  | None -> run gen ()
  | Some v -> Seq.Cons (v, run gen)

