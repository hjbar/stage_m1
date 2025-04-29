type 'a t =
  | Return : 'a -> 'a t
  | Fail : 'a t
  | Delay : 'a t Lazy.t -> 'a t
  | Map : 'a t * ('a -> 'b) -> 'b t
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Sum : 'a t list -> 'a t
  | One_of : 'a array -> 'a t

let is_empty = function
  | Fail -> true
  | _ -> false

let return v = Return v
let fail = Fail
let delay f = Delay (Lazy.from_fun f)
let map f t =
  if is_empty t then Fail
  else Map (t, f)
let bind t f =
  if is_empty t then Fail
  else Bind (t, f)

let sum ts =
  match List.filter (fun t -> not (is_empty t)) ts with
  | [] -> Fail
  | ts -> Sum ts

let one_of arr = One_of arr

let rec next : type a . a t -> a option * a t = fun t -> match t with
  | Return x -> Some x, t
  | Fail -> None, t
  | Delay f -> next (Lazy.force f)
  | Map (t, f) ->
    let o, t = next t in Option.map f o, map f t
  | One_of arr ->
    if arr = [| |] then None, Fail
    else Some arr.(Random.int (Array.length arr)), t
  | Sum ts ->
    if ts = [] then None, Fail
    else begin
      let arr = Array.of_list ts in
      let i = Random.int (Array.length arr) in
      let (o, t) = next arr.(i) in
      arr.(i) <- t;
      o, sum (Array.to_list arr)
    end
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

