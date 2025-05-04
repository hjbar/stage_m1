(* This implementation builds on top of "full removal".

   When drawing from a sub-value of type ['a t] fails, we try to
   redraw from this value instead of propagating the failure to the
   toplevel and failing there.

   This strategy can considerably reduce the number of retries
   necessary to generate a term. On the other hand, sometimes it get
   stucks in dead ends coming from bad earlier choices, and becomes
   much worse than retrying at the top.

   Finally, this strategy must build on top of a "full removal"
   implementation, which guarantees that empty generators eventually
   become [Fail] after a certain number of retries. Otherwise it could
   loop on subterms that are empty but never reduce to [Fail].
*)

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

let map f t =
  if t = Fail then Fail else Map (t, f)

let sum ts = Sum (List.filter ((<>) fail) ts)
let sum_cons t ts = if t = Fail then Sum ts else Sum (t :: ts)

let bind ta f =
  if ta = Fail then Fail else Bind (None, ta, f)

let bind_cons tb ta f =
  if tb = Fail then bind ta f
  else Bind (Some tb, ta, f)

let shuffle arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = arr.(j) in
    arr.(j) <- arr.(i);
    arr.(i) <- tmp
  done

let one_of arr = shuffle arr; One_of (Array.to_list arr)
let one_of_list = function
  | [] -> Fail
  | li -> One_of li

let list_pop_rand = function
  | [] -> None
  | li ->
    let len = List.length li in
    let i = Random.int len in
    Some (List.nth li i, List.filteri (fun j _ -> j <> i) li)

let tries = ref 1

let rec next : type a . a t -> a option * a t = fun t ->
  let retry o rest =
    if o <> None || rest = Fail then o, rest
    else (incr tries; next rest)
  in
  match t with
  | Return x ->
    Some x, Fail
  | Fail ->
    None, Fail
  | Delay f ->
    next (Lazy.force f)
  | Map (t, f) ->
    let o, t = next t in
    retry (Option.map f o) (map f t)
  | One_of li ->
    begin match list_pop_rand li with
    | None -> None, Fail
    | Some (a, li) -> Some a, one_of_list li
    end
  | Sum ts ->
    begin match list_pop_rand ts with
    | None -> None, Fail
    | Some (t, ts) ->
      let (o, t) = next t in
      retry o (sum_cons t ts)
    end
  | Bind (Some tb, ta, f) ->
    let (ob, tb) = next tb in
    retry ob (bind_cons tb ta f)
  | Bind (None, ta, f) ->
    let (oa, ta) = next ta in
    begin match oa with
    | None -> retry None (bind ta f)
    | Some a -> next (bind_cons (f a) ta f)
    end

let rec run (gen : 'a t) : 'a Seq.t = fun () ->
  incr tries;
  if gen = Fail then Seq.Nil
  else
    let o, gen = next gen in
    match o with
    | None -> run gen ()
    | Some v -> Seq.Cons (v, run gen)

