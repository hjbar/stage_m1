(* This implementation of MRand is designed to never enumerate follow
   the same path through the generator twice. Ensuring this property
   is simple for most constructors, except for [Bind].

   When the naive generator must generate an element from
     [Bind(ta : 'a t, f : 'a -> 'b t)], it first generates some
   [a : 'a] from [ta], then computes [tb : 'b t] as [f a], and then
   generates some [b : 'b] from [tb].

   We cannot remove the [a] from [ta], because we generated a single
   value [b] and not all possible values in [tb], so we would lose
   information at this point. We want to keep [a] as a result of [ta],
   but then remove the [b] from the corresponding [tb].

   Our solution in this implementation is to remove [a] from [ta], but
   keep the [tb] around as an extra argument to the Bind constructor:
    [Bind : 'b t option * 'a t * ('a -> 'b t) -> 'b t]

   On further draws to this [Bind(tb, ta, f)] constructor, we always
   draw from [tb] if it is set, and we only draw from [ta] if [tb] is
   [None]. This has the defect of being fragile to "deep dead ends":
   maybe the [a] we drew is very bad and it will take a larger number
   of tries to find a value or empty it, and taking another value of
   [ta] would have worked much better, but now we are committed to
   this [tb].

   (Note: I tried to store a list of [tbs] instead of an option, and
   then randomly draw from [ta] on occasion, but this did not give
   clearly better results; there is no robust strategy to choose
   between drawing from [ta] or from one of the already-computed
   [tbs].)
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

let rec run (gen : 'a t) : 'a Seq.t =
 fun () ->
  incr tries;
  if gen = Fail then Seq.Nil
  else
    let o, gen = next gen in
    match o with
    | None -> run gen ()
    | Some v -> Seq.Cons (v, run gen)
