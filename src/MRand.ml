let shuffle arr =
  for i = Array.length arr - 1 downto 1 do
    let j = Random.int (i + 1) in
    let t = arr.(j) in
    arr.(j) <- arr.(i);
    arr.(i) <- t
  done

type 'a t = unit -> 'a option
let map f v = fun () -> Option.map f (v ())

let return x : 'a t = fun () -> Some x

let fail = fun () -> None

let one_of (arr : 'a array) : 'a t =
  fun () ->
  if arr = [| |] then None
  else Some arr.(Random.int (Array.length arr))


let sum (li : 'a t list) : 'a t =
  let choices = Array.of_list li in
  fun () ->
    shuffle choices;
    Array.find_map (fun f -> f ()) choices

let delay (f : unit -> 'a t) : 'a t =
  fun () -> f () ()

let bind v f =
  fun () ->
  match v () with
  | None -> None
  | Some a -> f a ()

let run (gen : 'a t) : 'a Seq.t =
  Seq.forever (fun () -> gen)
  |> Seq.filter_map (fun f -> f ())
