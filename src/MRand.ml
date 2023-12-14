(*sujet
type 'a t = MRand_not_implemented_yet

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
  Utils.not_yet "MRand.map" (f, s)

let return (x : 'a) : 'a t =
  Utils.not_yet "MRand.return" x

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
  Utils.not_yet "MRand.bind" (sa, f)

let delay (f : unit -> 'a t) : 'a t =
  Utils.not_yet "MRand.delay" (f ())

let sum (li : 'a t list) : 'a t =
  Utils.not_yet "MRand.sum" li

let fail : 'a t =
  MRand_not_implemented_yet

let one_of (vs : 'a array) : 'a t =
  Utils.not_yet "MRand.one_of" vs

let run (s : 'a t) : 'a Seq.t =
  Utils.not_yet "MRand.run" s
/sujet*)
(*corrige*)
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
(*/corrige*)
