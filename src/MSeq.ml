(** Exhaustive term generation will be as lazy as possible so that
   [minigen --exhaustive --depth $A_LOT --count $NOT_MUCH] is computationally
   feasible.
   [Seq] is a natural base constructor to use since it is also the final type
   and we don't need fast access ever, and there are ways to express terms
   lazily. *)

(** Big thanks to [Seq] for having enough features that this is easy. *)
type 'a t = 'a Seq.t

(** [return], [fail], [sum] and [one_of] are trivially given by [Seq] *)
let return (x : 'a) : 'a t =
  Seq.(return x)
      
let fail : 'a t =
  Seq.empty

let sum (li : 'a t list) : 'a t =
  List.(to_seq li) |> Seq.concat

let one_of (vs : 'a array) : 'a t =
  Array.(to_seq vs)

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
  Seq.concat_map f sa

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
  Seq.map f s

(** Explicit laziness *)
let delay (f : unit -> 'a t) : 'a t =
  (fun () -> f () ())

let run : type a . a t -> a Seq.t = fun s -> s
