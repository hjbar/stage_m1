(** Exhaustive term generation will be as lazy as possible so that
   [minigen --exhaustive --depth $A_LOT --count $NOT_MUCH] is computationally
   feasible.
   [Seq] is a natural base constructor to use since it is also the final type
   and we don't need fast access ever, and there are ways to express terms
   lazily. *)
type 'a t =
    (* Base case: finished generation *)
    | Done : 'a Seq.t -> 'a t
    (* Explicit laziness (required by the user) *)
    | Delay : (unit -> 'a t) -> 'a t
    (* Other forms of laziness (implementation detail) *)
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Sum : 'a t Seq.t -> 'a t

    (** [return], [fail], [sum] and [one_of] are trivially given by [Seq] *)
let return (x : 'a) : 'a t =
    Done Seq.(return x)

let fail : 'a t =
    Done Seq.empty

let sum (li : 'a t list) : 'a t =
    Sum List.(to_seq li)

let one_of (vs : 'a array) : 'a t =
    Done Array.(to_seq vs)

(** [bind] is easy here thanks to laziness but we will pay the price at [run]time. *)
let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
    Bind (sa, f)

(** Standard implementation of [map] from [bind] and [return] *)
let map (f : 'a -> 'b) (s : 'a t) : 'b t =
    bind s (fun x -> x |> f |> return)

(** Explicit laziness *)
let delay (f : unit -> 'a t) : 'a t =
    Delay f

(** Big thanks to [Seq] for having enough features that this is easy. *)
let rec run : type a . a t -> a Seq.t = function
    | Done l -> l
    | Delay f -> f () |> run
    | Bind (s, f) -> s |> run |> Seq.concat_map (fun x -> x |> f |> run)
    | Sum ls -> ls |> Seq.concat_map run
