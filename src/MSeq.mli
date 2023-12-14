include Utils.MonadPlus

val delay : (unit -> 'a t) -> 'a t

val all : 'a t -> 'a Seq.t
