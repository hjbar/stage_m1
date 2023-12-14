include Utils.MonadPlus

val delay : (unit -> 'a t) -> 'a t

val pair : 'a t -> 'b t -> ('a * 'b) t

val one_of : 'a array -> 'a t

val forever : 'a t -> 'a Seq.t

val run : 'a t -> 'a option
