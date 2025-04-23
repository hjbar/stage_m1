type 'a t = 'a Seq.t

let map = Seq.map

let delay f = fun () -> f () ()

let return = Seq.return

let bind s f = Seq.concat_map f s

let fail = Seq.empty

let one_of = Array.to_seq

let sum seqs = seqs |> List.to_seq |> Seq.concat

let run = Fun.id
