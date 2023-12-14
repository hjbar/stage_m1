(*sujet
type 'a t = MSeq_not_implemented_yet

let map (f : 'a -> 'b) (s : 'a t) : 'b t =
  Utils.not_yet "MSeq.map" (f, s)

let return (x : 'a) : 'a t =
  Utils.not_yet "MSeq.return" x

let bind (sa : 'a t) (f : 'a -> 'b t) : 'b t =
  Utils.not_yet "MSeq.bind" (sa, f)

let delay (f : unit -> 'a t) : 'a t =
  Utils.not_yet "MSeq.delay" (f ())

let sum (li : 'a t list) : 'a t =
  Utils.not_yet "MSeq.sum" li

let fail : 'a t =
  MSeq_not_implemented_yet

let one_of (vs : 'a array) : 'a t =
  Utils.not_yet "MSeq.one_of" vs

let run (s : 'a t) : 'a Seq.t =
  Utils.not_yet "MSeq.run" s
/sujet*)
(*corrige*)
type 'a t = 'a Seq.t
let map = Seq.map
            
let delay f = fun () -> f () ()

let return x = Seq.return x
let bind s f = Seq.concat_map f s

let fail = Seq.empty
let one_of = Array.to_seq
let sum seqs = seqs |> List.to_seq |> Seq.concat

let run s = s
(*/corrige*)
