type path = ChoicePath.t
open ChoicePath

let invalid_path f path =
  ignore path;
  Printf.ksprintf failwith "MSeq.%s: invalid path '%s'"
    f
    (ChoicePathPrinter.print path
     |> Utils.string_of_doc)

type 'a t = path -> (path * 'a) Seq.t

let map f s = fun p ->
  s p
  |> Seq.map (fun (p', v) -> (p', f v))

let delay ds = fun p -> ds () p

let return v : 'a t = fun p ->
  match p with
  | Nil | Return ->
    Seq.return (Return, v)
  | Fail | Sum _ | Bind _ ->
    invalid_path "return" p

let bind s f = fun p ->
  let pa, first_pb = match p with
    | Nil -> (Nil, Nil)
    | Bind (pa, pb) -> (pa, pb)
    | Fail | Return | Sum _ ->
      invalid_path "bind" p
  in
  s pa |> Seq.mapi (fun i (pa', sa) ->
    (pa', (if i = 0 then first_pb else Nil), sa)
  ) |> Seq.concat_map @@ fun (pa', pb, va) ->
  f va pb |> Seq.map @@ fun (pb', vb) ->
  (Bind (pa', pb'), vb)

let fail = fun p ->
  match p with
  | Nil | Fail -> Seq.empty
  | Return | Sum _ | Bind _ ->
    invalid_path "fail" p

let one_of arr = fun p ->
  let start = match p with
    | Nil -> 0
    | Sum (i, Return) -> i
    | Return | Fail | Sum _ | Bind _ ->
      invalid_path "one_of" p
  in
  Seq.ints start
  |> Seq.take_while (fun i -> i < Array.length arr)
  |> Seq.map (fun i -> (Sum (i, Return), arr.(i)))

let sum ss = fun p ->
  let start, start_p = match p with
    | Nil -> 0, Nil
    | Sum (i, i_p) -> (i, i_p)
    | Return | Fail | Bind _ ->
      invalid_path "sum" p
  in
  ss
  |> List.to_seq
  |> Seq.mapi (fun i s -> (i, s))
  |> Seq.concat_map (fun (i, s) ->
    Seq.map (fun (p, v) -> (Sum (i, p), v)) @@
    if i < start then Seq.empty
    else if i = start then s start_p
    else s Nil
  )

let run s =
  s Nil
  |> Seq.map snd

let run' s = fun p -> s p
