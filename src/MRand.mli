include Choice.Intf

(** We demand that [MRand.run e] returns an infinite sequence of solutions
    sampled from [e], for example
    {[
      (let open MRand in
       sum [ return 1; return 2; return 3 ] )
      |> MRand.run (* infinite stream of random samples *)
      |> Seq.take 10 (* take the first 10 elements *)
      |> List.of_seq
    ]}
    could be the list [[1; 2; 1; 2; 1; 1; 1; 3; 3; 2]]. *)
