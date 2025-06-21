include Choice.Intf

(** We demand that [MSeq.run e] returns the finite list of solutions for [e],
    for example
    {[
      (let open MSeq in
       sum [ return 1; return 2; return 3 ] )
      |> MSeq.run (* list of all solutions *)
      |> List.of_seq
    ]}
    should be the list [[1; 2; 3]] -- or maybe the same elements in some other
    order. *)
