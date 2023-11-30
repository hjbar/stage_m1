module Input = struct
  open Untyped

  let id_poly =
    let x = Untyped.Var.fresh "x" in
    Abs (x, Var x)

  let id_int =
    let x = Var.fresh "x" in
    let ty_int =
      let open STLC in 
      Constr (Structure.Var (TyVar.fresh "int")) in
    Abs (x, Annot (Var x, ty_int))

  let error =
    (* id_poly has an arrow type, not an atomic type,
       so there should be an error here *)
    App (id_int, id_poly)
end

module Output = struct
  let id_poly () = Typer.infer Input.id_poly

  let id_int () = Typer.infer Input.id_int

  let error () = Typer.infer Input.error
end

module Print = struct
  open struct
    let print (_cstr, result) =
      result
      |> Typer.print_result
      |> Printer.string_of_doc
      |> print_endline
  end

  let id_poly () = print @@ Output.id_poly ()
  let id_int () = print @@ Output.id_int ()
  let error () = print @@ Output.error ()
end
