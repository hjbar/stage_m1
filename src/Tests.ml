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

  let curry =
    let f = Untyped.Var.fresh "f" in
    let x = Untyped.Var.fresh "x" in
    let y = Untyped.Var.fresh "y" in
    Abs (f, Abs (x, Abs (y, App (Var f, Tuple [Var x; Var y]))))

  let uncurry =
    let f = Untyped.Var.fresh "f" in
    let p = Untyped.Var.fresh "p" in
    let x = Untyped.Var.fresh "x" in
    let y = Untyped.Var.fresh "y" in
    Abs (f, Abs (p, LetTuple ([x; y], Var p,
      App (App (Var f, Var x), Var y)
    )))
end

module Output = struct
  open struct
    let infer term =
      let cst, (logs, result) = Typer.infer ~log:true term in
      PPrint.(separate hardline logs)
      |> Printer.string_of_doc
      |> print_endline;
      cst, result
  end

  let id_poly () = infer Input.id_poly

  let id_int () = infer Input.id_int

  let error () = infer Input.error

  let curry () = infer Input.curry

  let uncurry () = infer Input.uncurry
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
  let curry () = print @@ Output.curry ()
  let uncurry () = print @@ Output.uncurry ()
end
