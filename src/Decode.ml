type env = Unif.Env.t

type slot =
  | Ongoing
  | Done of Typed.ty

let new_var = Typed.TyVar.namegen [| "α"; "β"; "γ"; "δ" |]

type decoder = Constraint.variable -> Typed.ty

let decode (env : env) () : decoder =
  let table = Hashtbl.create 16 in

  fun v ->
    let exception Found_cycle of Constraint.variable Utils.cycle in
    let rec decode (v : Constraint.variable) : Typed.ty =
      let repr = Unif.Env.repr v env in

      match Hashtbl.find table repr.var with
      | Done ty -> ty
      | Ongoing -> raise @@ Found_cycle (Utils.Cycle repr.var)
      | exception Not_found ->
        Hashtbl.replace table repr.var Ongoing;

        let ty =
          Typed.Constr
            ( match repr.structure with
            | Some s -> Structure.map decode s
            | None -> Var (new_var ()) )
        in

        Hashtbl.replace table repr.var (Done ty);
        ty
    in

    (* Because we perform an occur-check on unification, we can assume
       that we never find any cycle during decoding:
       [Found_cycle] should never be raised here. *)
    decode v
