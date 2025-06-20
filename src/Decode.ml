type env = Unif.Env.t

type slot =
  | Ongoing
  | Done of F.ty

let new_var = F.TyVar.namegen [| "α"; "β"; "γ"; "δ" |]

type decoder = Constraint.variable -> F.ty

let decode (env : env) () : decoder =
  let table = Hashtbl.create 42 in

  fun v ->
    let exception Found_cycle of Constraint.variable Utils.cycle in
    let rec decode (v : Constraint.variable) : F.ty =
      let repr = Unif.Env.repr v env in

      match Hashtbl.find table repr.var with
      | Done ty -> ty
      | Ongoing -> raise @@ Found_cycle (Utils.Cycle repr.var)
      | exception Not_found ->
        Hashtbl.replace table repr.var Ongoing;

        let ty =
          F.Constr
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
