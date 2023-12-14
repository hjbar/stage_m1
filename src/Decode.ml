type env = Unif.Env.t

type slot =
  | Ongoing
  | Done of STLC.ty

let new_var =
  Utils.namegen STLC.TyVar.fresh [|"α"; "β"; "γ"; "δ"|]

let table = Hashtbl.create 42

let decode (env : env) (v : Constraint.variable) : STLC.ty =
  let exception Found_cycle of Constraint.variable Utils.cycle in
  let rec decode (uvar : Unif.uvar) : STLC.ty =
    let s = Unif.UF.get env.store uvar in
    begin match Hashtbl.find table s.var with
    | Done ty -> ty
    | Ongoing -> raise (Found_cycle (Utils.Cycle s.var))
    | exception Not_found ->
      Hashtbl.replace table s.var Ongoing;
      let ty =
        STLC.Constr (
          match s.data with
          | Some s -> Structure.map decode s
          | None -> Var (new_var ())
        )
      in
      Hashtbl.replace table s.var (Done ty);
      ty
    end
  in
  (* Because we now perform an occur-check on unification,
     we can assume that we never find any cycle during decoding:
     [Found_cycle] should never be raised here. *)
  decode (Unif.Env.uvar env v)
