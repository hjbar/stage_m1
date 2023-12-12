type env = Unif.Env.t

type slot =
  | Ongoing
  | Done of STLC.ty

let decode (env : env) (v : Constraint.variable) : (STLC.ty, Constraint.variable Utils.cycle) result =
  let table = Hashtbl.create 42 in
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
          | None -> Var (STLC.TyVar.fresh "α")
        )
      in
      Hashtbl.replace table s.var (Done ty);
      ty
    end
  in
  match decode (Unif.Env.uvar env v) with
  | ty -> Ok ty
  | exception (Found_cycle cy) -> Error cy
