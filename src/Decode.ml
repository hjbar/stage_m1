type env = Unif.Env.t

let decode (env : env) (v : Constraint.variable) : STLC.ty =
  let table = Hashtbl.create 42 in
  let rec decode (uvar : Unif.uvar) : STLC.ty =
    let s = Unif.UF.get env.store uvar in
    match s.data with
    | Some s -> STLC.Constr (Structure.map decode s)
    | None ->
      let alpha =
        try Hashtbl.find table s.var with Not_found ->
          let alpha = STLC.TyVar.fresh "α" in
          Hashtbl.add table s.var alpha;
          alpha
      in STLC.Constr (Var alpha)
  in decode (Unif.Env.uvar env v)
