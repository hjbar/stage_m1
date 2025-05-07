module Make (T : Utils.Functor) = struct
  open Constraint.Make (T)

  open SatConstraint.Make (T)

  type env = Unif.Env.t

  let simplify (env : env) (c : sat_constraint) : sat_constraint =
    let is_in_env v = Unif.Env.mem v env in

    let normalize v =
      match Unif.Env.repr v env with
      | { var; _ } -> var
      | exception Not_found -> v
    in

    let module VarSet = Constraint.Var.Set in
    let exist v s (fvs, c) : VarSet.t * sat_constraint =
      assert (Var.eq v (normalize v));

      let s =
        match Unif.Env.repr v env with
        | exception Not_found -> s
        | { structure; _ } -> structure
      in

      let fvs =
        let fvs = ref fvs in
        Option.iter (Structure.iter @@ fun v -> fvs := VarSet.add v !fvs) s;
        !fvs
      in

      (VarSet.remove v fvs, Exist (v, s, c))
    in

    let rec simpl (bvs : VarSet.t) (c : sat_constraint) :
      VarSet.t * sat_constraint =
      match c with
      | False ->
        (* Note: we do not attempt to normalize (⊥ ∧ C) into ⊥, (∃w. ⊥)
           into ⊥, etc. If a contradiction appears in the constraint, we
           think that it is clearer to see it deep in the constraint
           term, in the context where the solver found it, rather than
           bring it all the way to the top and erasing the rest of the
           constraint in the process.  *)
        (VarSet.empty, False)
      | Conj cs ->
        let fvs, cs =
          List.fold_left
            begin
              fun (fvs, cs) c ->
                let fvs', c = simpl bvs c in
                let cs' = match c with Conj cs' -> cs' | _ -> [ c ] in

                (VarSet.union fvs' fvs, cs' @ cs)
            end
            (VarSet.empty, []) cs
        in
        (fvs, Conj cs)
      | Eq (v1, v2) -> begin
        let v1, v2 = (normalize v1, normalize v2) in

        if Constraint.Var.eq v1 v2 then (VarSet.empty, Conj [] (* True *))
        else
          match Unif.unifiable env v1 v2 with
          | false -> (VarSet.empty, False)
          | true | (exception Not_found) ->
            (VarSet.of_list [ v1; v2 ], Eq (v1, v2))
      end
      | Exist (v, s, c) ->
        let v = normalize v in
        let fvs, c = simpl (VarSet.add v bvs) c in

        if is_in_env v then (fvs, c)
        else if not (VarSet.mem v fvs) then (fvs, c)
        else exist v s (fvs, c)
      | Decode v ->
        let v = normalize v in
        (VarSet.singleton v, Decode v)
      | Do p -> (bvs, Do p)
      | DecodeScheme sch_var ->
        (* TODO: normalize sch_var *)
        (VarSet.empty, DecodeScheme sch_var)
      | Instance (sch_var, var) ->
        (* TODO: normalize sch_var *)
        let var = normalize var in
        (VarSet.singleton var, Instance (sch_var, var))
      | Let (sch_var, var, c1, c2) ->
        (* TODO: normalize sch_var *)
        let var = normalize var in

        let bvs', c1 = simpl bvs c1 in
        let bvs_inner = bvs |> VarSet.add var |> VarSet.union bvs' in

        let bvs_inner', c2 = simpl bvs_inner c2 in
        let bvs_inner_union = VarSet.union bvs_inner bvs_inner' in

        (bvs_inner_union, Let (sch_var, var, c1, c2))
    in

    let rec add_exist (fvs, c) =
      match VarSet.choose_opt fvs with
      | None -> c
      | Some v -> add_exist (exist v None (fvs, c))
    in

    add_exist (simpl VarSet.empty c)
end
