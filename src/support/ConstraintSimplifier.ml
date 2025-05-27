module Make (T : Utils.Functor) = struct
  open Constraint.Make (T)

  open SatConstraint.Make (T)

  type env = Unif.Env.t

  let simplify (env : env) (c : sat_constraint) : sat_constraint =
    let is_in_env v = Unif.Env.mem v env in

    let is_rank_k v k =
      Debug.debug_what_rank v env;
      Unif.Env.mem v env && (Unif.Env.repr v env).rank = k
    in

    let normalize v =
      match Unif.Env.repr v env with
      | { var; _ } -> var
      | exception Not_found -> v
    in

    let module VarSet = Constraint.Var.Set in
    let exist v s (fvs, c) : VarSet.t * sat_constraint =
      Debug.debug_what_repr_assoc v env;
      assert (Var.eq v (normalize v));

      let s =
        match Unif.Env.repr v env with
        | exception Not_found -> s
        | { structure; _ } -> structure
      in

      let fvs =
        let fvs = ref fvs in
        Option.iter
          ( Structure.iter @@ fun v ->
            let v = normalize v in
            fvs := VarSet.add v !fvs )
          s;
        !fvs
      in

      (VarSet.remove v fvs, Exist (v, s, c))
    in

    let rec simpl ~rank (bvs : VarSet.t) (c : sat_constraint) :
      VarSet.t * sat_constraint =
      match c with
      | Loc (loc, c) ->
        let vs, c = simpl ~rank bvs c in
        (vs, Loc (loc, c))
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
                let fvs', c = simpl ~rank bvs c in
                let cs' = match c with Conj cs' -> cs' | _ -> [ c ] in

                (VarSet.union fvs fvs', cs @ cs')
            end
            (VarSet.empty, []) cs
        in
        (fvs, Conj cs)
      | Eq (v1, v2) -> begin
        let v1, v2 = (normalize v1, normalize v2) in

        let res =
          if Constraint.Var.eq v1 v2 then (VarSet.empty, Conj [] (* True *))
          else if
            Unif.Env.mem v1 env && Unif.Env.mem v2 env
            && Unif.unifiable env v1 v2
          then (VarSet.empty, False)
          else (VarSet.of_list [ v1; v2 ], Eq (v1, v2))
        in

        res
      end
      | Exist (v, s, c) ->
        let v = normalize v in
        let fvs, c = simpl ~rank (VarSet.add v bvs) c in

        if is_in_env v then (fvs, c)
        else if not @@ VarSet.mem v fvs then (fvs, c)
        else exist v s (fvs, c)
      | Decode v ->
        let v = normalize v in
        (VarSet.singleton v, Decode v)
      | Do p -> (bvs, Do p)
      | DecodeScheme sch_var -> (VarSet.empty, DecodeScheme sch_var)
      | Instance (sch_var, var) ->
        let var = normalize var in
        (VarSet.singleton var, Instance (sch_var, var))
      | Let (sch_var, var, c1, c2) ->
        let var = normalize var in

        let rank_inner = rank + 1 in
        let bvs_inner = VarSet.add var bvs in
        let fvs1, c1 = simpl ~rank:rank_inner bvs_inner c1 in

        let fvs2, c2 = simpl ~rank bvs c2 in

        let fvs = fvs1 |> VarSet.remove var |> VarSet.union fvs2 in

        (fvs, add_exist ~rank:rank_inner (fvs, Let (sch_var, var, c1, c2)))
    and add_exist ~rank (fvs, c) =
      match VarSet.choose_opt fvs with
      | None -> c
      | Some v when is_rank_k v rank -> add_exist ~rank @@ exist v None (fvs, c)
      | Some v -> add_exist ~rank (VarSet.remove v fvs, c)
    in

    let rank = Unif.base_rank - 1 in
    add_exist ~rank @@ simpl ~rank VarSet.empty c
end
