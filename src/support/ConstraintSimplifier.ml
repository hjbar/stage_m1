module Make (T : Utils.Functor) = struct
  (* Open some stuff *)

  open Constraint.Make (T)

  open SatConstraint.Make (T)

  type env = Unif.Env.t

  module VarSet = Constraint.Var.Set

  (* Simplify the constraint thanks to the unify's env *)
  let simplify (env : env) (c : sat_constraint) : sat_constraint =
    (* Some helper functions *)

    (* v is in unify's env ? *)
    let is_in_env v = Unif.Env.mem v env in

    (* v is register at rank k in the unify's env ? *)
    let is_rank_k v k = Unif.Env.mem v env && (Unif.Env.repr v env).rank = k in

    (* Returns true if v1 and v2 are not unifiable, false otherwise *)
    let is_not_unifiable v1 v2 =
      Unif.Env.mem v1 env && Unif.Env.mem v2 env
      && not (Unif.unifiable env v1 v2)
    in

    (* Returns the represent of v if exists, v otherwise *)
    let normalize v =
      match Unif.Env.repr v env with
      | { var; _ } -> var
      | exception Not_found -> v
    in

    (* Add annotation for structure of a variable in a constraint *)
    let annot_structure v c =
      match Unif.Env.repr v env with
      | { structure = None; _ } | (exception Not_found) -> c
      | { structure = Some s; _ } ->
        let ws = Constraint.Var.fresh @@ Constraint.Var.name v ^ "'" in
        Exist (ws, Some s, Eq (v, ws))
    in

    (* Bind existential v to the constraint and remove v from the free variables of the constraint *)
    let exist v s (fvs, c) : VarSet.t * sat_constraint =
      assert (Var.eq v @@ normalize v);

      let s =
        match Unif.Env.repr v env with
        | exception Not_found -> s
        | { structure; _ } -> structure
      in

      let fvs =
        Option.fold ~none:fvs
          ~some:
            begin
              Structure.fold
                begin
                  fun fvs v ->
                    let v = normalize v in
                    VarSet.add v fvs
                end
                fvs
            end
          s
      in

      (VarSet.remove v fvs, Exist (v, s, c))
    in

    (*
      Simplify the constraint
      rank is the actual level of the program, i.e. number of let
      bvs is the bind variables
      lvs is the variables binded by a let
      returns the free variables of the constraint and the simplified constraint
    *)
    let rec simpl ~(rank : Unif.rank) ~(bvs : VarSet.t) ~(lvs : VarSet.t)
      (c : sat_constraint) : VarSet.t * sat_constraint =
      match c with
      | Loc (loc, c) ->
        let fvs, c = simpl ~rank ~bvs ~lvs c in
        (fvs, Loc (loc, c))
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
                let fvs', c = simpl ~rank ~bvs ~lvs c in
                let cs' = match c with Conj cs' -> cs' | _ -> [ c ] in

                (VarSet.union fvs fvs', cs @ cs')
            end
            (VarSet.empty, []) cs
        in
        (fvs, Conj cs)
      | Eq (v1, v2) -> begin
        let v1, v2 = (normalize v1, normalize v2) in

        if Constraint.Var.eq v1 v2 then (VarSet.empty, Conj []) (* True *)
        else if is_not_unifiable v1 v2 then (VarSet.empty, False)
        else (VarSet.of_list [ v1; v2 ], Eq (v1, v2))
      end
      | Exist (v, s, c) ->
        let v = normalize v in
        let bvs = VarSet.add v bvs in
        let fvs, c = simpl ~rank ~bvs ~lvs c in

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
        let lvs = VarSet.add var lvs in

        let rank_inner = rank + 1 in
        let bvs_inner = VarSet.add var bvs in
        let fvs1, c1 = simpl ~rank:rank_inner ~bvs:bvs_inner ~lvs c1 in

        let fvs1 = VarSet.remove var fvs1 in
        let fvs1, c1 =
          add_exist ~rank:rank_inner (fvs1, annot_structure var c1)
        in

        let fvs2, c2 = simpl ~rank ~bvs ~lvs c2 in

        let fvs = VarSet.union fvs1 fvs2 in
        add_exist ~rank (fvs, Let (sch_var, var, c1, c2))
    (*
      Add existential variables of this rank
      rank is the actual level of the program
      fvs is the free variables of the constraint
      c is the constraint that we want to add existentials
      returns the free variables not bounded by existentials and the new simplified constraint
    *)
    and add_exist ~(rank : Unif.rank) ((fvs, c) : VarSet.t * sat_constraint) :
      VarSet.t * sat_constraint =
      match VarSet.choose_opt fvs with
      | None -> (VarSet.empty, c)
      | Some v when is_rank_k v rank -> add_exist ~rank @@ exist v None (fvs, c)
      | Some v ->
        let fvs, c = add_exist ~rank (VarSet.remove v fvs, c) in
        (VarSet.add v fvs, c)
    in

    (* Compute the simplified constraint *)
    let rank = Unif.base_rank - 1 in
    let bvs = VarSet.empty in
    let lvs = VarSet.empty in
    let _fvs, c = add_exist ~rank @@ simpl ~rank ~bvs ~lvs c in
    c
end
