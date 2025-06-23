(* Import some modules *)

module Env = Unif.Env
module IntMap = Unif.IntMap
module Var = Constraint.Var

(* Import some types *)

type variable = Unif.var

type structure = Unif.structure

type rank = Unif.rank

type status = Unif.status =
  | Flexible
  | Generic

type repr = Unif.repr

type env = Unif.Env.t

type err = Unif.err

(* Import some values *)

let base_rank = Unif.base_rank

(* Adjust the rank of the variable's representative
   when it is flexible, i.e., not yet generalized *)

let adjust_rank (repr : repr) (k : rank) (env : env) : env * repr =
  assert (repr.status <> Generic);

  let repr = { repr with rank = min repr.rank k } in
  let env = Env.set repr env in

  (env, repr)

(* Create a new flexible variable in the environment *)

let fresh_flexible ?(name = "fresh") (structure : structure) (env : env) :
  env * variable =
  let var = Var.fresh name in
  let env = Env.add_flexible var structure env in
  (env, var)

(* Set up the environment before entering a new level *)

let enter (env : env) : env =
  let env = Env.incr_young env in
  let young = Env.get_young env in

  assert (Env.pool_k_is_empty young env);
  env

(* The set of all young variables. A variable is considered
   young if it is currently registered in the youngest pool.
   This implies that its rank is at most [state.young]. *)

type generation =
  { inhabitants : variable list (* All young variables *)
  ; by_rank : variable list IntMap.t (* All young variables indexed by rank *)
  ; is_young : variable -> bool
      (* True if the given variable belongs to the same
     equivalence class as some young variable v' *)
  }

(* Before exiting, create a generation describing the young generation *)

let discover_young_generation (env : env) : generation =
  (* Young level of the env *)
  let state_young = Env.get_young env in

  (* The most recent pool holds the list of
     all variables in the young generation *)
  let inhabitants = Env.get_pool state_young env in

  (* Cache to track which equivalence classes are considered young *)
  let cache = Hashtbl.create 16 in

  (* Compute the young generation, grouping variables by rank *)
  let by_rank =
    List.fold_left
      begin
        fun by_rank var ->
          Hashtbl.replace cache var ();

          let repr = Env.repr var env in
          let r = repr.rank in

          assert (repr.status <> Generic);
          assert (base_rank <= r && r <= state_young);

          let l = Option.value ~default:[] @@ IntMap.find_opt r by_rank in
          let by_rank = IntMap.add r (var :: l) by_rank in

          by_rank
      end
      IntMap.empty inhabitants
  in

  (* Returns true if [var] is part of the young generation *)
  let is_young var = Hashtbl.mem cache var in

  (* Final result *)
  { inhabitants; by_rank; is_young }

(* Update the rank of every variable in the young generation *)

let update_ranks (generation : generation) (env : env) : env =
  (* Cache to mark visited variables *)
  let cache = Hashtbl.create 16 in

  (* Accumulator for the final environment *)
  let env = ref env in

  (* Update the environment for each rank from base to young *)
  for k = base_rank to Env.get_young !env do
    (* Recursively compute the updated rank for a variable *)
    let rec traverse (var : variable) : rank =
      (* Get the representative of the variable and check its status *)
      let repr = Env.repr var !env in
      assert (repr.status <> Generic);

      (* If we've already visited this variable, return its current rank *)
      if Hashtbl.mem cache repr.var then begin
        assert (repr.rank <= k);
        repr.rank
      end
      else begin
        (* Mark this variable as visited *)
        Hashtbl.replace cache repr.var ();

        (* Update the variable’s rank and record it in the environment *)
        let new_env, repr = adjust_rank repr k !env in
        env := new_env;

        (* If the variable is not young, stop traversal *)
        if not (generation.is_young var) then repr.rank
        else begin
          (* The variable must have rank [k] at this point *)
          assert (repr.rank = k);

          (* If the variable has no structure, stop *)
          if repr.structure = None then repr.rank
          else begin
            (* Recursively traverse all children and compute max rank *)
            let max_rank =
              Structure.fold
                (fun max_rank child -> max max_rank (traverse child))
                base_rank
                (Option.get repr.structure)
            in

            (* Adjust this variable's rank based on its children *)
            let new_env, repr = adjust_rank repr max_rank !env in
            env := new_env;

            (* Return the new rank of the variable's rank *)
            repr.rank
          end
        end
      end
    in

    (* Apply traversal to all variables of rank [k] *)
    List.iter
      (fun var -> traverse var |> ignore)
      (Option.value ~default:[] @@ IntMap.find_opt k generation.by_rank)
  done;

  (* Return the updated environment *)
  !env

(* Return a list of variables that have become generic *)

let generalize (generation : generation) (env : env) : env * variable list =
  (* Initialize *)
  let state_young = Env.get_young env in
  let env = ref env in

  (* Compute the list of generic variables *)
  let l =
    List.filter
      begin
        fun var ->
          (* Ignore variables that are not representatives of their equivalence class *)
          Env.is_representative var !env
          && begin
               let repr = Env.repr var !env in
               let rank = repr.rank in

               (* If rank is less than state_young,
               re-register at the lower level and drop *)
               if rank < state_young then begin
                 env := Env.register var ~rank !env;
                 false
               end
               else begin
                 (* If rank equals state_young, make var generic
                 and drop it if it has structure *)
                 assert (rank = state_young);

                 let repr = { repr with status = Generic } in
                 env := Env.set repr !env;

                 repr.structure = None
               end
             end
      end
      generation.inhabitants
  in

  (* Return the updated environment and the list of generic variables *)
  (!env, l)

(* Representation of a scheme *)

type scheme =
  { root : variable (* A root variable: must be generic *)
  ; generics : variable list (* All generic variables of the scheme *)
  ; quantifiers : variable list
      (* All generic variables that have no structure *)
  }

(* Utility function: get the "body" of a scheme *)

let body { root; _ } = root

(* Utility function: get the quantifiers of a scheme *)

let quantifiers { quantifiers; _ } = quantifiers

(* Utility function: get the debug representation of a scheme *)

let debug_scheme { root; quantifiers; generics } =
  let open PPrint in
  let root_doc = Constraint.Var.print root ^^ space in
  let quantifiers_doc =
    concat_map
      (fun var -> string "∀" ^^ Constraint.Var.print var ^^ string ". ")
      quantifiers
  in
  let generics_doc =
    brackets (separate_map space Constraint.Var.print generics)
  in

  quantifiers_doc ^^ root_doc ^^ generics_doc

(* Transform root into a scheme.
   Assertion: should be called only after generalization *)

let schemify (env : env) (quantifiers : variable list) (root : variable) :
  scheme =
  (* Cache to avoid visiting a root's representative twice *)
  let cache = Hashtbl.create 16 in

  (* Compute generic variables reachable from [root] *)
  let traverse (root : variable) : variable list =
    let rec loop (acc : variable list) (root : variable) : variable list =
      let repr = Env.repr root env in

      if repr.status <> Generic || Hashtbl.mem cache repr.var then acc
      else begin
        Hashtbl.replace cache repr.var ();
        let acc = repr.var :: acc in

        match repr.structure with
        | None -> acc
        | Some structure -> Structure.fold loop acc structure
      end
    in
    loop [] root
  in

  let generics = List.rev (traverse root) in

  (* Construct and return the scheme *)
  { root; generics; quantifiers }

(* Function to exit, to terminate the process of generalization *)

let exit (roots : variable list) (env : env) : env * scheme list =
  (* Get the young level of the environment *)
  let state_young = Env.get_young env in

  (* Ensure balanced calls to enter and exit *)
  assert (state_young >= base_rank);

  (* Discover all young variables *)
  let generation = discover_young_generation env in

  (* Update the rank of every young variable.
     Variables that must become generic still have rank state_young *)
  let env = update_ranks generation env in

  (* Generalize young variables that still have rank state_young *)
  let env, quantifiers = generalize generation env in

  (* Build a scheme for each root variable *)
  let schemes = List.map (schemify env quantifiers) roots in

  (* Clean up the environment *)
  let env = Env.clean_pool state_young env in
  let env = Env.decr_young env in

  (* Return the updated environment, list of quantifiers, and schemes *)
  (env, schemes)

(* Instantiate a scheme with a constraint variable *)

let instantiate ({ root; generics; quantifiers } : scheme) (var : variable)
  (env : env) : env * (variable list, err) result =
  (* Create a flexible copy without structure for each generic variable *)
  let (env : env), (mapping : (variable, variable) Hashtbl.t) =
    let ht = Hashtbl.create 16 in

    let env =
      List.fold_left
        begin
          fun env var ->
            let repr = Env.repr var env in
            assert (repr.status = Generic);

            if Hashtbl.mem ht repr.var then env
            else begin
              let env, fresh_var =
                fresh_flexible ~name:("fresh_" ^ repr.var.name) None env
              in
              Hashtbl.replace ht repr.var fresh_var;

              env
            end
        end
        env (quantifiers @ generics)
    in

    (env, ht)
  in

  (* Map every variable to its copy if existed *)
  let copy (env : env) (var : variable) : variable =
    let repr = Env.repr var env in
    if repr.status <> Generic then var else Hashtbl.find mapping repr.var
  in

  (* For each generic variable, equip its copy with a copy of its structure *)
  let env =
    List.fold_left
      begin
        fun env var ->
          let repr = Env.repr var env in

          let var_copy = Hashtbl.find mapping repr.var in

          let structure_copy =
            Option.map (Structure.map (copy env)) repr.structure
          in

          let copy_repr =
            { (Env.repr var_copy env) with structure = structure_copy }
          in

          Env.set copy_repr env
      end
      env generics
  in

  (* Instantiate the scheme by unifying the root copy with [var] *)
  let copy_root = copy env root in

  match Unif.unify env copy_root var with
  | Ok new_env ->
    let copy_quantifiers = List.map (copy env) quantifiers in
    (new_env, Ok copy_quantifiers)
  | Error err -> (env, Error err)
