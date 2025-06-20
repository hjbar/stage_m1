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

type data = Unif.repr

type env = Unif.Env.t

type err = Unif.err

(* Import some values *)

let base_rank = Unif.base_rank

(* Adjust the rank when is Flexible, i.e. not generalized yet *)

let adjust_rank (data : data) (k : rank) (env : env) : env * data =
  assert (data.status <> Generic);

  let data = { data with rank = min data.rank k } in
  let env = Env.set data env in

  (env, data)

(* Create a new flexible variable in this environment *)

let fresh_flexible ?(name = "fresh") (structure : structure) (env : env) :
  env * variable =
  let var = Var.fresh name in
  let env = Env.add_flexible var structure env in
  (env, var)

(* Call this function before enter in a new level *)

let enter (env : env) : env =
  let env = Env.incr_young env in
  let young = Env.get_young env in

  assert (Env.pool_k_is_empty young env);
  env

(* The set of all young variables *)
(* A variable is young if it is currently registered in the yougest pool *)
(* This implies that its rank is at most state.young *)

type generation =
  { inhabitants : variable list (* All young variables *)
  ; by_rank : variable list IntMap.t (* All young variables indexed by rank *)
  ; is_young : variable -> bool
      (* true if v is the same equivalence class as some young variable v' *)
  }

(* Before we want to exit, create a generation describing the young generation *)

let discover_young_generation (env : env) : generation =
  (* Young of the env *)
  let state_young = Env.get_young env in

  (* The most recent pool holds a list of all variables in the young generation *)
  let inhabitants = Env.get_pool state_young env in

  (* To mark which equivalence classes are young *)
  let cache = Hashtbl.create 16 in

  (* Compute the elements of the young generation *)
  let by_rank =
    List.fold_left
      begin
        fun by_rank var ->
          let data = Env.repr var env in
          Hashtbl.replace cache var ();

          let r = data.rank in

          assert (data.status <> Generic);
          assert (base_rank <= r && r <= state_young);

          let l = Option.value ~default:[] @@ IntMap.find_opt r by_rank in
          let by_rank = IntMap.add r (var :: l) by_rank in

          by_rank
      end
      IntMap.empty inhabitants
  in

  (* Returns true if var is young, false otherwise *)
  let is_young var = Hashtbl.mem cache var in

  (* Result *)
  { inhabitants; by_rank; is_young }

(* updates the rank of every variables in the young generation *)

let update_ranks (generation : generation) (env : env) : env =
  (* To mark visited variable *)
  let cache = Hashtbl.create 16 in

  (* Final env *)
  let final_env = ref env in

  (* We compute the new env for each rank *)
  for k = base_rank to Env.get_young env do
    (* Function that compute the new rank *)
    let rec traverse (var : variable) (env : env) : env * rank =
      (* To get the repr of the variable & check its status *)
      let data = Env.repr var env in
      assert (data.status <> Generic);

      (* If we have already visited this variable, stop *)
      if Hashtbl.mem cache data.var then begin
        assert (data.rank <= k);
        (env, data.rank)
      end
      else begin
        (* Push the information that we visited this variable *)
        Hashtbl.replace cache data.var ();

        (* Update the rank of the variable and push the update in env *)
        let env, data = adjust_rank data k env in

        (* If the variable is not young, stop *)
        if not @@ generation.is_young var then (env, data.rank)
        else begin
          (* The variable is not visited yet, so its rank must be k *)
          assert (data.rank = k);

          (* If the variable has no structure, stop *)
          if data.structure = None then (env, data.rank)
          else begin
            (* Traverse all the children of var and compute the max rank and the new env *)
            let env, max_rank =
              Structure.fold
                begin
                  fun (env, acc) child ->
                    let env, cur_rank = traverse child env in
                    (env, max cur_rank acc)
                end
                (env, base_rank)
                (Option.get data.structure)
            in

            (* Adjust the rank of var with the max rank of its children *)
            let env, data = adjust_rank data max_rank env in

            (* Return the new env *)
            (env, data.rank)
          end
        end
      end
    in

    (* Compute the new env *)
    final_env :=
      List.fold_left
        (fun env var -> fst @@ traverse var env)
        !final_env
        (Option.value ~default:[] @@ IntMap.find_opt k generation.by_rank)
  done;

  (* Return the updated env *)
  !final_env

(* Returns a list of the variables that have become generic *)

let generalize (generation : generation) (env : env) : env * variable list =
  (* Init *)
  let state_young = Env.get_young env in
  let env = ref env in

  (* Compute generics *)
  let l =
    List.filter
      begin
        fun var ->
          (* If var is not the representative element of its equivalence class,
             we ignore and drop it *)
          Env.is_representative var !env
          && begin
               let data = Env.repr var !env in
               let rank = data.rank in

               (* Re-register at lower level and drop it *)
               if rank < state_young then begin
                 env := Env.register var ~rank !env;
                 false
               end
               (* r = state.young : make var Generic and drop it if var has no structure *)
                 else begin
                 assert (rank = state_young);

                 let data = { data with status = Generic } in
                 env := Env.set data !env;

                 data.structure = None
               end
             end
      end
      generation.inhabitants
  in

  (* Result *)
  (!env, l)

(* The representation of a scheme *)

type scheme =
  { root : root (* A root variable : must be generic *)
  ; generics : variable list (* All the generic variables of the scheme *)
  ; quantifiers : quantifiers
      (* All the generic variables that have no structure *)
  }

and root = variable

and roots = root list

and quantifiers = variable list

and schemes = scheme list

(* Utils function : get the "body" of a scheme *)

let body { root; _ } = root

(* Utils function : get the quantifiers of a scheme *)

let quantifiers { quantifiers; _ } = quantifiers

let debug_scheme { root; quantifiers; generics } =
  let open PPrint in
  let quantifiers_doc =
    concat_map
      (fun var -> string "∀" ^^ Constraint.Var.print var ^^ string ". ")
      quantifiers
  in
  quantifiers_doc ^^ Constraint.Var.print root ^^ space
  ^^ brackets (separate_map space Constraint.Var.print generics)

(* Transform root into a scheme -- assert : calls after generalization *)

let schemify (env : env) (root : variable) : scheme =
  (* Compute generics *)
  let cache = Hashtbl.create 16 in

  let traverse (var : variable) : variable list =
    let rec loop (acc : variable list) (var : variable) : variable list =
      let data = Env.repr var env in

      if data.status <> Generic || Hashtbl.mem cache data.var then acc
      else begin
        Hashtbl.replace cache data.var ();
        let acc = data.var :: acc in

        match data.structure with
        | None -> acc
        | Some structure -> Structure.fold loop acc structure
      end
    in
    loop [] var
  in

  let generics = List.rev @@ traverse root in

  (* Compute quantifiers *)
  let has_no_structure var = (Env.repr var env).structure = None in
  let quantifiers = List.filter has_no_structure generics in

  (* Result *)
  { root; generics; quantifiers }

(* Exit function *)

let exit (roots : roots) (env : env) : env * quantifiers * schemes =
  (* The young of the env *)
  let state_young = Env.get_young env in

  (* Calls to enter and exit must be balanced *)
  assert (state_young >= base_rank);

  (* Discover the young variables *)
  let generation = discover_young_generation env in

  (* Update the rank of every young variable -- variables that must become generic still have rank state_young *)
  let env = update_ranks generation env in

  (* Turn young variables still have rank state_young into generic *)
  let env, quantifiers = generalize generation env in

  (* We build a scheme for every root *)
  let schemes = List.map (schemify env) roots in

  (* Clean the environment *)
  let env = Env.clean_pool state_young env in
  let env = Env.decr_young env in

  (* Result *)
  (env, quantifiers, schemes)

let instantiate ({ root; generics; quantifiers } : scheme) (var : variable)
  (env : env) : env * (quantifiers, err) result =
  (* Create a flexible copy without structure of each generic variable *)
  let (env : env), (mapping : (variable, variable) Hashtbl.t) =
    let ht = Hashtbl.create 16 in

    let env =
      List.fold_left
        begin
          fun env var ->
            let data = Env.repr var env in
            assert (data.status = Generic);

            let env, fresh_var =
              fresh_flexible ~name:("fresh_" ^ data.var.name) None env
            in
            Hashtbl.replace ht data.var fresh_var;

            env
        end
        env generics
    in

    (env, ht)
  in

  (* Maps every variable to its copy if needed *)
  let copy (env : env) (var : variable) : variable =
    let data = Env.repr var env in
    if data.status <> Generic then var else Hashtbl.find mapping data.var
  in

  (* For every pair of var and var_copy, equip var_copy with structure of var *)
  let env =
    List.fold_left
      begin
        fun env var ->
          let data = Env.repr var env in

          let var_copy = Hashtbl.find mapping data.var in

          let structure_copy =
            Option.map (Structure.map (copy env)) data.structure
          in

          let copy_data =
            { (Env.repr var_copy env) with structure = structure_copy }
          in

          Env.set copy_data env
      end
      env generics
  in

  (* Result *)
  let copy_root = copy env root in

  match Unif.unify env copy_root var with
  | Ok new_env ->
    let copy_quantifiers = List.map (copy env) quantifiers in
    (new_env, Ok copy_quantifiers)
  | Error err -> (env, Error err)
