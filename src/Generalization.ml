(* Import some modules *)

module Env = Unif.Env
module IntMap = Unif.IntMap
module Var = Constraint.Var

let print_under =
  let cpt = ref ~-1 in
  fun s ->
    incr cpt;
    Format.printf "Under %s - %d\n%!" s !cpt

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
  print_under "adjust_rank";

  assert (data.status <> Generic);

  let data = { data with rank = min data.rank k } in
  let env = Env.add_repr data env in

  (env, data)

(* Create a new flexible variable in this environment *)

let fresh_flexible ?(name = "fresh") (structure : structure) (env : env) :
  env * variable =
  print_under "fresh_flexible";

  let var = Var.fresh name in
  let env = Env.add_flexible var structure env in
  (env, var)

(* Call this function before enter in a new level *)

let enter (env : env) : env =
  print_under "enter";

  let env = Env.incr_young env in
  let young = Env.get_young env in

  Debug.print_header "DEBUG POOLS" (Unif.Env.debug_pool_assoc env);
  Debug.print_message "The pool must be empty";
  assert (Env.pool_is_empty young env);

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

(* To know which variable is young *)
module VarSet = Set.Make (Var)

(* Before we want to exit, create a generation describing the young generation *)

let discover_young_generation (env : env) : generation =
  print_under "discover_young_generation";

  (* Young of the env *)
  let state_young = Env.get_young env in

  (* The most recent pool holds a list of all variables in the young generation *)
  Debug.print_header "DEBUG POOL" (Unif.Env.debug_pool_assoc env);

  let inhabitants = Env.get_pool state_young env in

  (* Compute the elements of the young generation *)
  let by_rank, set =
    List.fold_left
      begin
        fun (by_rank, set) var ->
          let data = Env.repr var env in
          let set = VarSet.add var set in

          let r = data.rank in

          Debug.print_message
          @@ Format.sprintf "%d <= %d && %d <= %d" base_rank r r state_young;

          assert (data.status <> Generic);
          assert (base_rank <= r && r <= state_young);

          let l = Option.value ~default:[] @@ IntMap.find_opt r by_rank in
          let by_rank = IntMap.add r (var :: l) by_rank in

          (by_rank, set)
      end
      (IntMap.empty, VarSet.empty)
      inhabitants
  in

  (* Returns true if var is young, false otherwise *)
  let is_young var = VarSet.mem var set in

  (* Result *)
  { inhabitants; by_rank; is_young }

(* updates the rank of every variables in the young generation *)

let update_ranks (generation : generation) (env : env) : env =
  print_under "update_ranks";
  Debug.print_header "DEBUG ENV" (Unif.Env.debug env);

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

      (* If we already this variable, stop *)
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
            let env, _data = adjust_rank data max_rank env in

            (* Return the new env *)
            (env, max_rank)
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
  (* Debug print *)
  Debug.print_header "Generalization.generalize" PPrint.empty;
  print_under "generalize";

  (* Init *)
  let state_young = Env.get_young env in
  let env = ref env in

  (* Compote generics *)
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
               (* r = state.young : make var Generic and grop it if var has no structure *)
                 else begin
                 assert (rank = state_young);

                 let data = { data with status = Generic } in
                 env := Env.add_repr data !env;

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

let debug_scheme { root; quantifiers; _ } =
  let open PPrint in
  string "forall "
  ^^ separate_map space Constraint.Var.print quantifiers
  ^^ dot ^^ space ^^ Constraint.Var.print root

(* Return a monomorphic scheme whose root is the root *)

let trivial (root : variable) : scheme =
  let generics = [] in
  let quantifiers = [] in
  { root; generics; quantifiers }

(* Transform root into a scheme -- assert : calls after generalization *)

let schemify (root : variable) (env : env) : scheme =
  print_under "schemify";

  (* Compute generics *)
  let cache = Hashtbl.create 16 in

  let rec traverse (var : variable) (acc : variable list) : variable list =
    let data = Env.repr var env in

    if data.status <> Generic || Hashtbl.mem cache data.var then acc
    else begin
      Hashtbl.replace cache data.var ();
      let acc = var :: acc in

      match data.structure with
      | None -> acc
      | Some structure -> Structure.fold (Fun.flip traverse) acc structure
    end
  in

  let generics = traverse root [] in

  (* Compute quantifiers *)
  let has_no_structure var = (Env.repr var env).structure = None in
  let quantifiers = List.filter has_no_structure generics in

  (* Result *)
  { root; generics; quantifiers }

(* Exit function *)

let exit (roots : roots) (env : env) : env * quantifiers * schemes =
  print_under "exit";

  (* The young of the env *)
  let state_young = Env.get_young env in

  (* Calls to enter and exit must be balanced *)
  assert (state_young >= base_rank);

  (* Discover the young variables *)
  Debug.print_header "ENV" (Unif.Env.debug env);
  let generation = discover_young_generation env in

  (* Update the rank of every young variable -- variables that must become generic still have rank state_young *)
  let env = update_ranks generation env in

  (* Turn young variables still have rank state_young into generic *)
  let env, quantifiers = generalize generation env in

  (* We build a scheme for every root *)
  let schemes = List.map (Fun.flip schemify @@ env) roots in

  (* Clean the environment *)
  Debug.print_header "DEBUG POOL" (Unif.Env.debug_pool_assoc env);
  let env = Env.clean_pool state_young env in
  let env = Env.decr_young env in
  Debug.print_header "DEBUG POOL" (Unif.Env.debug_pool_assoc env);

  (* Result *)
  (env, quantifiers, schemes)

(*
let cpt = ref 0

(* Instantiation of a scheme *)

let instantiate ({ root; generics; quantifiers } : scheme) (var : variable)
  (env : env) : (env * quantifiers, err) result =
  (* Debug print *)
  Debug.print_header "Generalization.instantiate" PPrint.empty;
  print_under "Under instantiate\n%!";

  (* To mark variables *)
  let table = Hashtbl.create 16 in

  print_under "Before env, mapping :\n%!";
  Debug.debug_what_rank root env;

  (* Create flexible copy of generic variables *)
  let (env : env), (mapping : variable list) =
    let env = ref env in

    let mapping =
      List.mapi
        begin
          fun i var ->
            let data = Env.repr var !env in
            assert (data.status = Generic);

            Hashtbl.replace table var i;

            let cur_env, var =
              fresh_flexible ~name:("fresh_" ^ var.name) None !env
            in
            env := cur_env;

            var
        end
        generics
    in

    (!env, mapping)
  in

  if generics <> [] then incr cpt;
  print_under "cpt = %d\n%!" !cpt;

  (* Maps every variable to its copy *)
  let copy (var : variable) (env : env) : variable =
    let data = Env.repr var env in

    if data.status <> Generic then var
    else
      let i = Hashtbl.find table var in
      List.nth mapping i
  in

  print_under "Before env :\n%!";
  Debug.debug_what_rank root env;

  (* For every pair of var and var_copy, equip var_copy with structure of var *)
  let env =
    List.fold_left2
      begin
        fun env var var_copy ->
          let structure_copy =
            Option.map
              (Structure.map (Fun.flip copy @@ env))
              (Env.repr var env).structure
          in

          let data =
            { (Env.repr var_copy env) with structure = structure_copy }
          in

          Env.add_repr data env
      end
      env generics mapping
  in

  print_under "Before copy root :\n%!";
  Debug.debug_what_rank root env;

  (* Result *)
  let copy_root = copy root env in

  print_under "Before unify :\n%!";
  Debug.debug_what_rank root env;

  let res =
    match Unif.unify env copy_root var with
    | Ok new_env -> Ok (new_env, List.map (Fun.flip copy @@ env) quantifiers)
    | Error err -> Error err
  in

  Debug.debug_what_rank root env;
  print_under "Finish instantiate !\n%!";

  res
*)

let instantiate ({ root; generics; quantifiers } : scheme) (var : variable)
  (env : env) : (env * quantifiers, err) result =
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
          let var_copy = Hashtbl.find mapping var in

          let structure_copy =
            Option.map (Structure.map (copy env)) (Env.repr var env).structure
          in

          let data =
            { (Env.repr var_copy env) with structure = structure_copy }
          in

          Env.add_repr data env
      end
      env generics
  in

  (* Result *)
  let copy_root = copy env root in

  match Unif.unify env copy_root var with
  | Ok new_env -> Ok (new_env, List.map (copy env) quantifiers)
  | Error err -> Error err
