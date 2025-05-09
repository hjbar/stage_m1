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

(* Import some values *)

let base_rank = Unif.base_rank

(* Adjust the rank when is Flexible, i.e. not generalized yet *)

let adjust_rank (data : data) (k : rank) : data =
  assert (data.status <> Generic);
  { data with rank = min data.rank k }

(* Insert v at rank r *)

let register (var : variable) ~(rank : int) (env : env) : env =
  Env.register var ~rank env

(* *)

let flexible (structure : structure) (env : env) : env * data =
  let var = Var.fresh "v" in
  let status = Flexible in
  let rank = Env.get_young env in

  let data = Unif.{ var; structure; status; rank } in
  let env = Env.add_data data env in
  let env = register var ~rank env in

  (env, data)

(* Call this function before enter in a new level *)

let enter (env : env) : env =
  let young = Env.get_young env + 1 in
  let env = Env.set_young young env in

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
  (* Young of the env *)
  let state_young = Env.get_young env in

  (* The most recent pool holds a list of all variables in the young generation *)
  let inhabitants = Env.get_pool state_young env in

  (* Compute the elements of the young generation *)
  let by_rank, set =
    List.fold_left
      begin
        fun (by_rank, set) var ->
          let data = Env.repr var env in
          let set = VarSet.add var set in

          let r = data.rank in

          assert (data.status <> Flexible);
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
  (* The young of the environment *)
  let state_young = Env.get_young env in

  (* Returns the new rank of var -- postcond : rank var <= k *)
  let rec traverse (var : variable) (k : rank) (env : env) : env * rank =
    let data = Env.repr var env in

    (* Update the rank *)
    assert (data.status <> Generic);
    let data = adjust_rank data k in
    let env = Env.add_data data env in

    (* If var is young, we update ranks of its children *)
    if not @@ generation.is_young var then (env, data.rank)
    else begin
      assert (data.rank = k);

      (* No structure = nothing to update *)
      if data.structure = None then (env, data.rank)
      else begin
        (* Compute the max of the ranks to re-update var *)
        let env, rank =
          Structure.fold
            begin
              fun (env, max_) v ->
                (* Update the rank v by side-effects *)
                let env, rank = traverse v k env in
                (env, max max_ rank)
            end
            (env, base_rank)
          @@ Option.get data.structure
        in

        (* Re-update var with a new rank *)
        let data = adjust_rank data rank in
        (Env.add_data data env, rank)
      end
    end
  in

  (* Compute traverse for var and propagate result *)
  let rec loop_for i last_i env =
    if i > last_i then env
    else begin
      let env =
        List.fold_left
          begin
            fun env var ->
              let env, _rank = traverse var i env in
              env
          end
          env
        @@ IntMap.find i generation.by_rank
      in

      loop_for (i + 1) last_i env
    end
  in

  loop_for base_rank state_young env

(* Returns a list of the variables that have become generic *)

let generalize (generation : generation) (env : env) : env * variable list =
  let is_representative (_v : variable) : bool = failwith "TODO" in

  let state_young = Env.get_young env in
  let env = ref env in

  let l =
    List.filter
      begin
        fun var ->
          (* If var is not the representative element of its equivalence class, we ignore and drop it *)
          is_representative var
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
                 env := Env.add_data data !env;

                 data.structure = None
               end
             end
      end
      generation.inhabitants
  in

  (!env, l)

(* The representation of a scheme *)

type scheme =
  { root : root (* A root variable : must be generic *)
  ; generics : variable list (* All the generic variables of the scheme *)
  ; quantifiers : quantifiers
      (* All the generic variables that are not no structure *)
  }

and root = variable

and roots = root list

and quantifiers = variable list

and schemes = scheme list

(* Utils function : get the "body" of a scheme *)

let body { root; _ } = root

(* Utils function : get the quantifiers of a scheme *)

let quantifiers { quantifiers; _ } = quantifiers

(* Return a monomorphic scheme whose root is the root *)

let trivial (root : variable) : scheme =
  let generics = [] in
  let quantifiers = [] in
  { root; generics; quantifiers }

(* Transform root into a scheme -- assert : calls after generalization *)

let schemify (root : variable) (env : env) : scheme =
  (* Compute generics *)
  let cache = Hashtbl.create 16 in

  let rec traverse (var : variable) (acc : variable list) : variable list =
    let data = Env.repr var env in

    if data.status <> Generic || Hashtbl.mem cache var then acc
    else begin
      Hashtbl.replace cache var ();
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
  let schemes = List.map (Fun.flip schemify @@ env) roots in

  (* Clean the environment *)
  let env = Env.clean_pool state_young env in
  let env = Env.set_young (state_young - 1) env in

  (* Result *)
  (env, quantifiers, schemes)

(* Instantiation of a scheme *)

let instantiate ({ root; generics; quantifiers } : scheme) (env : env) :
  quantifiers * root =
  (* To mark variables *)
  let table = Hashtbl.create 16 in

  (* Create flexible copy of generic variables *)
  let ((env : env), (_idx : int)), (mapping : variable list) =
    List.fold_left_map
      begin
        fun (env, i) var ->
          let data = Env.repr var env in
          assert (data.status = Generic);

          Hashtbl.replace table var i;
          let env, data = flexible None env in
          ((env, i + 1), data.var)
      end
      (env, 0) generics
  in

  (* Maps every variable to its copy *)
  let copy (var : variable) (env : env) : variable =
    let data = Env.repr var env in

    if data.status <> Generic then var
    else
      let i = Hashtbl.find table var in
      List.nth mapping i
  in

  (* *)
  let env =
    List.fold_left2
      begin
        fun env var var_copy ->
          let structure_copy =
            Option.get (Env.repr var env).structure
            |> Structure.map (Fun.flip copy @@ env)
            |> Option.some
          in
          let data =
            { (Env.repr var_copy env) with structure = structure_copy }
          in
          Env.add_data data env
      end
      env generics mapping
  in

  (* Result *)
  (List.map (Fun.flip copy @@ env) quantifiers, copy root env)
