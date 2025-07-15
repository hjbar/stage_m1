(* Import some modules *)

module UEnv = Unif.Env
module Var = Constraint.Var

(* Import some types *)

type variable = Constraint.variable

type rank = Unif.rank

module RankMap = Map.Make (Int)

type status = Unif.status =
  | Flexible
  | Generic

type repr = Unif.repr

(* Import some values *)

let base_rank = 0

module Env : sig
  type t

  val empty : t

  val is_empty : t -> bool

  (* Young generation *)

  val get_young : t -> rank

  val incr_young : t -> t

  val decr_young : t -> t

  (* Pool functions *)

  val pool_is_empty : rank:rank -> t -> bool

  val get_pool : rank:rank -> t -> variable list

  val add_to_pool : variable -> rank:rank -> t -> t

  val clean_pool : rank:rank -> t -> t

  (* Debugging functions *)

  val debug : Unif.Env.t -> t -> PPrint.document
end = struct
  type pool = variable list

  type pools = pool RankMap.t

  type t = {
    pools : pools;
    young : rank;
  }

  (* Empty environment *)

  let empty =
    let pools = RankMap.empty in
    let young = base_rank - 1 in
    { pools; young }


  (* Functions to check whether parts of the environment are empty *)

  let is_empty env = RankMap.is_empty env.pools

  (* Current young generation *)

  let get_young env = env.young

  let incr_young env =
    let young = env.young + 1 in
    { env with young }


  let decr_young env =
    let young = env.young - 1 in
    { env with young }


  (* Pool manipulation *)

  let pool_is_empty ~rank env =
    match RankMap.find_opt rank env.pools with
    | None | Some [] -> true
    | Some _ -> false


  let get_pool ~rank env =
    Option.value ~default:[] @@ RankMap.find_opt rank env.pools


  let add_to_pool var ~rank env =
    let pools =
      RankMap.update rank
        (fun pool -> Some (var :: Option.value ~default:[] pool))
        env.pools
    in
    { env with pools }


  let clean_pool ~rank env =
    let pools = RankMap.remove rank env.pools in
    { env with pools }


  (* Debugging functions *)

  let debug uenv env =
    let open PPrint in
    env.pools
    |> RankMap.bindings
    |> List.map
         begin
           fun (rank, variables) ->
             let alinea = hardline ^^ space ^^ space ^^ space in
             let variables_doc =
               variables
               |> List.map (fun v -> UEnv.debug_var v uenv)
               |> separate alinea
             in
             string (Format.sprintf "%d |-->" rank) ^^ alinea ^^ variables_doc
         end
    |> separate hardline
end

type uenv = UEnv.t

type env = Env.t

let add_flexible uenv env var structure =
  let young = Env.get_young env in
  let uenv =
    UEnv.add { var; structure; rank = young; status = Flexible } uenv
  in
  let env = Env.add_to_pool var ~rank:young env in
  (uenv, env)


(* Adjust the rank of the variable's representative
   when it is flexible, i.e., not yet generalized *)

let adjust_rank (repr : repr) (k : rank) uenv : uenv * repr =
  assert (repr.status <> Generic);

  let repr = { repr with rank = min repr.rank k } in
  let uenv = UEnv.set repr uenv in

  (uenv, repr)


(* Set up the environment before entering a new level *)

let enter (env : env) : env =
  let env = Env.incr_young env in
  let young = Env.get_young env in
  assert (Env.pool_is_empty ~rank:young env);
  env


(* The set of all young variables. A variable is considered
   young if it is currently registered in the youngest pool.
   This implies that its rank is at most [state.young]. *)

type generation = {
  inhabitants : variable list (* All young variables *);
  by_rank : variable list RankMap.t (* All young variables indexed by rank *);
  is_young : variable -> bool;
    (* True if the given variable belongs to the same
     equivalence class as some young variable v' *)
}

(* Before exiting, create a generation describing the young generation *)

let discover_young_generation (uenv : uenv) (env : env) : generation =
  (* Young level of the env *)
  let young = Env.get_young env in

  (* The most recent pool holds the list of
     all variables in the young generation *)
  let inhabitants = Env.get_pool ~rank:young env in

  (* Cache to track which equivalence classes are considered young *)
  let cache = Hashtbl.create 16 in

  (* Compute the young generation, grouping variables by rank *)
  let by_rank =
    List.fold_left
      begin
        fun by_rank var ->
          Hashtbl.replace cache var ();

          let repr = UEnv.repr var uenv in
          let r = repr.rank in

          assert (repr.status <> Generic);
          assert (base_rank <= r && r <= young);

          let l = Option.value ~default:[] @@ RankMap.find_opt r by_rank in
          let by_rank = RankMap.add r (var :: l) by_rank in

          by_rank
      end
      RankMap.empty inhabitants
  in

  (* Returns true if [var] is part of the young generation *)
  let is_young var = Hashtbl.mem cache var in

  (* Final result *)
  { inhabitants; by_rank; is_young }


(* Update the rank of every variable in the young generation *)

let update_ranks (uenv : uenv) (env : env) (generation : generation) : uenv =
  (* Cache to mark visited variables *)
  let cache = Hashtbl.create 16 in

  (* Accumulator for the final environment *)
  let uenv = ref uenv in

  (* Update the environment for each rank from base to young *)
  for k = base_rank to Env.get_young env do
    (* Recursively compute the updated rank for a variable *)
    let rec traverse (var : variable) : rank =
      (* Get the representative of the variable and check its status *)
      let repr = UEnv.repr var !uenv in
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
        let new_uenv, repr = adjust_rank repr k !uenv in
        uenv := new_uenv;

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
            let new_uenv, repr = adjust_rank repr max_rank !uenv in
            uenv := new_uenv;

            (* Return the new rank of the variable's rank *)
            repr.rank
          end
        end
      end
    in

    (* Apply traversal to all variables of rank [k] *)
    List.iter
      (fun var -> traverse var |> ignore)
      (Option.value ~default:[] @@ RankMap.find_opt k generation.by_rank)
  done;

  (* Return the updated environment *)
  !uenv


(* Return a list of variables that have become generic *)

let generalize (uenv : uenv) (env : env) (generation : generation) :
  uenv * env * variable list =
  (* Initialize *)
  let young = Env.get_young env in
  let uenv = ref uenv in
  let env = ref env in

  (* Compute the list of generic variables *)
  let l =
    List.filter
      begin
        fun var ->
          (* Ignore variables that are not representatives of their equivalence class *)
          UEnv.is_representative var !uenv
          && begin
               let repr = UEnv.repr var !uenv in
               let rank = repr.rank in

               (* If rank is less than state_young,
               re-register at the lower level and drop *)
               if rank < young then begin
                 env := Env.add_to_pool var ~rank !env;
                 false
               end
               else begin
                 (* If rank equals state_young, make var generic
                 and drop it if it has structure *)
                 assert (rank = young);

                 let repr = { repr with status = Generic } in
                 uenv := UEnv.set repr !uenv;

                 repr.structure = None
               end
             end
      end
      generation.inhabitants
  in

  (* Return the updated environment and the list of generic variables *)
  (!uenv, !env, l)


(* Representation of a scheme *)

type scheme = {
  root : variable; (* A root variable: must be generic *)
  generics : variable list (* All generic variables of the scheme *);
  quantifiers : variable list; (* All generic variables that have no structure *)
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

let schemify (uenv : uenv) (quantifiers : variable list) (root : variable) :
  scheme =
  (* Cache to avoid visiting a root's representative twice *)
  let cache = Hashtbl.create 16 in

  (* Compute generic variables reachable from [root] *)
  let traverse (root : variable) : variable list =
    let rec loop (acc : variable list) (root : variable) : variable list =
      let repr = UEnv.repr root uenv in

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

let exit (uenv : uenv) (env : env) (roots : variable list) :
  uenv * env * scheme list =
  (* Get the young level of the environment *)
  let young = Env.get_young env in

  (* Ensure balanced calls to enter and exit *)
  assert (young >= base_rank);

  (* Discover all young variables *)
  let generation = discover_young_generation uenv env in

  (* Update the rank of every young variable.
     Variables that must become generic still have rank state_young *)
  let uenv = update_ranks uenv env generation in

  (* Generalize young variables that still have rank state_young *)
  let uenv, env, quantifiers = generalize uenv env generation in

  (* Build a scheme for each root variable *)
  let schemes = List.map (schemify uenv quantifiers) roots in

  (* Clean up the environment *)
  let env = Env.clean_pool ~rank:young env in
  let env = Env.decr_young env in

  (* Return the updated environment, list of quantifiers, and schemes *)
  (uenv, env, schemes)


(* Instantiate a scheme with a constraint variable *)

let instantiate uenv env ({ root; generics; quantifiers } : scheme) =
  let mapping : (variable, variable) Hashtbl.t = Hashtbl.create 16 in

  (* Create a flexible copy without structure for each generic variable *)
  let fresh (uenv, env) var =
    if Hashtbl.mem mapping var then (uenv, env)
    else begin
      let repr = UEnv.repr var uenv in
      assert (repr.status = Generic);
      let fresh_var = Constraint.Var.fresh ("fresh_" ^ repr.var.name) in
      Hashtbl.add mapping var fresh_var;
      add_flexible uenv env fresh_var None
    end
  in
  let uenv, env = List.fold_left fresh (uenv, env) (quantifiers @ generics) in

  (* Map every variable to its copy if it exists. *)
  let copy (var : variable) : variable =
    try Hashtbl.find mapping var with Not_found -> var
  in

  (* For each generic variable, equip its copy with a copy of its structure *)
  let uenv =
    List.fold_left
      begin
        fun uenv var ->
          let var_copy = copy var in
          let repr = UEnv.repr var uenv in
          let structure_copy = Option.map (Structure.map copy) repr.structure in
          let copy_repr =
            { (UEnv.repr var_copy uenv) with structure = structure_copy }
          in
          UEnv.set copy_repr uenv
      end
      uenv generics
  in

  (* Instantiate the scheme by unifying the root copy with [var] *)
  let copy_root = copy root in
  let copy_quantifiers = List.map copy quantifiers in
  (uenv, env, copy_root, copy_quantifiers)
