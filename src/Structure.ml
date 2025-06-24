(** Type-formers are defined explicit as type "structures".

    Type structures ['a t] are parametric over the type of their leaves. Typical
    tree-shaped representation of types would use [ty t], a structure carrying
    types as sub-expressions, but the types manipulated by the constraint solver
    are so-called "shallow types" that always use inference variables at the
    leaves. We cannot write, say, [?w = α -> (β * γ)], one has to write
    {[
      ∃?w1 ?w2 ?w3 ?w4.
        ?w = ?w1 -> ?w2
      ∧ ?w1 = α
      ∧ ?w2 = ?w3 * ?w4
      ∧ ?w3 = β
      ∧ ?w4 = γ
    ]}
    instead.

    (The implementation goes through a first step [('v, 'a) t_] that is also
    parametrized over a notion of type variable, just like ['v Untyped.term] --
    see the documentation there.) *)

module TyVar = Utils.Variables ()

type ('v, 'a) t_ =
  | Var of 'v
    (** Note: a type variable here represents a rigid/opaque/abstract type
        [α, β...], not a flexible inference variable like [?w] in constraints.

        For example, for two distinct type variables [α, β] the term
        [(lambda x. x : α → α) (y : β)] is always ill-typed. *)
  | Arrow of 'a * 'a
  | Prod of 'a list

type 'a raw = (string, 'a) t_

type 'a t = (TyVar.t, 'a) t_

let iter f = function
  | Var _alpha -> ()
  | Arrow (t1, t2) ->
    f t1;
    f t2
  | Prod ts -> List.iter f ts


let map f = function
  | Var alpha -> Var alpha
  | Arrow (t1, t2) -> Arrow (f t1, f t2)
  | Prod ts -> Prod (List.map f ts)


let fold f acc = function
  | Var _alpha -> acc
  | Arrow (t1, t2) ->
    let acc = f acc t1 in
    f acc t2
  | Prod ts -> List.fold_left f acc ts


let merge f s1 s2 =
  match (s1, s2) with
  | Var alpha, Var beta when TyVar.eq alpha beta -> Some (Var alpha)
  | Arrow (a1, a2), Arrow (b1, b2) ->
    let c1 = f a1 b1 in
    let c2 = f a2 b2 in

    Some (Arrow (c1, c2))
  | Prod as1, Prod as2 when List.compare_lengths as1 as2 = 0 ->
    Some (Prod (List.map2 f as1 as2))
  | _ -> None


let global_tyvar : string -> TyVar.t =
  (* There are no binders for type variables, which are scoped
     globally for the whole term. *)
  let tenv = Hashtbl.create 16 in

  fun alpha ->
    match Hashtbl.find_opt tenv alpha with
    | Some alpha_var -> alpha_var
    | None ->
      let alpha_var = TyVar.fresh alpha in
      Hashtbl.replace tenv alpha alpha_var;
      alpha_var


let freshen freshen = function
  | Var alpha -> Var (global_tyvar alpha)
  | Arrow (t1, t2) -> Arrow (freshen t1, freshen t2)
  | Prod ts -> Prod (List.map freshen ts)


let print p = function
  | Var v -> TyVar.print v
  | Arrow (t1, t2) -> Printer.arrow (p t1) (p t2)
  | Prod ts -> Printer.product (List.map p ts)
