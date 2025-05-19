module Make (T : Utils.Functor) = struct
  module Constraint = Constraint.Make (T)
  open Constraint

  type t = sat_constraint

  and sat_constraint =
    | Loc of Utils.loc * sat_constraint
    | Exist of variable * structure option * sat_constraint
    | Conj of sat_constraint list (* [True] is [Conj []] *)
    | Eq of variable * variable
    | Decode of variable
    | False
    | Do of sat_constraint T.t
    | DecodeScheme of scheme_variable
    | Instance of scheme_variable * variable
    | Let of scheme_variable * variable * sat_constraint * sat_constraint

  let rec erase : type a e. (a, e) Constraint.t -> sat_constraint = function
    | Exist (v, c, s) -> Exist (v, c, erase s)
    | Loc (loc, c) -> Loc (loc, erase c)
    | Map (c, _) -> erase c
    | MapErr (c, _) -> erase c
    | Ret _v -> Conj []
    | Err _e -> False
    | Conj (_, _) as conj -> begin
      let rec peel : type a e. (a, e) Constraint.t -> sat_constraint list =
        function
        | Loc (_loc, c) -> peel c
        | Map (c, _) -> peel c
        | MapErr (c, _) -> peel c
        | Conj (c1, c2) -> peel c1 @ peel c2
        | Err _ -> [ False ]
        | Ret _ -> []
        | Exist _ as c -> [ erase c ]
        | Eq _ as c -> [ erase c ]
        | Decode _ as c -> [ erase c ]
        | Do _ as c -> [ erase c ]
        | DecodeScheme _ as c -> [ erase c ]
        | Instance _ as c -> [ erase c ]
        | Let _ as c -> [ erase c ]
      in

      match peel conj with [ c ] -> c | cases -> Conj cases
    end
    | Eq (v1, v2) -> Eq (v1, v2)
    | Decode v -> Decode v
    | Do c -> Do (T.map erase c)
    | DecodeScheme sch_var -> DecodeScheme sch_var
    | Instance (sch_var, var) -> Instance (sch_var, var)
    | Let (sch_var, var, c1, c2) -> Let (sch_var, var, erase c1, erase c2)
end
