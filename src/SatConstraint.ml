open Constraint

type sat_constraint =
  | Exist of variable * structure option * sat_constraint
  | Conj of sat_constraint list (* [True] is [Conj []] *)
  | Eq of variable * variable
  | Decode of variable
  | False

let rec erase : type a e. (a, e) t -> sat_constraint = function
| Exist (v, c, s) -> Exist (v, c, erase s)
| Map (c, _) -> erase c
| MapErr (c, _) -> erase c
| True -> Conj []
| False -> False
| Conj (_, _) as conj ->
  let rec peel : type a e . (a, e) t -> sat_constraint list = function
    | Map (c, _) -> peel c
    | MapErr (c, _) -> peel c
    | Conj (c1, c2) -> peel c1 @ peel c2
    | False -> [False]
    | True -> []
    | Exist _  as c -> [erase c]
    | Eq _     as c -> [erase c]
    | Decode _ as c -> [erase c]
  in
  begin match peel conj with
  | [c] -> c
  | cases -> Conj cases
  end
| Eq (v1, v2) -> Eq (v1, v2)
| Decode v -> Decode v

