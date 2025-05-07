(*
type rank = int

let base_rank = 0

type 'a data =
  { var_sch : Constraint.scheme_variable
  ; stucture : 'a Structure.t option
  ; rank : rank
  }

let adjust_rank (data : 'a data) (k : rank) =
  { data with rank = min data.rank k }
*)
