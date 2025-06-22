type t =
  | Nil
  | Return
  | Fail
  | Index of int
  | Sum of int * t
  | Bind of t * t
