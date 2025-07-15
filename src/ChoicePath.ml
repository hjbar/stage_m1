type t =
  | Nil
  | Return
  | Fail
  | Sum of int * t
  | Bind of t * t
