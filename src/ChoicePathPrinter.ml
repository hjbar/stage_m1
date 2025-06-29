open ChoicePath
open PPrint

let rec print p =
  let rec elements = function
    | Nil -> string "_" :: []
    | Return -> string "." :: []
    | Fail -> string "fail" :: []
    | Sum (i, p) -> string (string_of_int i) :: elements p
    | Bind (p1, p2) -> parens (print p1) :: elements p2
  in
  group (flow (break 1) (elements p))
