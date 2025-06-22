open ChoicePath
open PPrint

let rec print p = group @@ match p with
| Nil -> string "_"
| Return -> string "."
| Fail -> string "fail"
| Index i -> string ("i" ^ string_of_int i)
| Sum (i, p) -> string ("s" ^ string_of_int i) ^/^ print p
| Bind (p1, p2) -> string "bind" ^^ parens (print p1) ^/^ print p2
