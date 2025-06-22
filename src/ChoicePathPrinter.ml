open ChoicePath
open PPrint

let rec print p = group @@ match p with
| Nil -> string "_"
| Return -> string "."
| Fail -> string "fail"
| Sum (i, p) -> string (string_of_int i) ^/^ print p
| Bind (p1, p2) -> parens (print p1) ^/^ print p2
