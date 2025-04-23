open PPrint

(** ?w *)
let inference_variable w = string "?" ^^ w

(** $t -> $u *)
let arrow t u = group @@ t ^/^ string "->" ^/^ u

(** {!$t1 * $t2 * ... $tn} *)
let product ts = group @@ braces (separate (break 1 ^^ star ^^ space) ts)

(** ($term : $ty) *)
let annot term ty = group @@ surround 2 0 lparen (term ^/^ colon ^//^ ty) rparen

(** lambda $input. $body *)
let lambda ~input ~body =
  group @@ string "lambda" ^/^ input ^^ string "." ^//^ body

(** let $var = $def in $body *)
let let_ ~var ~def ~body =
  group @@ string "let" ^/^ var ^/^ string "=" ^/^ def ^/^ string "in" ^//^ body

(** $t $u *)
let app t u = group @@ t ^//^ u

(** (t1, t2... tn) *)
let tuple p ts =
  group
  @@
  match ts with
  | [] -> lparen ^^ rparen
  | _ ->
    surround 2 0 lparen
      ( match ts with
      | [ t ] ->
        (* For arity-1 tuples we print (foo,)
             instead of (foo) which would be ambiguous. *)
        p t ^^ comma
      | _ -> separate_map (comma ^^ break 1) p ts )
      rparen

(** ∃$w1 $w2 ($w3 = $s) $w4... $wn. $c *)
let exist bindings body =
  group
  @@
  let print_binding (w, s) =
    match s with
    | None -> w
    | Some s -> group @@ surround 2 0 lparen (w ^/^ string "=" ^/^ s) rparen
  in
  let bindings = group (flow_map (break 1) print_binding bindings) in
  group
    ( utf8string "∃" ^^ ifflat empty space ^^ nest 2 bindings ^^ break 0
    ^^ string "." )
  ^^ prefix 2 1 empty body

let true_ = utf8string "⊤"

let false_ = utf8string "⊥"

(** $c1 ∧ $c2 ∧ .... ∧ $cn *)
let conjunction docs =
  group
  @@
  match docs with
  | [] -> true_
  | docs -> separate (break 1 ^^ utf8string "∧" ^^ space) docs

(** $v1 = $v2 *)
let eq v1 v2 = group @@ v1 ^/^ string "=" ^/^ v2

(** decode $v *)
let decode v = group @@ string "decode" ^^ break 1 ^^ v

let do_ = string "do?"

(** $ty1 incompatible with $ty2 *)
let incompatible ty1 ty2 =
  group (blank 2 ^^ nest 2 ty1)
  ^^ hardline ^^ string "incompatible with" ^^ hardline
  ^^ group (blank 2 ^^ nest 2 ty2)

let cycle v = string "cycle on constraint variable" ^/^ v

let with_header header doc =
  string header ^^ colon ^^ nest 2 (group (hardline ^^ doc))
