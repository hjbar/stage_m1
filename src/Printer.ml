open PPrint

let string_of_doc doc =
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf

(** $t -> $u *)
let arrow t u = group @@
  t ^/^ string "->" ^/^ u

(** ($t1 * $t2 * ... $tn) *)
let product ts = group @@
  parens (separate (break 1 ^^ star ^^ space) ts)

(** ($term : $ty) *)
let annot term ty = group @@
  surround 2 0 lparen (
    term ^/^ colon ^//^ ty
  ) rparen

(** fun $input -> $body *)
let fun_ ~input ~body = group @@
  string "fun"
  ^/^ input
  ^/^ string "->"
  ^//^ body

(** let $var = $def in $body *)
let let_ ~var ~def ~body = group @@
  string "let"
  ^/^ var
  ^/^ string "="
  ^/^ def
  ^/^ string "in"
  ^//^ body

(** $t $u *)
let app t u = group @@
  t ^/^ u

(** (t1, t2... tn) *)
let tuple p ts = group @@
  match ts with
  | [] -> lparen ^^ rparen
  | _ ->
    surround 2 0 lparen (
      match ts with
      | [t] ->
          (* For arity-1 tuples we print (foo,)
             instead of (foo) which would be ambiguous. *)
          p t ^^ comma
      | _ ->
          separate_map (comma ^^ break 1) p ts
    ) rparen

(** ∃$w1 $w2 ($w3 = $s) $w4... $wn. $c *)
let exist bindings body = group @@
  let print_binding (w, s) =
    match s with
    | None -> w
    | Some s ->
      group @@
      surround 2 0 lparen (
        w
        ^/^ string "="
        ^/^ s
      ) rparen
  in
  utf8string "∃"
  ^^ group (separate_map (break 1) print_binding bindings)
  ^^ string "."
  ^//^ body

(** $c1 ∧ $c2 ∧ .... ∧ $cn *)
let conjunction docs = group @@
  separate (break 1 ^^ utf8string "∧" ^^ space) docs

let true_ = utf8string "⊤"
let false_ = utf8string "⊥"

(** $v1 = $v2 *)
let eq v1 v2 = group @@
  v1
  ^/^ string "="
  ^/^ v2

(** decode $v *)
let decode v = group @@
  string "decode" ^^ break 1 ^^ v
