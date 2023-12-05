open PPrint

let string_of_doc doc =
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf

(** $t -> $u *)
let arrow t u = t ^/^ string "->" ^/^ u

(** ($t1 * $t2 * ... $tn) *)
let product ts =
  parens (separate (break 1 ^^ star ^^ space) ts)

(** ($term : $ty) *)
let annot term ty =
  surround 2 0 lparen (
    term ^/^ colon ^//^ ty
  ) rparen

(** fun $input -> $body *)
let fun_ ~input ~body =
  string "fun"
  ^/^ input
  ^/^ string "->"
  ^//^ body

(** let $var = $def in $body *)
let let_ ~var ~def ~body =
  string "let"
  ^/^ var
  ^/^ string "="
  ^/^ def
  ^/^ string "in"
  ^//^ body

(** $t $u *)
let app t u =
  t ^/^ u

let tuple p ts =
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
