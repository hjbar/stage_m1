open PPrint

let string_of_doc doc =
  let buf = Buffer.create 128 in
  PPrint.ToBuffer.pretty 0.9 80 buf doc;
  Buffer.contents buf

(** $t -> $u *)
let arrow t u = t ^/^ string "->" ^/^ u

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
