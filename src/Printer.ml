[@@@ocamlformat "disable"]

open PPrint

(** ?w *)
let inference_variable w =
  group (string "?" ^^ w)

(** ?s *)
let scheme_variable s = group (string "?scheme_" ^^ s)

(** $t -> $u *)
let arrow t u = group @@
  t ^/^ string "->" ^/^ u

(** {$t1 * $t2 * ... $tn} *)
let product ts = group @@
  braces (separate (break 1 ^^ star ^^ space) ts)

(** ($term : $ty) *)
let annot term ty = group @@
  surround 2 0 lparen (
    term ^/^ colon ^//^ ty
  ) rparen

(** ∀$ty1. ∀$ty2. ... *)
let print_quantifier quantifiers =
  group (concat_map (fun var -> string "∀" ^^ var ^^ dot ^^ space) quantifiers)


(** ∀$ty1. ∀$ty2. ... $ty *)
let scheme quantifiers ty =
  group (quantifiers ^^ ifflat empty (nest 2 (break 1)) ^^ ty)


(** x[$ty1, $ty2, ...] *)
let ty_app t tys =
  group (t ^^ lbracket ^^ separate (comma ^^ space) tys ^^ rbracket)


(** (x : ty) *)
let let_binding x ty =
  group
    begin
      lparen ^^ x ^^ space ^^ colon ^^ space ^^ ty ^^ rparen
    end


(** lambda $input. $body *)
let lambda ~input ~body = group @@
  string "lambda"
  ^/^ input
  ^^ string "."
  ^//^ body

(** Λ $input. $body *)
let big_lambda ~input ~body =
  group
    begin
      concat_map (fun var -> string "Λ" ^^ var ^^ dot ^^ space) input
      ^^ ifflat empty (break 1)
      ^^ body
    end


(** let $var = $def in $body *)
let let_ ~var ~def ~body =
  group begin
    group begin
      group (string "let" ^/^ var ^/^ string "=")
      ^^ nest 2 (break 1 ^^ def)
      ^/^ string "in"
    end
    ^//^ body
  end

(** $t $u *)
let app t u = group @@
  t ^//^ u

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
  let bindings =
    group (flow_map (break 1) print_binding bindings)
  in
  group (utf8string "∃" ^^ ifflat empty space
         ^^ nest 2 bindings
         ^^ break 0 ^^ string ".")
  ^^ prefix 2 1 empty body

let true_ = utf8string "⊤"
let false_ = utf8string "⊥"

(** $c1 ∧ $c2 ∧ .... ∧ $cn *)
let conjunction docs = group @@
  match docs with
  | [] -> true_
  | docs -> separate (break 1 ^^ utf8string "∧" ^^ space) docs

(** $v1 = $v2 *)
let eq v1 v2 = group @@
  v1
  ^/^ string "="
  ^/^ v2

(** decode $v *)
let decode v = group @@
  string "decode" ^^ break 1 ^^ v

let do_ = string "do?"

(** decode_scheme $s *)
let decode_scheme sch_var = group (string "decode_scheme" ^^ break 1 ^^ sch_var)

(** $s ≤ w *)
let instance sch_var var =
  group (sch_var ^^ break 1 ^^ utf8string "≤" ^^ break 1 ^^ var)

(** let x1:w1, x2:w2... = $c1 in $c2 *)
let let_sch bindings c1 c2 =
  let binding (sch_var, var) = sch_var ^/^ colon ^/^ var in

  group
    begin
      group
        begin
          string "let"
          ^/^ separate_map (comma ^^ break 1) binding bindings
          ^/^ string "="
        end
      ^^ nest 2 (break 1 ^^ c1)
      ^/^ string "in"
      ^//^ c2
    end

(** let true in $c2 *)
let let_sch_2 c2 =
  group (string "let " ^^ true_ ^^ break 1 ^^ string "in" ^^ break 1 ^^ c2)

(** hole {$env} $c *)
let hole ~env c =
  group
    begin
      group (string "hole" ^^ break 1 ^^ group (surround 2 0 lbrace env rbrace))
      ^^ break 1
      ^^ group (surround 2 0 lparen c rparen)
    end

(**
   $ty1
incompatible with
   $ty2
*)
let incompatible ty1 ty2 =
  group (blank 2 ^^ nest 2 ty1)
  ^^ hardline ^^ string "incompatible with" ^^ hardline ^^
  group (blank 2 ^^ nest 2 ty2)


let cycle v = group @@
  string "cycle on constraint variable" ^/^ v
