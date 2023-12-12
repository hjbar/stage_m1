%{
    open Untyped.Make(Utils.Id)
%}

%token <string> LIDENT

%token EOF

%token LET "let"
%token IN "in"
%token LAMBDA "lambda"

%token ARROW "->"
%token LPAR "("
%token RPAR ")"
%token STAR "*"
%token COMMA ","
%token EQ "="
%token COLON ":"
%token PERIOD "."

%type<Untyped.Make(Utils.Id).raw_term> term_eof

%start term_eof

%%

let term_eof :=
  | ~ = term ; EOF ;
    <>

(***************** TERMS ***************)

let term :=
  | ~ = term_abs ; <>

let term_abs :=
  | "lambda" ; xs = list (tevar) ; "." ; t = term_abs ;
    { List.fold_right (fun x t -> Abs (x, t)) xs t }
  | (x, t1, t2) = letin(tevar) ;
    { Let (x, t1, t2) }
  | (xs, t1, t2) = letin(tuple(tevar)) ;
    { LetTuple (xs, t1, t2) }
  | ~ = term_app ; <>

let term_app :=
  | t1 = term_app ; t2 = term_atom ;
    { App (t1, t2) }
  | ~ = term_atom ; <>

let term_atom :=
  | x = tevar ;
    { Var x }
  | ts = tuple (term) ;
    { Tuple ts }
  | "(" ; t = term ; ":" ; ty = typ ; ")" ;
    { Annot (t, ty) }
  | "(" ; ~ = term ; ")" ; <>

let tevar :=
  | ~ = LIDENT ; <>

let letin (X) :=
  | LET ; x = X ; EQ ;
      t1 = term ; IN ;
      t2 = term_abs ;
    { (x, t1, t2) }

let tuple (X) :=
  | "(" ; ")" ;
    { [] }
  (* note: the rule below enforces that one-element lists always
     end with a trailing comma *)
  | "(" ; x = X ; COMMA ; xs = item_sequence(X, COMMA) ; ")";
    { x :: xs }

(* item sequence with optional trailing separator *)
let item_sequence(X, Sep) :=
  |
    { [] }
  | x = X ;
    { [x] }
  | x = X ; () = Sep ; xs = item_sequence(X, Sep) ;
    { x :: xs }

(*************** TYPES ***************)

let typ :=
  | ~ = typ_arrow ; <>

let typ_arrow :=
  | ty1 = typ_atom ; "->" ; ty2 = typ_arrow ;
    { STLC.Constr (Structure.Arrow (ty1, ty2)) }
  | ~ = typ_atom ; <>

let typ_atom :=
  | x = tyvar ;
    { STLC.Constr (Structure.Var x) }
  | "(" ; tys = separated_list ("*", typ) ; ")" ;
    { STLC.Constr (Structure.Prod tys) }
  | "(" ; ~ = typ ; ")" ; <>

let tyvar :=
  | ~ = LIDENT ; <>

