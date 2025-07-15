%{
    open ChoicePath
%}

%token NIL
%token RETURN
%token FAIL
%token <int> SUM

%token LPAR "("
%token RPAR ")"

%token EOF

%type<ChoicePath.t> path_eof
%start path_eof

%%

let path_eof := p=path ; EOF ; { p }
let path :=
  | NIL; { Nil }
  | RETURN; { Return }
  | FAIL; { Fail }
  | i=SUM; p = path; { Sum(i, p) }
  | LPAR; p1 = path; RPAR; p2 = path; { Bind(p1, p2) }
