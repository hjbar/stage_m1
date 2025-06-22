%{
    open ChoicePath
%}

%token NIL
%token RETURN
%token FAIL
%token <int> INDEX
%token <int> SUM
%token BIND "bind"

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
  | i=INDEX; { Index(i) }
  | i=SUM; p = path; { Sum(i, p) }
  | BIND; LPAR; p1 = path; RPAR; p2 = path; { Bind(p1, p2) }
