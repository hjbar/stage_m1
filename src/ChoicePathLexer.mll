{
  open ChoicePathParser

  let new_line _lexbuf = ()
}

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let int = ['0' - '9' '_']+

rule read = parse
  | eof		    { EOF }
  | newline         { new_line lexbuf; read lexbuf }
  | blank           { read lexbuf }
  | '('		    { LPAR }
  | ')'		    { RPAR }
  | "_"             { NIL }
  | "."             { RETURN }
  | "fail"          { FAIL }
  | int as n        { SUM (int_of_string n) }
  | "--"            { line_comment lexbuf; read lexbuf }
  | _ as c
    { failwith
        (Printf.sprintf
           "Unexpected character during lexing: %c" c) }

and line_comment = parse
| newline
    { new_line lexbuf; () }
| eof
    { failwith "Unterminated OCaml comment: \
                no newline at end of file." }
| _
    { line_comment lexbuf }
