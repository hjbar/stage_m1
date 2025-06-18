{
  open UntypedParser

  let keyword_table =
    Hashtbl.create 17

  let keywords = [
      "let", LET;
      "in", IN;
      "lambda", LAMBDA;
    ]

  let _ =
    List.iter
      (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      keywords

  let new_line lexbuf =
    Lexing.new_line lexbuf
}

let identchar = ['a'-'z' 'A'-'Z' '0'-'9' '_']
let lident = ['a'-'z'] identchar*

let blank = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | eof		 { EOF }
  | newline      { new_line lexbuf; read lexbuf }
  | blank        { read lexbuf }
  | lident as id { try Hashtbl.find keyword_table id
                    with Not_found -> LIDENT id }
  | "->"         { ARROW }
  | '('		 { LPAR }
  | ')'		 { RPAR }
  | '{'		 { LBRACE }
  | '}'		 { RBRACE }
  | '*'		 { STAR }
  | ','		 { COMMA }
  | '='		 { EQ }
  | ":"		 { COLON }
  | '.'		 { PERIOD }
  | "--"         { line_comment lexbuf; read lexbuf }
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
