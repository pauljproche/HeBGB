(* Ocamllex scanner for HeBGB (using MicroC template) *)

{ open Parser }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                 { comment lexbuf }
| '('                  { LPAREN }
| ')'                  { RPAREN }
| '['                  { LBRACK } 
| ']'                  { RBRACK }
| '{'                  { LCURLY } 
| '}'                  { RCURLY } 
| ','                  { COMMA }
| "<-"                 { LARROW }
| "->"                 { RARROW }
| "<~"                 { LSQUIGGLY }
| '+'                  { PLUS }
| '-'                  { MINUS }
| '*'                  { TIMES }
| '/'                  { DIVIDE }
| '%'                  { MOD }
| "=="                 { EQ }
| "!="                 { NEQ }
| "<="                 { LEQ }
| ">="                 { GEQ }
| ">"                  { GT }
| '<'                  { LT }
| "&&"                 { AND }
| "||"                 { OR }
| "int"                { INT }
| "string"             { STRING }
| "cont"               { CONT }
| "list"               { LIST }
| "in"                 { IN }
| "continue"           { CONTINUE }
| "with"               { WITH }
| "then"               { THEN }
| "if"                 { IF }
| "do"                 { DO }
| "else"               { ELSE }
| '\"' _* '\"'                               as lit { LITSTR(lit) }
| '-'digit+ | digit+                         as lit { LITINT(int_of_string lit) }
| letter (letter | digit | '_' | '-' | '?')* as id  { ID(id) }
| eof                  { EOF }
| _ as char            { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

