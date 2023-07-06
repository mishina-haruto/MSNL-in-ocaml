{
open Parser
exception Eof
}
rule token = parse
    [' ' '\t']     { token lexbuf }     (* skip blanks *)
  | ['\n' ]        { EOL }
  | '('			       { LPAREN }
  | ')'			       { RPAREN }
  | '{'            { LCBRACKET }
  | '}'			       { RCBRACKET }
  | '['			       { LBRACKET }
  | ']'            { RBRACKET }
  | '<'			       { LT }
  | '>'			       { GT }
  | ','			       { COMMA }
  | ':'            { COLLON }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { TIMES }
  | '/'            { DEVIDE }
  | '&'            { AND }
  | '|'            { OR }
  | '!'            { NOT }
  | "=="           { EQUAL }
  | "->"           { ARROW }
  | "&&"           { GUARD }
  | '@' ['a'-'z']+ { VARIABLE (Lexing.lexeme lexbuf)}
  | '$' ['a'-'z']+ { CONTEXT (Lexing.lexeme lexbuf)}
  | "true"         { LITERALTRUE }
  | "false"        { LITERALFALSE }
  | ['a'-'z']+     { LITERALSTR (Lexing.lexeme lexbuf)}
  | ['0'-'9']+     { LITERALINT (int_of_string(Lexing.lexeme lexbuf))}
  | eof            { raise Eof }
