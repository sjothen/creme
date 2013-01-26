{
  module P = Parser
}

let special = [
  '+' '-' '*' '/' '<' '=' '>' '!' '?' ':' '$'
  '%' '_' '&' '~' '^'
]
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = [' ' '\t' '\n' '\r']
  
rule token = parse
  | space+                                            { token lexbuf }
  | (alpha | special) (alpha | special | digit)* as s { P.SYMBOL s }
  | digit+ as d                                       { P.NUMBER (int_of_string d) }
  | digit+ '.' digit+ as f                            { P.FLOAT (float_of_string f) }
  | "#t"                                              { P.BOOLEAN true }
  | "#f"                                              { P.BOOLEAN false }
  | '('                                               { P.LPAREN }
  | ')'                                               { P.RPAREN }
  | '.'                                               { P.DOT }
  | '''                                               { P.QUOTE }
  | '"' ([^ '"']* as s) '"'                           { P.STRING s }
  | eof                                               { raise End_of_file }
