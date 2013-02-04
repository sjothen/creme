{
  exception Eof
  module P = Parser
  module BI = Big_int
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
  | ';' [^ '\n']* '\n'                                { token lexbuf }
  | (alpha | special) (alpha | special | digit)* as s { P.SYMBOL s }
  | digit+ as d                                       { P.NUMBER (BI.big_int_of_string d) }
  | digit+ '.' digit+ as f                            { P.FLOAT (float_of_string f) }
  | "#t"                                              { P.BOOLEAN true }
  | "#f"                                              { P.BOOLEAN false }
  | "#inert"                                          { P.INERT }
  | "#ignore"                                         { P.IGNORE }
  | '('                                               { P.LPAREN }
  | "#("                                              { P.LVECTOR }
  | ')'                                               { P.RPAREN }
  | '.'                                               { P.DOT }
  | '"'                                               { P.STRING (str (Buffer.create 16) lexbuf) }
  | "#\\space"                                        { P.CHAR ' ' }
  | "#\\newline"                                      { P.CHAR '\n' }
  | "#\\" (_ as c)                                    { P.CHAR c }
  | eof                                               { raise Eof }
and str buf = parse
  | '"'                                               { Buffer.contents buf }
  | '\\' 'n'                                          { Buffer.add_char buf '\n'; str buf lexbuf }
  | '\\' '"'                                          { Buffer.add_char buf '"'; str buf lexbuf }
  | '\\' '\\'                                         { Buffer.add_char buf '\\'; str buf lexbuf }
  | _ as c                                            { Buffer.add_char buf c; str buf lexbuf }
