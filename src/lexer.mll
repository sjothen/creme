{
  module BI = Big_int

  type token =
    | NUMBER of (BI.big_int)
    | FLOAT of (float)
    | SYMBOL of (string)
    | STRING of (string)
    | BOOLEAN of (bool)
    | CHAR of (char)
    | INERT
    | IGNORE
    | DOT
    | RPAREN
    | LPAREN
    | LVECTOR
    | EOF

  let str_to_list s =
    let rec aux len lst =
      if len == 0 then
        lst
      else
        aux (len - 1) (s.[len - 1] :: lst)
    in
    aux (String.length s) []

  let bin_to_bi s =
    let rec aux bs bi =
      match bs with
      | []     -> bi
      | h :: t ->
          if h == '0' then
            aux t (BI.shift_left_big_int bi 1)
          else
            aux t (BI.add_big_int (BI.shift_left_big_int bi 1) BI.unit_big_int)
    in
    aux (List.rev (str_to_list s)) BI.zero_big_int
}

let special = [
  '+' '-' '*' '/' '<' '=' '>' '!' '?' ':' '$'
  '%' '_' '&' '~' '^'
]
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let space = [' ' '\t' '\n' '\r']

rule token = parse
  | [' ' '\t' '\r']+                                  { token lexbuf }
  | '\n'                                              { Lexing.new_line lexbuf; token lexbuf }
  | ';' [^ '\n']* '\n'                                { Lexing.new_line lexbuf; token lexbuf }
  | (alpha | special) (alpha | special | digit)* as s { SYMBOL s }
  | digit+ as d                                       { NUMBER (BI.big_int_of_string d) }
  | digit+ '.' digit+ as f                            { FLOAT (float_of_string f) }
  | "#b" (('0' | '1')+ as b)                          { NUMBER (bin_to_bi b) }
  | "#t"                                              { BOOLEAN true }
  | "#f"                                              { BOOLEAN false }
  | "#inert"                                          { INERT }
  | "#ignore"                                         { IGNORE }
  | '('                                               { LPAREN }
  | "#("                                              { LVECTOR }
  | ')'                                               { RPAREN }
  | '.'                                               { DOT }
  | '"'                                               { STRING (str (Buffer.create 16) lexbuf) }
  | "#\\space"                                        { CHAR ' ' }
  | "#\\newline"                                      { CHAR '\n' }
  | "#\\" '\n'                                        { Lexing.new_line lexbuf; CHAR '\n' }
  | "#\\" (_ as c)                                    { CHAR c }
  | eof                                               { EOF }
and str buf = parse
  | '"'                                               { Buffer.contents buf }
  | '\\' 'n'                                          { Buffer.add_char buf '\n'; str buf lexbuf }
  | '\\' '"'                                          { Buffer.add_char buf '"'; str buf lexbuf }
  | '\\' '\\'                                         { Buffer.add_char buf '\\'; str buf lexbuf }
  | '\n'                                              { Buffer.add_char buf '\n'; Lexing.new_line lexbuf; str buf lexbuf }
  | _ as c                                            { Buffer.add_char buf c; str buf lexbuf }
