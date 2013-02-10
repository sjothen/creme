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

  let char_to_num c =
    let code = Char.code c in
    if code >= 48 && code <= 57 then code - 48 else
    if code >= 65 && code <= 70 then code - 65 + 10 else
    if code >= 97 && code <= 102 then code - 97 + 10 else -1

  let str_to_list s =
    let rec aux len lst =
      if len == 0 then
        lst
      else
        aux (len - 1) (s.[len - 1] :: lst)
    in
    aux (String.length s) []

  let str_to_bi str shift =
    let rec aux bs bi =
      match bs with
      | []     -> bi
      | h :: t ->
          let shifted = BI.shift_left_big_int bi shift in
          let added = BI.add_big_int shifted (BI.big_int_of_int h) in
          aux t added
    in
    aux (List.map char_to_num (str_to_list str)) BI.zero_big_int
}

let special = [
  '+' '-' '*' '/' '<' '=' '>' '!' '?' ':' '$'
  '%' '_' '&' '~' '^'
]
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let hex = ['a'-'f' 'A'-'F']
let oct = ['0'-'7']
let space = [' ' '\t' '\n' '\r']

rule token = parse
  | [' ' '\t' '\r']+                                  { token lexbuf }
  | '\n'                                              { Lexing.new_line lexbuf; token lexbuf }
  | ';' [^ '\n']* '\n'                                { Lexing.new_line lexbuf; token lexbuf }
  | (alpha | special) (alpha | special | digit)* as s { SYMBOL s }
  | digit+ as d                                       { NUMBER (BI.big_int_of_string d) }
  | digit+ '.' digit+ as f                            { FLOAT (float_of_string f) }
  | "#b" (('0' | '1')+ as b)                          { NUMBER (str_to_bi b 1) }
  | "#x" (hex+ as h)                                  { NUMBER (str_to_bi h 4) }
  | "#o" (oct+ as o)                                  { NUMBER (str_to_bi o 3) }
  | "#d" (digit+ as d)                                { NUMBER (BI.big_int_of_string d) }
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
