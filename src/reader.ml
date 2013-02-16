open Creme
module L = Lexer

exception Read_error of string * int * int * string

let err buf str =
  let curr = buf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let name = curr.Lexing.pos_fname in 
  raise (Read_error (name, line, cnum, str))

let token_to_creme buf t =
  match t with
  | L.NUMBER n  -> Number n
  | L.FLOAT f   -> Float f
  | L.SYMBOL s  -> Symbol s
  | L.STRING s  -> String s
  | L.BOOLEAN b -> Boolean b
  | L.CHAR c    -> Char c
  | L.INERT     -> Inert
  | L.IGNORE    -> Ignore
  | L.RPAREN    -> err buf "expected atom, got closing paren"
  | L.LPAREN    -> err buf "expected atom, got opening paren"
  | L.LVECTOR   -> err buf "expected atom, got opening vector"
  | L.DOT       -> err buf "expected atom, got dot"
  | L.EOF       -> err buf "expected atom, got eof"
 
let rec read buf =
  let tok = L.token buf in
  match tok with
  | L.EOF       -> None
  | L.LPAREN    -> Some (read_list buf true)
  | L.LVECTOR   -> Some (read_vector buf)
  | L.DOT       -> err buf "unexpected dot"
  | L.RPAREN    -> err buf "unexpected closing paren"
  | t           -> Some (token_to_creme buf t)
and read_vector buf =
  let rec aux buf =
    let tok = L.token buf in
    match tok with
    | L.EOF    -> err buf "unexpected eof in vector"
    | L.DOT    -> err buf "unexpected dot in vector"
    | L.RPAREN -> []
    | L.LPAREN ->
        let xs = read_list buf true in
        xs :: (aux buf)
    | L.LVECTOR ->
        let vec = read_vector buf in
        vec :: (aux buf)
    | atom     -> (token_to_creme buf atom) :: (aux buf)
  in
  Vector (Array.of_list (aux buf))
and read_list buf fstcall =
  let tok = L.token buf in
  match tok with
  | L.EOF    -> err buf "unexpected eof in list"
  | L.DOT    -> 
      (if fstcall then
        err buf "expected atom, got dot"
      else
        let s = L.token buf in
        let last = (match s with
        | L.RPAREN  -> err buf "unexpected paren after dot"
        | L.EOF     -> err buf "unexpected eof after dot"
        | L.DOT     -> err buf "expected atom after dot, not dot"
        | L.LPAREN  -> read_list buf true
        | L.LVECTOR -> read_vector buf
        | s         -> token_to_creme buf s) in
        let par = L.token buf in
        match par with
        | L.RPAREN  -> last
        | _         -> err buf "expected closing paren after dot and final atom")
  | L.RPAREN -> Empty
  | L.LPAREN -> 
      let fst = read_list buf true in
      let snd = read_list buf false in
      Pair {car=fst; cdr=snd}
  | L.LVECTOR -> 
      let vec = read_vector buf in
      Pair {car=vec; cdr=read_list buf false}
  | t        ->
      let ntok = L.token buf in
      match ntok with
      | L.LPAREN ->
          let nlist = read_list buf true in
          let cont = read_list buf false in
          Pair {car=token_to_creme buf t; cdr=Pair {car=nlist; cdr=cont}}
      | L.LVECTOR ->
          let vec = read_vector buf in
          let cont = read_list buf false in
          Pair {car=token_to_creme buf t; cdr=Pair {car=vec; cdr=cont}}
      | L.RPAREN -> Pair {car=token_to_creme buf t; cdr=Empty}
      | L.EOF    -> err buf "unexpected eof"
      | L.DOT    ->
          let cdr = L.token buf in
          let last = (match cdr with
          | L.RPAREN -> err buf "expected atom after dot in list"
          | L.EOF    -> err buf "unexpected eof"
          | L.LPAREN -> Pair {car=token_to_creme buf t; cdr=read_list buf true}
          | L.LVECTOR -> Pair {car=token_to_creme buf t; cdr=read_vector buf}
          | s        -> Pair {car=token_to_creme buf t; cdr=token_to_creme buf cdr}) in
          let par = L.token buf in
          (match par with
          | L.RPAREN -> last
          | _        -> err buf "expected closing paren after dot and final atom")
      | u        -> Pair {car=token_to_creme buf t; cdr=Pair {car=token_to_creme buf u; cdr=read_list buf false}}
