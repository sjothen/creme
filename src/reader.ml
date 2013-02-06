module L = Lexer
module C = Creme

exception Read_error of string * int * int * string

let err buf str =
  let curr = buf.Lexing.lex_curr_p in
  let line = curr.Lexing.pos_lnum in
  let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
  let name = curr.Lexing.pos_fname in 
  raise (Read_error (name, line, cnum, str))

let token_to_creme buf t =
  match t with
  | L.NUMBER n  -> C.Number n
  | L.FLOAT f   -> C.Float f
  | L.SYMBOL s  -> C.Symbol s
  | L.STRING s  -> C.String s
  | L.BOOLEAN b -> C.Boolean b
  | L.CHAR c    -> C.Char c
  | L.INERT     -> C.Inert
  | L.IGNORE    -> C.Ignore
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
  C.Vector (Array.of_list (aux buf))
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
  | L.RPAREN -> C.Empty
  | L.LPAREN -> 
      let fst = read_list buf true in
      let snd = read_list buf false in
      C.Pair (fst, snd)
  | L.LVECTOR -> 
      let vec = read_vector buf in
      C.Pair (vec, read_list buf false)
  | t        ->
      let ntok = L.token buf in
      match ntok with
      | L.LPAREN ->
          let nlist = read_list buf true in
          let cont = read_list buf false in
          C.Pair (token_to_creme buf t, C.Pair (nlist, cont))
      | L.LVECTOR ->
          let vec = read_vector buf in
          let cont = read_list buf false in
          C.Pair (token_to_creme buf t, C.Pair (vec, cont))
      | L.RPAREN -> C.Pair (token_to_creme buf t, C.Empty)
      | L.EOF    -> err buf "unexpected eof"
      | L.DOT    ->
          let cdr = L.token buf in
          let last = (match cdr with
          | L.RPAREN -> err buf "expected atom after dot in list"
          | L.EOF    -> err buf "unexpected eof"
          | L.LPAREN -> C.Pair (token_to_creme buf t, read_list buf true)
          | L.LVECTOR -> C.Pair (token_to_creme buf t, read_vector buf)
          | s        -> C.Pair (token_to_creme buf t, token_to_creme buf cdr)) in
          let par = L.token buf in
          (match par with
          | L.RPAREN -> last
          | _        -> err buf "expected closing paren after dot and final atom")
      | u        -> C.Pair (token_to_creme buf t, C.Pair (token_to_creme buf u, read_list buf false))
