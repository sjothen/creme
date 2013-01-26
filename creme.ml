type creme = Number  of int
           | Float   of float
           | Symbol  of string
           | String  of string
           | Char    of char
           | Boolean of bool
           | Quoted  of creme
           | Pair    of creme * creme
           | Vector  of creme array
           | Empty

let rec creme_to_string x =
  match x with
  | Number  x      -> string_of_int x
  | Float   f      -> string_of_float f
  | Symbol  s      -> s
  | String  s      -> "\"" ^ s ^ "\""
  | Char    '\n'   -> "#\\newline"
  | Char    ' '    -> "#\\space"
  | Char    c      -> "#\\" ^ (String.make 1 c)
  | Boolean true   -> "#t"
  | Boolean false  -> "#f"
  | Quoted  c      -> "'" ^ (creme_to_string c)
  | Pair    (h, t) -> "(" ^ (creme_inside h t) ^ ")"
  | Vector  a      -> "#(" ^ (creme_inside_vec a) ^ ")"
  | Empty          -> "()"
and creme_inside h t =
  match t with
  | Empty         -> creme_to_string h
  | Pair (hh, tt) -> (creme_to_string h) ^ " " ^ (creme_inside hh tt)
  | _             -> (creme_to_string h) ^ " . " ^ (creme_to_string t)
and creme_inside_vec a =
  let rec loop arr s e =
    if s == e then
      ""
    else
      (creme_to_string arr.(s)) ^ (if s+1 == e then "" else " ") ^ (loop arr (s+1) e)
  in
  loop a 0 (Array.length a)

let print_creme x = print_endline (creme_to_string x)
