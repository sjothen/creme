type creme = Number  of int
           | Float   of float
           | Symbol  of string
           | String  of string
           | Boolean of bool
           | Quoted  of creme
           | Pair    of creme * creme
           | Empty

let rec creme_to_string x =
  match x with
  | Number  x      -> string_of_int x
  | Float   f      -> string_of_float f
  | Symbol  s      -> s
  | String  s      -> "\"" ^ s ^ "\""
  | Boolean true   -> "#t"
  | Boolean false  -> "#f"
  | Quoted  c      -> "'" ^ (creme_to_string c)
  | Pair    (h, t) -> "(" ^ (creme_inside h t) ^ ")"
  | Empty          -> "()"

and creme_inside h t =
  match t with
  | Empty         -> creme_to_string h
  | Pair (hh, tt) -> (creme_to_string h) ^ " " ^ (creme_inside hh tt)
  | _             -> (creme_to_string h) ^ " . " ^ (creme_to_string t)
 
let print_creme x = print_endline (creme_to_string x)
