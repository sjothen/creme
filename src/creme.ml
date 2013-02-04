module H = Hashtbl
module BI = Big_int

type env = Env of env option * (string, creme) H.t
and creme = Number  of BI.big_int
          | Float   of float
          | Symbol  of string
          | String  of string
          | Char    of char
          | Boolean of bool
          | Pair    of creme * creme
          | Vector  of creme array
          | Enviro  of env
          | PrimOperative of string * (env -> creme -> creme)
          (* static-env * formals * formal-env * body *)
          | Operative of env * creme * creme * creme
          (* operative *)
          | Applicative of creme
          | Empty
          | Inert
          | Ignore

let env_new p = Env (p, H.create 16)
let env_define (Env (p, h)) k v = H.add h k v

let rec env_find_ht (Env (p, h)) k =
  if H.mem h k then Some h else
    match p with
    | None -> None
    | Some p -> env_find_ht p k

let env_set e k v =
  match env_find_ht e k with
  | None -> env_define e k v
  | Some ht -> H.replace ht k v

let env_get e k =
  match env_find_ht e k with
  | None -> None
  | Some ht -> Some (H.find ht k)

let toplevel = env_new None

let (^$) s c = s ^ (String.make 1 c)

let rec creme_to_string x =
  match x with
  | Number  x      -> BI.string_of_big_int x
  | Float   f      -> string_of_float f
  | Symbol  s      -> s
  | String  s      -> "\"" ^ (String.escaped s) ^ "\""
  | Char    '\n'   -> "#\\newline"
  | Char    ' '    -> "#\\space"
  | Char    c      -> "#\\" ^$ c 
  | Boolean true   -> "#t"
  | Boolean false  -> "#f"
  | Pair    (h, t) -> "(" ^ (creme_inside h t) ^ ")"
  | Vector  a      -> "#(" ^ (creme_inside_vec a) ^ ")"
  | Empty          -> "()"
  | Enviro  e      -> "#(environment)"
  | Operative (_, _, _, _) -> "#(operative)"
  | PrimOperative (n, _) -> "#(operative " ^ n ^ ")"
  | Applicative o  -> "#(applicative)"
  | Inert          -> "#inert"
  | Ignore         -> "#ignore"
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

let rec env_print e =
  match e with
  | Env (Some p, ht) ->
      (
      H.iter (fun s c -> Printf.printf "%s => %s\n" s (creme_to_string c)) ht;
      env_print p
      )
  | Env (None, ht) ->
      H.iter (fun s c -> Printf.printf "%s => %s\n" s (creme_to_string c)) ht;
      Printf.printf "\n"

let creme_print x = print_endline (creme_to_string x)
