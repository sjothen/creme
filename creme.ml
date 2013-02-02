module H = Hashtbl

type env = Env of env option * (string, creme) H.t
and creme = Number  of int
          | Float   of float
          | Symbol  of string
          | String  of string
          | Char    of char
          | Boolean of bool
          | Quoted  of creme
          | Pair    of creme * creme
          | Vector  of creme array
          | Prim    of string * (env -> creme -> creme)
          | Special of string * (env -> creme -> creme)
          (* closed-env * args * body *)
          | Closure of env * creme * creme
          | Empty
          | Undef

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

let (^$) s c = s ^ (String.make 1 c)

let rec creme_to_string x =
  match x with
  | Number  x      -> string_of_int x
  | Float   f      -> string_of_float f
  | Symbol  s      -> s
  | String  s      -> "\"" ^ s ^ "\""
  | Char    '\n'   -> "#\\newline"
  | Char    ' '    -> "#\\space"
  | Char    c      -> "#\\" ^$ c 
  | Boolean true   -> "#t"
  | Boolean false  -> "#f"
  | Quoted  c      -> "'" ^ (creme_to_string c)
  | Pair    (h, t) -> "(" ^ (creme_inside h t) ^ ")"
  | Vector  a      -> "#(" ^ (creme_inside_vec a) ^ ")"
  | Empty          -> "()"
  | Undef          -> "#(undefined)"
  | Prim (n, fn)   -> "#(primitive " ^ n ^ ")"
  | Special (n, f) -> "#(syntax " ^ n ^ ")"
  | Closure (e, a, b) -> "#(closure)" 
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

let print_creme x = print_endline (creme_to_string x)
