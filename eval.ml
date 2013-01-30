open Creme
module L = List
module H = Hashtbl

(* Some useful constants *)
let t    = Boolean true
let f    = Boolean false
let zero = Number 0
let one  = Number 1

type env = Env of env option * (string, creme) H.t

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

exception Undefined_symbol of string

let rec creme_eval c e =
  match c with
  | Quoted f         -> f
  | Pair (h, t) as p -> p
  | Vector a as v    -> v
  | Empty as e       -> e
  | Symbol s         ->
      (match env_get e s with
      | None -> raise (Undefined_symbol s)
      | Some e -> e)
  | atom             -> atom

let eval c = creme_eval c toplevel
