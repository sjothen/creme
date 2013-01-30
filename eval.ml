open Creme
module L = List
module H = Hashtbl

exception Undefined_symbol of string
exception Apply_error of creme
exception Number_error
exception Empty_error

(* Some useful constants *)
let t    = Boolean true
let f    = Boolean false
let zero = Number 0
let one  = Number 1

let toplevel = env_new None

let binop env xs fn =
  let rec p env xs acc =
    match xs with
    | Empty -> acc
    | Pair (Number n, t) -> p env t (fn acc n)
    | _ -> raise Number_error
  in
  Number (p env xs 0)

let plus env xs = binop env xs (fun a b -> a + b)
let minus env xs = binop env xs (fun a b -> a - b)

let rec creme_eval e c =
  match c with
  | Quoted f         -> f
  | Pair (h, t)      ->
      (match creme_eval e h with
      | Prim (n, f) -> f e t
      | c -> raise (Apply_error c))
  | Vector a as v    -> v
  | Empty as e       -> e
  | Symbol s         ->
      (match env_get e s with
      | None -> raise (Undefined_symbol s)
      | Some e -> e)
  | atom             -> atom

let def_prim name fn =
  let p = Prim (name, fn) in
  env_define toplevel name p

let def_primitives () =
  def_prim "+" plus;
  def_prim "-" minus

let eval c =
  creme_eval toplevel c
