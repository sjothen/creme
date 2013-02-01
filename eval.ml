open Creme
module L = List
module H = Hashtbl

exception Undefined_symbol of string
exception Apply_error of creme
exception Number_error of string
exception Empty_error
exception Arg_error of string * int
exception Type_error of creme

(* Some useful constants *)
let t    = Boolean true
let f    = Boolean false
let zero = Number 0
let one  = Number 1

let toplevel = env_new None

let binop1 ffn ifn = function
  | (Number n, Number m) -> Number (ifn n m)
  | (Float n, Float m)   -> Float (ffn n m)
  | (Number n, Float m)  -> let fn = float_of_int n in Float (ffn fn m)
  | (Float n, Number m)  -> let fm = float_of_int m in Float (ffn n fm)
  | (a, b) -> raise (Type_error a)

let binop name env xs fn start =
  let rec p env xs acc =
    match xs with
    | Empty -> acc
    | Pair (h, t) -> p env t (fn (acc, h))
    | _ -> raise (Number_error name)
  in
  p env xs (Number start)

(* Special form of binary operator where we require at least
 * a single argument which is treated differently than above. *)
let rec binop1arg env xs acc fst fn =
  match xs with
  | Pair (h, Empty) ->
      if fst then fn (acc, h) else fn (acc, h)
  | Pair (h, t) ->
      binop1arg env t (if fst then h else fn (acc, h)) false fn
  | c -> raise (Type_error c)

let plus env xs = binop "+" env xs (binop1 ( +. ) ( + )) 0
let mult env xs = binop "*" env xs (binop1 ( *. ) ( * )) 1

let minus env xs = binop1arg env xs (Number 0) true (binop1 ( -. ) ( - ))
let divide env xs = binop1arg env xs (Number 1) true (binop1 ( /. ) ( / ))

let car env p =
  match p with
  | Pair (h, t) -> (match h with Pair (hh, tt) -> hh | _ -> raise Empty_error)
  | _ -> raise Empty_error

let cdr env p =
  match p with
  | Pair (h, t) -> (match h with Pair (hh, tt) -> tt | _ -> raise Empty_error)
  | _ -> raise Empty_error

let rec creme_eval_args e c =
  match c with
  | Empty -> Empty
  | Pair (h, t) -> Pair (creme_eval e h, creme_eval_args e t)
  | x -> raise (Type_error x)
and creme_eval e c =
  match c with
  | Quoted f         -> f
  | Pair (h, t)      ->
      (match creme_eval e h with
      | Prim (n, f) -> f e (creme_eval_args e t)
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
  def_prim "*" mult;
  def_prim "-" minus;
  def_prim "/" divide;
  def_prim "car" car;
  def_prim "cdr" cdr

let eval c =
  creme_eval toplevel c
