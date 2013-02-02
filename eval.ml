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

let car_ p =
  match p with
  | Pair (h, t) -> h
  | _ -> raise Empty_error

let cdr_ p =
  match p with 
  | Pair (h, t) -> t
  | _ -> raise Empty_error

(* We expect list of args to be a list of a single item, we unwrap
 * it and pass it to fn. *)
let unwrap1 env p fn =
  match p with
  | Pair (h, t) -> fn h
  | _ -> raise Empty_error

let compose f g = fun x -> f (g x)

let car env p   = unwrap1 env p car_
let cdr env p   = unwrap1 env p cdr_
let cadr env p  = unwrap1 env p (compose car_ cdr_)
let caar env p  = unwrap1 env p (compose car_ car_)
let cddr env p  = unwrap1 env p (compose cdr_ cdr_)
let caaar env p = unwrap1 env p (compose car_ (compose car_ car_))
let caadr env p = unwrap1 env p (compose car_ (compose car_ cdr_))
let cadar env p = unwrap1 env p (compose car_ (compose cdr_ car_))
let caddr env p = unwrap1 env p (compose car_ (compose cdr_ cdr_))
let cdaar env p = unwrap1 env p (compose cdr_ (compose car_ car_))
let cdadr env p = unwrap1 env p (compose cdr_ (compose car_ cdr_))
let cddar env p = unwrap1 env p (compose cdr_ (compose cdr_ car_))
let ccddr env p = unwrap1 env p (compose cdr_ (compose cdr_ cdr_))

exception CremeException of string

let err s =
  raise (CremeException s)

let rec zip_to_env e pars args =
  match (pars, args) with
  | (Pair (Symbol ph, pt), Pair (ah, at)) ->
      env_define e ph ah;
      zip_to_env e pt at
  | (Empty, Empty) -> e
  | (Symbol restpar, restarg) -> env_define e restpar restarg; e
  | _ -> raise Empty_error

let rec match_pform env pform args =
  match pform, args with
  | Ignore, _                  -> ()
  | Empty, Empty               -> ()
  | Symbol s, a                -> env_define env s a
  | Pair (h, t), Pair (ah, at) -> match_pform env h ah; match_pform env t at
  | _, _                       -> err "unexpected form in match"

let rec creme_apply_closure c args =
  match c with
  | Closure (env, params, body) -> 
      let newenv = env_new (Some env) in
      let envwargs = zip_to_env newenv params args in
      creme_eval_body envwargs body
  | _ -> raise Empty_error
and creme_eval_body env body =
  match body with 
  | Pair (form, Empty) -> 
      creme_eval env form
  | Pair (form, t) ->
      ignore (creme_eval env form);
      creme_eval_body env t
 | _ -> raise Empty_error
and creme_eval_args e c =
  match c with
  | Empty -> Empty
  | Pair (h, t) -> 
      let ea = creme_eval e h in
      Pair (ea, creme_eval_args e t)
  | x -> raise (Type_error x)
and creme_eval_operative dynenv staticenv formals envformal body args =
  let newenv = env_new (Some staticenv) in
  match_pform newenv formals args;
  (match envformal with
  | Ignore -> ()
  | Symbol s -> env_define newenv s (Enviro dynenv)
  | _ -> err "environment formal must be ignore/symbol");
  creme_eval newenv body
and creme_eval e c =
  match c with
  | Quoted f         -> f
  | Pair (h, t)      ->
      (match creme_eval e h with
      | Prim (n, f) -> f e (creme_eval_args e t)
      | Special (n, f) -> f e t
      (* Don't match env part as e, it shadows e param in creme_eval *)
      | Closure (_, a, b) as c ->
          let eargs = creme_eval_args e t in
          creme_apply_closure c eargs
      | PrimOperative (_, f) -> f e t
      | Operative (se, f, es, b) -> creme_eval_operative e se f es b t
      | c -> raise (Apply_error c))
  | Vector a as v    -> v
  | Empty as e       -> e
  | Symbol s         ->
      (match env_get e s with
      | None -> raise (Undefined_symbol s)
      | Some e -> e)
  | atom             -> atom

let runif env exp =
  match exp with
  | Pair (cond, Pair (texp, Empty)) ->
      let tval = creme_eval env cond in
      (match tval with
      | Boolean false -> Undef
      | _ -> creme_eval env texp)
  | Pair (cond, Pair (texp, Pair (fexp, Empty))) ->
      let tval = creme_eval env cond in
      (match tval with
      | Boolean false -> creme_eval env fexp
      | _ -> creme_eval env texp)
  | _ -> err "if takes 2 or 3 arguments"

let neq env exp =
  match exp with
  | Pair (Number n, Pair (Number m, Empty)) -> Boolean (n = m)
  | _ -> err "= takes 2 number arguments"

let define env args =
  match args with
  | Pair (Symbol x, Pair (h, t)) -> env_define env x (creme_eval env h); Undef
  | _ -> raise Empty_error

let set env args =
  match args with
  | Pair (Symbol x, Pair (h, t)) -> env_set env x (creme_eval env h); Undef
  | _ -> raise Empty_error

let lambda env args =
  match args with
  | Pair ((Symbol rest) as s, body) -> Closure (env, s, body)
  | Pair (Pair (arg, t) as p, body) -> Closure (env, p, body)
  | _ -> raise Empty_error
 
let def_prim name fn =
  let p = Prim (name, fn) in
  env_define toplevel name p

let def_spec name fn =
  let s = Special (name, fn) in
  env_define toplevel name s

let def_operative name fn = 
  let po = PrimOperative (name, fn) in
  env_define toplevel name po

let vau env exp =
  match exp with
  (* This version of $vau only supports a single body item *)
  | Pair (formals, Pair (envformal, Pair (body, Empty))) ->
      (match formals, envformal with
      | Symbol _, Symbol _ | Ignore, Ignore
      | Pair (_, _), Symbol _ | Symbol _, Ignore
      | Pair (_, _), Ignore ->
          Operative (env, formals, envformal, body)
      | _ -> err "environment formal parameter must be symbol or #ignore")
  | _ -> err "incorrect form in $vau"

let definef env exp =
  match exp with
  | Pair (definiend, Pair (expression, Empty)) ->
      match_pform env definiend expression;
      Inert
  | _ -> err "$define! requires 2 arguments"

let def_primitives () =
  def_prim "+" plus;
  def_prim "*" mult;
  def_prim "-" minus;
  def_prim "/" divide;
  def_prim "=" neq;
  def_prim "car" car;
  def_prim "cdr" cdr;
  def_prim "caar" caar;
  def_prim "cadr" cadr;
  def_prim "cddr" cddr;
  def_spec "define" define;
  def_spec "lambda" lambda;
  def_spec "if" runif;
  def_spec "set!" set;
  def_operative "$vau" vau;
  def_operative "$define!" definef

let eval c =
  creme_eval toplevel c
