open Creme
module L = List
module H = Hashtbl

exception Undefined_symbol of string
exception Apply_error of creme
exception Number_error of string
exception Empty_error
exception Arg_error of string * int
exception Type_error of creme

exception CremeException of string

(* Some useful constants *)
let t    = Boolean true
let f    = Boolean false
let zero = Number 0
let one  = Number 1

let toplevel = env_new None

let err s =
  raise (CremeException s)

let rec match_pform env pform args =
  match pform, args with
  | Ignore, _                  -> ()
  | Empty, Empty               -> ()
  | Symbol s, a                -> env_define env s a
  | Pair (h, t), Pair (ah, at) -> match_pform env h ah; match_pform env t at
  | _, _                       -> err "unexpected form in match"

let rec creme_eval_operative dynenv staticenv formals envformal body args =
  let newenv = env_new (Some staticenv) in
  match_pform newenv formals args;
  (match envformal with
  | Ignore -> ()
  | Symbol s -> env_define newenv s (Enviro dynenv)
  | _ -> err "environment formal must be either #ignore or a symbol");
  creme_eval newenv body
and creme_eval e c =
  match c with
  | Pair (h, t)      ->
      (match creme_eval e h with
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
      let eexp = creme_eval env expression in
      match_pform env definiend eexp;
      Inert
  | _ -> err "$define! requires 2 arguments"

let define_base () =
  def_operative "$vau" vau;
  def_operative "$define!" definef

let eval c =
  creme_eval toplevel c
