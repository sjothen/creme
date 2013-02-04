open Creme
module L = List
module H = Hashtbl
module BI = Big_int

(* Some useful constants *)
let t    = Boolean true
let f    = Boolean false
let zero = Number BI.zero_big_int
let one  = Number BI.unit_big_int

exception Creme_error of string

let err s =
  raise (Creme_error s)

let rec match_ptree env ptree args =
  match ptree, args with
  | Ignore, _                  -> ()
  | Empty, Empty               -> ()
  | Symbol s, a                -> env_define env s a
  | Pair (h, t), Pair (ah, at) -> match_ptree env h ah; match_ptree env t at
  | x, y                       -> err ("could not match ptree " ^ (creme_to_string x) ^ " with " ^ (creme_to_string y))

let rec creme_eval_operative dynenv staticenv formals envformal body args =
  let newenv = env_new (Some staticenv) in
  match_ptree newenv formals args;
  match_ptree newenv envformal (Enviro dynenv);
  creme_eval newenv body
and creme_eval_list e p =
  match p with
  | Empty -> Empty
  | Pair (h, t) -> Pair (creme_eval e h, creme_eval_list e t)
  | f -> err ("arg list must be a proper list; got " ^ (creme_to_string f))
and creme_eval e c =
  match c with
  | Pair (h, t)      ->
      (match creme_eval e h with
      | PrimOperative (_, f) -> f e t
      | Operative (se, f, es, b) -> creme_eval_operative e se f es b t
      (* Applicative is a wrapper around a PrimOperative *)
      | Applicative (PrimOperative (_, f)) -> f e (creme_eval_list e t)
      | Applicative (Operative (se, f, es, b)) -> creme_eval_operative e se f es b (creme_eval_list e t)
      | c -> err ("cannot apply non-operative/applicative " ^ (creme_to_string c) ^ " onto " ^ (creme_to_string t)))
  | Vector a as v    -> v
  | Empty as e       -> e
  | Symbol s         ->
      (match env_get e s with
      | None -> err ("undefined variable " ^ s)
      | Some e -> e)
  | atom             -> atom

let def_operative name fn = 
  let po = PrimOperative (name, fn) in
  env_define toplevel name po

let def_applicative name fn =
  let a = Applicative (PrimOperative (name, fn)) in
  env_define toplevel name a

let vau env exp =
  match exp with
  (* This version of $vau only supports a single body item *)
  | Pair (formals, Pair (envformal, Pair (body, Empty))) ->
      (match formals, envformal with
      | Symbol _, Symbol _ | Ignore, Ignore
      | Pair (_, _), Symbol _ | Symbol _, Ignore
      | Pair (_, _), Ignore | Ignore, Symbol _ ->
          Operative (env, formals, envformal, body)
      | _ -> err "environment formal parameter must be symbol or #ignore")
  | _ -> err "incorrect form in $vau"

let definef env exp =
  match exp with
  | Pair (definiend, Pair (expression, Empty)) ->
      let eexp = creme_eval env expression in
      match_ptree env definiend eexp;
      Inert
  | _ -> err "$define! requires 2 arguments"

let wrap env exp =
  match exp with 
  | Pair (Operative (_, _, _, _) as o, Empty) -> Applicative o
  | Pair (PrimOperative (_, _) as po, Empty) -> Applicative po
  | _ -> err "cannot wrap around non-operative"

let unwrap env exp =
  match exp with
  | Pair (Applicative o, Empty) -> o
  | _ -> err "cannot unwrap non-applicative"

let cons env exp =
  match exp with 
  | Pair (a, Pair (b, Empty)) -> Pair (a, b)
  | _ -> err "cons requires 2 arguments"

let rec nullp env exp =
  match exp with
  | Empty -> t
  | Pair (Empty, tail) -> nullp env tail
  | _ -> f

let rec pairp env exp =
  match exp with
  | Empty -> t
  | Pair (Pair (_, _), tail) -> pairp env tail
  | _ -> f

let rec eval env exp =
  match exp with
  | Pair (expression, Pair (environment, Empty)) -> 
      (match environment with
      | Enviro e -> creme_eval e expression
      | _ -> err "eval requires second argument to be environment")
  | _ -> err "eval requires 2 arguments"

let makeenv env exp =
  match exp with 
  | Empty -> Enviro (env_new None)
  | _ -> err "make-environment takes 0 arguments"

let rec envp env exp =
  match exp with
  | Empty -> t
  | Pair (Enviro e, tail) -> envp env tail
  | _ -> f

let rec ignorep env exp =
  match exp with
  | Empty -> t
  | Pair (Ignore, tail) -> ignorep env tail
  | _ -> f

let rec inertp env exp =
  match exp with
  | Empty -> t
  | Pair (Inert, tail) -> inertp env tail
  | _ -> f

let oif env exp =
  match exp with
  | Pair (test, Pair (consequent, Pair (alternative, Empty))) ->
      let et = creme_eval env test in
      (match et with
      | Boolean true -> creme_eval env consequent
      | Boolean false -> creme_eval env alternative
      | _ -> err "test in $if must eval to boolean")
  | _ -> err "illegal form in $if"

let rec symbolp env exp =
  match exp with
  | Empty -> t
  | Pair (Symbol s, tail) -> symbolp env tail
  | _ -> f

let rec booleanp env exp =
  match exp with
  | Empty -> t
  | Pair (Boolean b, tail) -> booleanp env tail
  | _ -> f

let rec operativep env exp =
  match exp with
  | Empty -> t
  | Pair (Operative (_, _, _, _), tail) -> operativep env tail
  | Pair (PrimOperative (_, _), tail) -> operativep env tail
  | _ -> f

let rec applicativep env exp =
  match exp with
  | Empty -> t
  | Pair (Applicative o, tail) -> applicativep env tail
  | _ -> f

let binop env exp fn start =
  let rec aux ns acc =
    match ns with
    | Empty -> acc
    | Pair (Number b, Empty) -> fn acc b
    | Pair (Number b, t) -> aux t (fn acc b)
    | _ -> err "+ requires number arguments"
  in
  Number (aux exp start)

let plus env exp = binop env exp BI.add_big_int BI.zero_big_int
let mult env exp = binop env exp BI.mult_big_int BI.unit_big_int

let minus env exp =
  match exp with
  | Pair (Number a, Pair (Number b, Empty)) ->  Number (BI.sub_big_int a b)
  | _ -> err "- requires two number arguments"

let neqp env exp =
  match exp with
  | Pair (Number n, Pair (Number m, Empty)) ->
      if BI.eq_big_int n m then t else f
  | _ -> err "=? requires two number arguments"

let define_base () =
  (* 4.1 *)
  def_applicative "boolean?" booleanp;
  (* 4.4 *)
  def_applicative "symbol?" symbolp;
  (* 4.5 *)
  def_applicative "inert?" inertp;
  def_operative "$if" oif;
  (* 4.6 *)
  def_applicative "cons" cons;
  def_applicative "null?" nullp;
  def_applicative "pair?" pairp;
  (* 4.7 *)
  (* 4.8 *)
  def_applicative "environment?" envp;
  def_applicative "ignore?" ignorep;
  def_applicative "eval" eval;
  def_applicative "make-environment" makeenv;
  (* 4.9 *)
  def_operative "$define!" definef;
  (* 4.10 *)
  def_operative "$vau" vau;
  def_applicative "wrap" wrap;
  def_applicative "unwrap" unwrap;
  def_applicative "applicative?" applicativep;
  def_applicative "operative?" operativep;
  def_applicative "=?" neqp;
  def_applicative "+" plus;
  def_applicative "*" mult;
  def_applicative "-" minus
 
let eval c =
  creme_eval toplevel c
