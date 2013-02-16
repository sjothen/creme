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
  | Ignore, _ -> ()
  | Empty, Empty -> ()
  | Symbol s, a -> env_define env s a
  | Pair {car=h; cdr=t}, Pair {car=ah; cdr=at} ->
      match_ptree env h ah; match_ptree env t at
  | x, y ->
      err ("could not match ptree " ^ (creme_to_string x) ^ " with " ^ (creme_to_string y))

let rec creme_eval_operative dynenv staticenv formals envformal body args =
  let newenv = env_new (Some staticenv) in
  match_ptree newenv formals args;
  match_ptree newenv envformal (Enviro dynenv);
  creme_eval_body newenv body
and creme_eval_body e b =
  match b with
  | Empty ->
      Inert
  | Pair {car=h; cdr=Empty} ->
      creme_eval e h
  | Pair {car=h; cdr=t} ->
      ignore (creme_eval e h);
      creme_eval_body e t
  | _ -> err "$vau: unreachable"
and creme_eval_list e p =
  match p with
  | Empty -> Empty
  | Pair {car=h; cdr=t} -> Pair {car=creme_eval e h; cdr=creme_eval_list e t}
  | f -> err ("arg list must be a proper list; got " ^ (creme_to_string f))
and creme_eval e c =
  match c with
  | Pair {car=h; cdr=t} ->
      (match creme_eval e h with
      | PrimOperative (_, f) -> f e t
      | Operative (se, f, es, b) -> creme_eval_operative e se f es b t
      (* Applicative is a wrapper around a PrimOperative *)
      | Applicative (PrimOperative (_, f)) ->
            let el = creme_eval_list e t in
            f e el
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
  | Pair {car=formals; cdr=Pair {car=eformal; cdr=body}} ->
      (match formals, eformal with
      | Symbol _, Symbol _ | Ignore, Ignore
      | Pair _, Symbol _ | Symbol _, Ignore
      | Pair _, Ignore | Ignore, Symbol _
      | Empty, Ignore | Empty, Symbol _ ->
          Operative (env, formals, eformal, body)
      | _ ->
          err "environment formal parameter must be symbol or #ignore")
  | _ -> err "incorrect form in $vau"

let definef env exp =
  match exp with
  | Pair {car=definiend; cdr=Pair {car=expression; cdr=Empty}} ->
      let eexp = creme_eval env expression in
      match_ptree env definiend eexp;
      Inert
  | _ -> err "$define! requires 2 arguments"

let wrap env exp =
  match exp with 
  | Pair {car=Operative (_, _, _, _) as o; cdr=Empty} -> Applicative o
  | Pair {car=PrimOperative (_, _) as po; cdr=Empty} -> Applicative po
  | _ -> err "cannot wrap around non-operative"

let unwrap env exp =
  match exp with
  | Pair {car=Applicative o; cdr=Empty} -> o
  | _ -> err "cannot unwrap non-applicative"

let cons env exp =
  match exp with 
  | Pair {car=a; cdr=Pair {car=b; cdr=Empty}} -> Pair {car=a; cdr=b}
  | _ -> err "cons requires 2 arguments"

let rec nullp env exp =
  match exp with
  | Empty -> t
  | Pair {car=Empty; cdr=tail} -> nullp env tail
  | _ -> f

let rec pairp env exp =
  match exp with
  | Empty -> t
  | Pair {car=Pair p; cdr=tail} -> pairp env tail
  | _ -> f

let rec eval env exp =
  match exp with
  | Pair {car=expression; cdr=Pair {car=environment; cdr=Empty}} ->
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
  | Pair {car=Enviro e; cdr=tail} -> envp env tail
  | _ -> f

let rec ignorep env exp =
  match exp with
  | Empty -> t
  | Pair {car=Ignore; cdr=tail} -> ignorep env tail
  | _ -> f

let rec inertp env exp =
  match exp with
  | Empty -> t
  | Pair {car=Inert; cdr=tail} -> inertp env tail
  | _ -> f

let oif env exp =
  match exp with
  | Pair {car=test; cdr=Pair{car=consequent; cdr=Pair{car=alternative; cdr=Empty}}} ->
      let et = creme_eval env test in
      (match et with
      | Boolean true -> creme_eval env consequent
      | Boolean false -> creme_eval env alternative
      | _ -> err "test in $if must eval to boolean")
  | _ -> err "illegal form in $if"

let rec symbolp env exp =
  match exp with
  | Empty -> t
  | Pair {car=Symbol s; cdr=tail} -> symbolp env tail
  | _ -> f

let rec booleanp env exp =
  match exp with
  | Empty -> t
  | Pair {car=Boolean b; cdr=tail} -> booleanp env tail
  | _ -> f

let rec operativep env exp =
  match exp with
  | Empty -> t
  | Pair {car=Operative (_, _, _, _); cdr=tail} -> operativep env tail
  | Pair {car=PrimOperative (_, _); cdr=tail} -> operativep env tail
  | _ -> f

let rec applicativep env exp =
  match exp with
  | Empty -> t
  | Pair {car=Applicative o; cdr=tail} -> applicativep env tail
  | _ -> f

let to_float a =
  match a with
  | Number n      -> Float (BI.float_of_big_int n)
  | Float fs as f -> f
  | _             -> err "attempt to cast uncastable to float"

let cast a b =
  match a, b with
  | Float f as a, b -> (a, to_float b)
  | a, (Float f as b) -> (to_float a, b)
  | a, b -> (a, b)

let helper name ffn ifn a b =
  match cast a b with
  | Float a, Float b -> Float (ffn a b)
  | Number a, Number b -> Number (ifn a b)
  | a, b -> err (name ^ ": cannot operate on non-numeric value")

let p = helper "+" ( +. ) BI.add_big_int
let s = helper "-" ( -. ) BI.sub_big_int
let d = helper "/" ( /. ) BI.div_big_int
let m = helper "*" ( *. ) BI.mult_big_int

let twoarg name fn exp =
  match exp with
  | Pair {car=a; cdr=Pair {car=b; cdr=Empty}} -> fn a b
  | _ -> err (name ^ ": requires 2 arguments")

let plus env exp = twoarg "+" p exp
let mult env exp = twoarg "*" m exp
let subt env exp = twoarg "-" s exp
let divd env exp = twoarg "/" d exp

(*
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
*)

let neqp env exp =
  match exp with
  | Pair {car=Number n; cdr=Pair {car=Number m; cdr=Empty}} ->
      if BI.eq_big_int n m then t else f
  | _ -> err "=? requires two number arguments"

let display env exp =
  match exp with
  | Pair {car=h; cdr=Empty} ->
      (match h with
      | String s -> print_string s
      | Char c -> print_char c
      | f -> print_string (creme_to_string f));
      flush stdout;
      Inert
  | _ -> err "display requires 1 argument"

let eqp env exp =
  match exp with
  | Pair {car=fst; cdr=Pair {car=snd; cdr=Empty}} -> creme_cmp fst snd
  | _ -> err "eq? requires 2 arguments"

let gtep env exp =
  match exp with
  | Pair {car=Number n; cdr=Pair {car=Number m; cdr=Empty}} -> Boolean (BI.ge_big_int n m)
  | _ -> err ">=? requires 2 number arguments"

let ltep env exp =
  match exp with
  | Pair {car=Number n; cdr=Pair {car=Number m; cdr=Empty}} -> Boolean (BI.le_big_int n m)
  | x -> creme_print x; err "<=? requires 2 number arguments"

let gtp env exp =
  match exp with
  | Pair {car=Number n; cdr=Pair {car=Number m; cdr=Empty}} -> Boolean (BI.gt_big_int n m)
  | _ -> err ">? requires 2 number arguments"

let ltp env exp =
  match exp with
  | Pair {car=Number n; cdr=Pair {car=Number m; cdr=Empty}} -> Boolean (BI.lt_big_int n m)
  | _ -> err "<? requires 2 number arguments"

let error env exp =
  match exp with
  | Pair {car=String s; cdr=Empty} -> err s
  | _ -> err "err: requires 1 argument"

let rec seq env exp =
  match exp with
  | Empty ->
      Inert
  | Pair {car=h; cdr=Empty} ->
      creme_eval env h
  | Pair {car=h; cdr=t} ->
      ignore (creme_eval env h);
      seq env t
  | _ -> err "$sequence: unreachable"

let lambda env exp =
  match exp with
  | Pair {car=formals; cdr=body} ->
      let op = vau env (Pair {car=formals; cdr=Pair {car=Ignore; cdr=body}}) in
      Applicative op
  | _ -> err "$lambda: incorrect form"

let setcar env exp =
  match exp with
  | Pair {car=var; cdr=Pair {car=value; cdr=Empty}} ->
      (match var with
      | Pair p -> p.car <- value; Inert
      | _ -> err "set-car!: 1st argument must be a pair")
  | _ -> err "set-car!: incorrect form"

let setcdr env exp =
  match exp with
  | Pair {car=var; cdr=Pair {car=value; cdr=Empty}} ->
      (match var with
      | Pair p -> p.cdr <- value; Inert
      | _ -> err "set-cdr!: 1st argument must be a pair")
  | _ -> err "set-cdr!: incorrect form"

let define_base () =
  def_operative "$lambda" lambda;
  def_operative "$sequence" seq;
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
  def_applicative "-" subt;
  def_applicative "/" divd;
  def_applicative "display" display;
  def_applicative "eq?" eqp;
  def_applicative "equal?" eqp;
  def_applicative ">=?" gtep;
  def_applicative "<=?" ltep;
  def_applicative ">?" gtp;
  def_applicative "<?" ltp;
  def_applicative "error" error;
  def_applicative "set-car!" setcar;
  def_applicative "set-cdr!" setcdr
 
let eval c =
  creme_eval toplevel c
