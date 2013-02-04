module S = String
module L = List

let last s =
  let len = S.length s in
  (S.sub s 0 (len - 1), S.get s (len - 1))

let rec generateform name =
  match last name with
  | ("", 'a') -> "(x . #ignore)"
  | ("", 'd') -> "(#ignore . x)"
  | (pfix, 'a') -> "(" ^ (generateform pfix) ^ " . #ignore)"
  | (pfix, 'd') -> "(#ignore . " ^ (generateform pfix) ^ ")"
  | _ -> ""

let generatedef name =
  let f = generateform name in
  "($define! c" ^ name ^ "r ($lambda (" ^ f ^ ") x))"

let rec generate len from =
  if len = 1 then
    from
  else
    generate (len - 1) (L.flatten (L.map (fun x -> ["a"^x; "d"^x]) from))

let genlen n = L.iter (fun x -> print_endline (generatedef x)) (generate n ["a"; "d"])

(* Generate definitions for car, cdr, caar, cadr, ... *)
let _ =
  genlen 1;
  print_endline "";
  genlen 2;
  print_endline "";
  genlen 3;
  print_endline "";
  genlen 4
