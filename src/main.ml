module P = Parser
module L = Lexer
module E = Eval

let perror = Printf.printf
let ctos c = Creme.creme_to_string c

let print_exn e =
  Printf.printf "error: ";
  match e with
  | E.Creme_error s -> perror "%s\n" s
  | x               -> perror "unhandled exception %s\n" (Printexc.to_string x)

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = P.main L.token buf in
  let evd = E.eval tok in
  Creme.creme_print evd;
  repl prompt buf

let load fname =
  let rec aux c =
    let tok = P.main L.token c in
    ignore (E.eval tok);
    aux c
  in
  try
    aux (Lexing.from_channel (open_in fname))
  with L.Eof -> () | x -> print_exn x

let rec repl_ f =
  try
    f ()
  with
  | L.Eof -> exit 0
  | x     -> print_exn x; repl_ f

let _ =
  E.define_base ();
  load "boot/base.crm";
  if Array.length Sys.argv > 1 then
    load Sys.argv.(1)
  else
    repl_ (fun () -> repl "> " (Lexing.from_channel stdin))
