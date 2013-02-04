module P = Parser
module L = Lexer
module E = Eval

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = P.main L.token buf in
  let evd = E.eval tok in
  Creme.creme_print evd;
  repl prompt buf

let rec load chan =
  let tok = P.main L.token chan in
  ignore (E.eval tok);
  load chan

let perror = Printf.printf
let ctos c = Creme.creme_to_string c

let print_exn e =
  Printf.printf "error: ";
  match e with
  | E.Creme_error s -> perror "%s\n" s
  | x               -> perror "unhandled exception %s\n" (Printexc.to_string x)

let rec repl_ f =
  try
    f ()
  with
  | L.Eof -> exit 0
  | x     -> print_exn x; repl_ f

let _ =
  E.define_base ();
  try
    load (Lexing.from_channel (open_in "boot/base.crm"))
  with
  | L.Eof -> repl_ (fun () -> repl "> " (Lexing.from_channel stdin))
  | x     -> print_exn x
