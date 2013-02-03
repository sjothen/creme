module P = Parser
module L = Lexer
module E = Eval

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = P.main L.token buf in
  let evd = E.eval tok in
  Creme.print_creme evd;
  repl prompt buf

let rec load chan =
  let tok = P.main L.token chan in
  ignore (E.eval tok);
  load chan

let perror = Printf.printf

let rec repl_ f =
  try
    f ()
  with
  | L.Eof -> exit 0
  | E.Undefined_symbol s -> perror "undefined symbol %s\n" s; repl_ f
  | E.Apply_error c -> perror "cannot apply %s\n" (Creme.creme_to_string c); repl_ f

let _ =
  E.define_base ();
  try
    load (Lexing.from_channel (open_in "base.crm"))
  with L.Eof -> 
    repl_ (fun () -> repl "> " (Lexing.from_channel stdin))
