module P = Parser
module L = Lexer
module E = Eval

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = P.main L.token buf in
  let evd = E.eval tok in
  Creme.print_creme evd;
  repl prompt buf

let perror = Printf.printf

let rec repl_ f =
  try
    f ()
  with
  | End_of_file -> exit 0
  | E.Undefined_symbol s -> perror "undefined symbol %s\n" s; repl_ f

let _ =
  E.def_primitives ();
  repl_ (fun () -> repl "> " (Lexing.from_channel stdin))
