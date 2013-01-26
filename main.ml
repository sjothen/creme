module P = Parser
module L = Lexer

let rec repl prompt chan =
  let buf = Lexing.from_channel chan in
  try
    while true do
      print_string prompt; flush stdout;
      let tok = P.main L.token buf in
      Creme.print_creme (Eval.creme_eval tok)
    done
  with End_of_file ->
    exit 0

let _ =
  repl "> " stdin
