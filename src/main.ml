module P = Parser
module L = Lexer
module E = Eval

exception Parse_error of Lexing.position * int * int * string

let print_exn e =
  match e with
  | Parse_error (p, l, c, s) -> Printf.printf "error: syntax error\n"
  | E.Creme_error s          -> Printf.printf "error: %s\n" s
  | x                        -> Printf.printf "error: unhandled exception %s\n" (Printexc.to_string x)

let parse buf =
  try
    P.main L.token buf
  with ex ->
    let curr = buf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme buf in
    raise (Parse_error (curr, line, cnum, tok))

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = parse buf in
  match tok with
  | Some t ->
      let evd = E.eval t in
      Creme.creme_print evd;
      repl prompt buf
  (* EOF *)
  | None -> ()

let load fname =
  let rec aux c =
    let tok = parse c in
    match tok with
    | Some t -> ignore (E.eval t); aux c
    (* EOF *)
    | None -> ()
  in
  try
    aux (Lexing.from_channel (open_in fname))
  with x -> print_exn x

let rec repl_ f =
  try f()
  with x -> print_exn x; repl_ f

let _ =
  E.define_base ();
  load "boot/base.crm";
  if Array.length Sys.argv > 1 then
    load Sys.argv.(1)
  else
    repl_ (fun () -> repl "> " (Lexing.from_channel stdin))
