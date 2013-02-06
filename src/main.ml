module R = Reader
module L = Lexer
module E = Eval

let print_exn e =
  match e with
  | R.Read_error (l, c, s) -> Printf.printf "read error: %s on line %d, char %d\n" s l c
  | E.Creme_error s        -> Printf.printf "error: %s\n" s
  | x                      -> Printf.printf "error: unhandled exception %s\n" (Printexc.to_string x)

let rec repl prompt buf =
  print_string prompt; flush stdout;
  let tok = R.read buf in
  match tok with
  | Some t ->
      let evd = E.eval t in
      Creme.creme_print evd;
      repl prompt buf
  (* EOF *)
  | None -> ()

let load fname =
  let rec aux c =
    let tok = R.read c in
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
