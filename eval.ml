module C = Creme

(* Some useful constants *)
let t    = C.Boolean true
let f    = C.Boolean false
let zero = C.Number 0
let one  = C.Number 1

let rec creme_eval c =
  match c with
  | C.Quoted f         -> f
  | C.Pair (h, t) as p -> p
  | C.Vector a as v    -> v
  | C.Empty as e       -> e
  | atom               -> atom
