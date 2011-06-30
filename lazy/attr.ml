type s =
  Arity of int
| Arg of int
| Path of int list
type t = s list

let arity attr =
  let Arity(ar) = List.find (function Arity(_) -> true | _ -> false ) attr in
  ar

let arg attr =
  let Arg(arg) = List.find (function Arg(_) -> true | _ -> false ) attr in
  arg

let path attr =
  let Path(p) = List.find (function Path(_) -> true | _ -> false ) attr in
  p
