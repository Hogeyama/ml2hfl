type s = Arity of int
type t = s list

(*
let arity attr =
  let Arity(ar) = List.find (function Arity(_) -> true | _ -> false ) attr in
  ar
*)
