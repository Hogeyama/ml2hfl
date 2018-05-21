(* Contracts AST *)
type fname = string
type vpattern = Any | Var of string | Const of string
type epattern =
  | Call of fname * vpattern
  | Ret of fname * vpattern
type tcontract =
  | Atom of epattern 
  | Neg of epattern
  | Concat of tcontract * tcontract
  | Kleene of tcontract
  | Not of tcontract
  | Or of tcontract * tcontract
  | Wilds
type flat = FAny | FInt | FBool
type scontract =
  | SFlat of flat
  | SFun1 of fname * scontract * scontract
  | SFun2 of scontract * scontract
type scontracts = scontract list
(* S' where R *)
type hotcontract = scontracts * tcontract

(* Automata *)
(* Automata alphabets *)
type alpha = ACall of fname | ARet of fname | Eps
(* Automata states *)
type state = int * bool
