(** Constants*)

type t =
  Event of Idnt.t
| Unit
| True
| False
| And
| Or
| Imply
| Iff
| Not
| Lt
| Gt
| Leq
| Geq
| EqBool
| EqInt
| EqUnit
| NeqBool
| NeqInt
| NeqUnit
| Int of int
| RandInt
| Add
| Sub
| Mul
| Minus

(** @param c a constant
    @return whether c is a binary relation on integers *)
let rec is_ibin c =
  match c with
    Event(_)
  | Unit
  | True | False | And | Or | Imply | Iff | Not | EqBool | NeqBool | EqUnit | NeqUnit -> false
  | Lt | Gt | Leq | Geq | EqInt | NeqInt -> true
  | Int(_) | RandInt | Add | Sub | Mul | Minus -> false

(** @param c a constant
    @return whether c is a binary relation *)
let rec is_bin c =
  match c with
    Event(_)
  | Unit
  | True | False
  | Int(_) | RandInt -> false
  | Not
  | Minus -> false
  | And | Or | Imply | Iff
  | Lt | Gt | Leq | Geq | EqInt | NeqInt
  | EqBool | NeqBool | EqUnit | NeqUnit
  | Add | Sub | Mul -> true

let rec is_eq c =
  match c with
    Event(_)
  | Unit
  | True | False
  | Int(_) | RandInt
  | Not
  | Minus
  | And | Or | Imply | Iff
  | Lt | Gt | Leq | Geq -> false
  | EqInt | EqBool | EqUnit -> true
  | NeqInt | NeqBool | NeqUnit
  | Add | Sub | Mul -> false

let rec is_formula c =
  match c with
    Event(_)
  | Unit
  | Int(_) | RandInt
  | Minus
  | Add | Sub | Mul -> false
  | True | False | Not | And | Or | Imply | Iff
  | Lt | Gt | Leq | Geq
  | EqInt | EqBool | EqUnit
  | NeqInt | NeqBool | NeqUnit -> true

let rec pr ppf c =
  match c with
    Event(id) -> Format.fprintf ppf "%a" Idnt.pr id
  | Unit -> Format.fprintf ppf "()"
  | True -> Format.fprintf ppf "true"
  | False -> Format.fprintf ppf "false"
  | And -> Format.fprintf ppf "(&&)"
  | Or -> Format.fprintf ppf "(||)"
  | Imply -> Format.fprintf ppf "(=>)"
  | Iff -> Format.fprintf ppf "(<=>)"
  | Not -> Format.fprintf ppf "not"
  | Lt -> Format.fprintf ppf "(<)"
  | Gt -> Format.fprintf ppf "(>)"
  | Leq -> Format.fprintf ppf "(<=)"
  | Geq -> Format.fprintf ppf "(>=)"
  | EqUnit -> Format.fprintf ppf "(=u)"
  | EqBool -> Format.fprintf ppf "(=b)"
  | EqInt -> Format.fprintf ppf "(=i)"
(*
  | EqUnit | EqBool | EqInt -> Format.fprintf ppf "(=)"
*)
  | NeqBool | NeqInt | NeqUnit -> Format.fprintf ppf "(<>)"
  | Int(n) -> Format.fprintf ppf "%d" n
  | RandInt -> Format.fprintf ppf "rand_int"
  | Add -> Format.fprintf ppf "(+)"
  | Sub -> Format.fprintf ppf "(-)"
  | Mul -> Format.fprintf ppf "(*)"
  | Minus -> Format.fprintf ppf "-"

let bnot_ibin c =
  match c with
    Lt -> Geq
  | Gt -> Leq
  | Leq -> Gt
  | Geq -> Lt
  | EqInt -> NeqInt
  | NeqInt -> EqInt
  | _ -> let _ = Format.printf "%a" pr c in assert false

let rec pr_bin ppf c =
  match c with
    Event(_)
  | Unit
  | True
  | False
  | Int(_)
  | RandInt -> assert false
  | Not
  | Minus -> assert false
  | And -> Format.fprintf ppf "&&"
  | Or -> Format.fprintf ppf "||"
  | Imply -> Format.fprintf ppf "=>"
  | Iff -> Format.fprintf ppf "<=>"
  | Lt -> Format.fprintf ppf "<"
  | Gt -> Format.fprintf ppf ">"
  | Leq -> Format.fprintf ppf "<="
  | Geq -> Format.fprintf ppf ">="
  | EqUnit -> Format.fprintf ppf "=u"
  | EqBool -> Format.fprintf ppf "=b"
  | EqInt -> Format.fprintf ppf "=i"
(*
   | EqUnit| EqBool | EqInt -> Format.fprintf ppf "="
*)
  | NeqBool | NeqInt | NeqUnit -> Format.fprintf ppf "<>"
  | Add -> Format.fprintf ppf "+"
  | Sub -> Format.fprintf ppf "-"
  | Mul -> Format.fprintf ppf "*"
