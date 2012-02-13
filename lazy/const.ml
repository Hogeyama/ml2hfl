(** Constants*)

type t =
  Event of Idnt.t
| Unit | True | False | Int of int | RandInt
| Not | Minus
| And | Or | Imply | Iff
| EqUnit | NeqUnit | EqBool | NeqBool
| EqInt | NeqInt | Lt | Gt | Leq | Geq
| Add | Sub | Mul
| Div | Mod

(** @param c a constant
    @return whether c is a binary relation *)
let rec is_bin c =
  match c with
    Event(_)
  | Unit | True | False | Int(_) | RandInt
  | Not | Minus -> false
  | And | Or | Imply | Iff
  | EqBool | NeqBool | EqUnit | NeqUnit
  | Lt | Gt | Leq | Geq | EqInt | NeqInt
  | Add | Sub | Mul -> true

(** @param c a constant
    @return whether c is a binary relation on integers *)
let rec is_ibin c =
  match c with
    Event(_)
  | Unit | True | False | Int(_) | RandInt
  | Not| Minus
  | And | Or | Imply | Iff 
  | EqBool | NeqBool | EqUnit | NeqUnit
  | Add | Sub | Mul -> false
  | EqInt | NeqInt | Lt | Gt | Leq | Geq -> true

(** @return whether c is an equality or a non-equality *)
let rec is_eq c =
  match c with
    Event(_)
  | Unit | True | False | Int(_) | RandInt
  | Not | Minus
  | And | Or | Imply | Iff
  | Lt | Gt | Leq | Geq
  | Add | Sub | Mul -> false
  | EqUnit | NeqUnit | EqBool | NeqBool | EqInt | NeqInt -> true

(** @return whether c is an integer expression *)
let rec is_iexp c =
  match c with
    Event(_)
  | Unit | True | False -> false
  | Int(_) | RandInt -> true
  | Not -> false
  | Minus -> true
  | And | Or | Imply | Iff
  | EqUnit | NeqUnit | EqBool | NeqBool
  | EqInt | NeqInt | Lt | Gt | Leq | Geq -> false
  | Add | Sub | Mul -> true

let rec is_formula c =
  match c with
    Event(_)
  | Unit
  | Int(_) | RandInt
  | Minus
  | Add | Sub | Mul -> false
  | True | False | Not | And | Or | Imply | Iff
  | Lt | Gt | Leq | Geq
  | EqUnit | NeqUnit | EqBool | NeqBool | EqInt | NeqInt -> true

let rec pr ppf c =
  match c with
    Event(id) -> Format.fprintf ppf "%a" Idnt.pr id
  | Unit -> Format.fprintf ppf "()"
  | True -> Format.fprintf ppf "true"
  | False -> Format.fprintf ppf "false"
  | Int(n) -> Format.fprintf ppf "%d" n
  | RandInt -> Format.fprintf ppf "rand_int"
  | Not -> Format.fprintf ppf "not"
  | Minus -> Format.fprintf ppf "-"
  | And -> Format.fprintf ppf "(&&)"
  | Or -> Format.fprintf ppf "(||)"
  | Imply -> Format.fprintf ppf "(=>)"
  | Iff -> Format.fprintf ppf "(<=>)"
  | EqUnit -> Format.fprintf ppf "(=u)"
  | EqBool -> Format.fprintf ppf "(=b)"
  | EqInt -> Format.fprintf ppf "(=i)"
(*
  | EqUnit | EqBool | EqInt -> Format.fprintf ppf "(=)"
*)
  | Lt -> Format.fprintf ppf "(<)"
  | Gt -> Format.fprintf ppf "(>)"
  | Leq -> Format.fprintf ppf "(<=)"
  | Geq -> Format.fprintf ppf "(>=)"
  | NeqUnit | NeqBool | NeqInt -> Format.fprintf ppf "(<>)"
  | Add -> Format.fprintf ppf "(+)"
  | Sub -> Format.fprintf ppf "(-)"
  | Mul -> Format.fprintf ppf "(*)"

let bnot_ibin c =
  match c with
    EqInt -> NeqInt
  | NeqInt -> EqInt
  | Lt -> Geq
  | Gt -> Leq
  | Leq -> Gt
  | Geq -> Lt
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
  | EqUnit -> Format.fprintf ppf "=u"
  | EqBool -> Format.fprintf ppf "=b"
  | EqInt -> Format.fprintf ppf "=i"
(*
   | EqUnit| EqBool | EqInt -> Format.fprintf ppf "="
*)
  | Lt -> Format.fprintf ppf "<"
  | Gt -> Format.fprintf ppf ">"
  | Leq -> Format.fprintf ppf "<="
  | Geq -> Format.fprintf ppf ">="
  | NeqUnit | NeqBool | NeqInt -> Format.fprintf ppf "<>"
  | Add -> Format.fprintf ppf "+"
  | Sub -> Format.fprintf ppf "-"
  | Mul -> Format.fprintf ppf "*"
