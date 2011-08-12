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

let rec is_int_rel c =
  match c with
    Event(_)
  | Unit
  | True | False | And | Or | Imply | Iff | Not | EqBool | NeqBool | EqUnit | NeqUnit -> false
  | Lt | Gt | Leq | Geq | EqInt | NeqInt -> true
  | Int(_) | RandInt | Add | Sub | Mul | Minus -> false

let rec is_binary c =
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
  | EqBool | EqInt | EqUnit -> Format.fprintf ppf "(=)"
  | NeqBool | NeqInt | NeqUnit -> Format.fprintf ppf "(<>)"
  | Int(n) -> Format.fprintf ppf "%d" n
  | RandInt -> Format.fprintf ppf "rand_int()"
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
  | _ -> assert false

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
  | EqBool | EqInt | EqUnit -> Format.fprintf ppf "="
  | NeqBool | NeqInt | NeqUnit -> Format.fprintf ppf "<>"
  | Add -> Format.fprintf ppf "+"
  | Sub -> Format.fprintf ppf "-"
  | Mul -> Format.fprintf ppf "*"
