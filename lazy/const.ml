type t =
  Event of Id.t
| Unit
| True
| False
| And
| Or
| Imply
| Not
| Lt
| Gt
| Leq
| Geq
| Eq
| Neq
| Int of int
| Add
| Sub
| Mul
| Minus

let rec is_int_rel c =
  match c with
    Event(_)
  | Unit
  | True | False | And | Or | Imply | Not -> false
  | Lt | Gt | Leq | Geq | Eq | Neq -> true
  | Int(_) | Add | Sub | Mul | Minus -> false

let rec is_binary c =
  match c with
    Event(_)
  | Unit
  | True | False
  | Int(_) -> false
  | Not
  | Minus -> false
  | And | Or | Imply
  | Lt | Gt | Leq | Geq | Eq | Neq
  | Add | Sub | Mul -> true

let rec pr ppf c =
  match c with
    Event(id) -> Format.fprintf ppf "%a" Id.pr id
  | Unit -> Format.fprintf ppf "()"
  | True -> Format.fprintf ppf "true"
  | False -> Format.fprintf ppf "false"
  | And -> Format.fprintf ppf "(&&)"
  | Or -> Format.fprintf ppf "(||)"
  | Imply -> Format.fprintf ppf "(=>)"
  | Not -> Format.fprintf ppf "not"
  | Lt -> Format.fprintf ppf "(<)"
  | Gt -> Format.fprintf ppf "(>)"
  | Leq -> Format.fprintf ppf "(<=)"
  | Geq -> Format.fprintf ppf "(>=)"
  | Eq -> Format.fprintf ppf "(=)"
  | Neq -> Format.fprintf ppf "(<>)"
  | Int(n) -> Format.fprintf ppf "%d" n
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
  | Eq -> Neq
  | Neq -> Eq

let rec pr_bin ppf c =
  match c with
    Event(_)
  | Unit
  | True
  | False
  | Int(_) -> assert false
  | Not
  | Minus -> assert false
  | And -> Format.fprintf ppf "&&"
  | Or -> Format.fprintf ppf "||"
  | Imply -> Format.fprintf ppf "=>"
  | Lt -> Format.fprintf ppf "<"
  | Gt -> Format.fprintf ppf ">"
  | Leq -> Format.fprintf ppf "<="
  | Geq -> Format.fprintf ppf ">="
  | Eq -> Format.fprintf ppf "="
  | Neq -> Format.fprintf ppf "<>"
  | Add -> Format.fprintf ppf "+"
  | Sub -> Format.fprintf ppf "-"
  | Mul -> Format.fprintf ppf "*"
