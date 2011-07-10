type t =
    Fail
  | Event of string
  | Label of int
  | Unit
  | True
  | False
  | Unknown
  | And
  | Or
  | Not
  | Lt
  | Gt
  | Leq
  | Geq
  | Eq
  | Int of int
  | Add
  | Sub
  | Mul
  | Tuple of int
  | Proj of int (* 0-origin *)
  | If (* for temporary use *)


let print fm = function
    Fail -> Format.fprintf fm "fail"
  | Event s -> Format.fprintf fm "event(%s)" s
  | Label n -> Format.fprintf fm "label(%d)" n
  | Unit -> Format.fprintf fm "()"
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | And -> Format.fprintf fm "&&"
  | Or -> Format.fprintf fm "||"
  | Not -> Format.fprintf fm "not"
  | Lt -> Format.fprintf fm "<"
  | Gt -> Format.fprintf fm ">"
  | Leq -> Format.fprintf fm "<="
  | Geq -> Format.fprintf fm ">="
  | Eq -> Format.fprintf fm "="
  | Int n -> Format.fprintf fm "%d" n
  | Add -> Format.fprintf fm "+"
  | Sub -> Format.fprintf fm "-"
  | Mul -> Format.fprintf fm "*"

let is_fun = function
    Fail -> false
  | Event _ -> false
  | Label _ -> true
  | Unit _ -> false
  | True _ -> false
  | False _ -> false
  | Unknown _ -> false
  | And -> true
  | Or -> true
  | Not -> true
  | Lt -> true
  | Gt -> true
  | Leq -> true
  | Geq -> true
  | Eq -> true
  | Int _ -> false
  | Add -> true
  | Sub -> true
  | Mul -> true
  | Tuple _ -> true
  | Proj _ -> true
  | If _ -> true



