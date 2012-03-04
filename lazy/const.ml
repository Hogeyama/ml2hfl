open ExtList

(** Constants*)

type t =
  Event of Idnt.t
| Unit | True | False | Int of int | RandInt
| Not | Minus
| And | Or | Imply | Iff
| EqUnit | NeqUnit | EqBool | NeqBool
| EqInt | NeqInt | Lt | Gt | Leq | Geq
| IBTrue | IBFalse (* only used for cand and cor *)
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
let is_ibrel c =
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

let bnot_ibrel c =
  match c with
    EqInt -> NeqInt
  | NeqInt -> EqInt
  | Lt -> Geq
  | Gt -> Leq
  | Leq -> Gt
  | Geq -> Lt
  | _ -> let _ = Format.printf "%a" pr c in assert false

let minus_ibrel c =
  match c with
    EqInt -> EqInt
  | NeqInt -> NeqInt
  | Lt -> Gt
  | Gt -> Lt
  | Leq -> Geq
  | Geq -> Leq
  | _ -> let _ = Format.printf "%a" pr c in assert false

let lift_ibrel c =
  match c with
    EqInt -> (=)
  | NeqInt -> (<>)
  | Lt -> (<)
  | Gt -> (>)
  | Leq -> (<=)
  | Geq -> (>=)
(*
  | IBTrue -> fun _ _ -> true
  | IBFalse -> fun _ _ -> false
*)
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


let rec cand c1 c2 =
  match c1, c2 with
  | EqInt, EqInt -> EqInt
  | EqInt, NeqInt -> IBFalse
  | EqInt, Lt -> IBFalse
  | EqInt, Gt -> IBFalse
  | EqInt, Leq -> EqInt
  | EqInt, Geq -> EqInt

  | NeqInt, EqInt -> cand c2 c1
  | NeqInt, NeqInt -> NeqInt
  | NeqInt, Lt -> Lt
  | NeqInt, Gt -> Gt
  | NeqInt, Leq -> Lt
  | NeqInt, Geq -> Gt

  | Lt, EqInt -> cand c2 c1
  | Lt, NeqInt -> cand c2 c1
  | Lt, Lt -> Lt
  | Lt, Gt -> IBFalse
  | Lt, Leq -> Lt
  | Lt, Geq -> IBFalse

  | Gt, EqInt -> cand c2 c1
  | Gt, NeqInt -> cand c2 c1
  | Gt, Lt -> cand c2 c1
  | Gt, Gt -> Gt
  | Gt, Leq -> IBFalse
  | Gt, Geq -> Gt

  | Leq, EqInt -> cand c2 c1
  | Leq, NeqInt -> cand c2 c1
  | Leq, Lt -> cand c2 c1
  | Leq, Gt -> cand c2 c1
  | Leq, Leq -> Leq
  | Leq, Geq -> EqInt

  | Geq, EqInt -> cand c2 c1
  | Geq, NeqInt -> cand c2 c1
  | Geq, Lt -> cand c2 c1
  | Geq, Gt -> cand c2 c1
  | Geq, Leq -> cand c2 c1
  | Geq, Geq -> Geq

  | IBTrue, c | c, IBTrue -> c
  | IBFalse, _ | _, IBFalse -> IBFalse

  | _ -> assert false

let rec cor c1 c2 =
  match c1, c2 with
  | EqInt, EqInt -> EqInt
  | EqInt, NeqInt -> IBTrue
  | EqInt, Lt -> Leq
  | EqInt, Gt -> Geq
  | EqInt, Leq -> Leq
  | EqInt, Geq -> Geq

  | NeqInt, EqInt -> cor c2 c1
  | NeqInt, NeqInt -> NeqInt
  | NeqInt, Lt -> NeqInt
  | NeqInt, Gt -> NeqInt
  | NeqInt, Leq -> IBTrue
  | NeqInt, Geq -> IBTrue

  | Lt, EqInt -> cor c2 c1
  | Lt, NeqInt -> cor c2 c1
  | Lt, Lt -> Lt
  | Lt, Gt -> NeqInt
  | Lt, Leq -> Leq
  | Lt, Geq -> IBTrue

  | Gt, EqInt -> cor c2 c1
  | Gt, NeqInt -> cor c2 c1
  | Gt, Lt -> cor c2 c1
  | Gt, Gt -> Gt
  | Gt, Leq -> IBTrue
  | Gt, Geq -> Geq

  | Leq, EqInt -> cor c2 c1
  | Leq, NeqInt -> cor c2 c1
  | Leq, Lt -> cor c2 c1
  | Leq, Gt -> cor c2 c1
  | Leq, Leq -> Leq
  | Leq, Geq -> IBTrue

  | Geq, EqInt -> cor c2 c1
  | Geq, NeqInt -> cor c2 c1
  | Geq, Lt -> cor c2 c1
  | Geq, Gt -> cor c2 c1
  | Geq, Leq -> cor c2 c1
  | Geq, Geq -> Geq

  | IBTrue, _ | _, IBTrue -> IBTrue
  | IBFalse, c | c, IBFalse -> c

  | _ -> assert false


(** x c1 n1 and x c2 n2 *)
let rec candn (c1, n1) (c2, n2) =
  match c1, c2 with
  | EqInt, EqInt -> if n1 = n2 then [EqInt, n1] else [IBFalse, 0]
  | EqInt, NeqInt -> if n1 = n2 then [IBFalse, 0] else [EqInt, n1]
  | EqInt, Lt -> if n1 >= n2 then [IBFalse, 0] else [EqInt, n1]
  | EqInt, Gt -> if n1 <= n2 then [IBFalse, 0] else [EqInt, n1]
  | EqInt, Leq -> if n1 > n2 then [IBFalse, 0] else [EqInt, n1]
  | EqInt, Geq -> if n1 < n2 then [IBFalse, 0] else [EqInt, n1]

  | NeqInt, EqInt -> candn (c2, n2) (c1, n1)
  | NeqInt, NeqInt -> if n1 = n2 then [NeqInt, n1] else [NeqInt, n1; NeqInt, n2]
  | NeqInt, Lt -> if n1 >= n2 then [Lt, n2] else [NeqInt, n1; Lt, n2]
  | NeqInt, Gt -> if n1 <= n2 then [Gt, n2] else [NeqInt, n1; Gt, n2]
  | NeqInt, Leq -> if n1 = n2 then [Lt, n2] else if n1 > n2 then [Leq, n2] else [NeqInt, n1; Leq, n2]
  | NeqInt, Geq -> if n1 = n2 then [Gt, n2] else if n1 < n2 then [Geq, n2] else [NeqInt, n1; Geq, n2]

  | Lt, EqInt -> candn (c2, n2) (c1, n1)
  | Lt, NeqInt -> candn (c2, n2) (c1, n1)
  | Lt, Lt -> if n1 <= n2 then [Lt, n1] else [Lt, n2]
  | Lt, Gt -> if n1 = n2 + 2 then [EqInt, n2 + 1] else if n1 <= n2 + 1 then [IBFalse, 0] else [Lt, n1; Gt, n2]
  | Lt, Leq -> if n1 <= n2 then [Lt, n1] else [Leq, n2]
  | Lt, Geq -> if n1 = n2 + 1 then [EqInt, n2] else if n1 <= n2 then [IBFalse, 0] else [Lt, n1; Geq, n2]

  | Gt, EqInt -> candn (c2, n2) (c1, n1)
  | Gt, NeqInt -> candn (c2, n2) (c1, n1)
  | Gt, Lt -> candn (c2, n2) (c1, n1)
  | Gt, Gt -> if n1 >= n2 then [Gt, n1] else [Gt, n2]
  | Gt, Leq -> if n1 + 1 = n2 then [EqInt, n2] else if n1 >= n2 then [IBFalse, 0] else [Gt, n1; Leq, n2]
  | Gt, Geq -> if n1 >= n2 then [Gt, n1] else [Geq, n2]

  | Leq, EqInt -> candn (c2, n2) (c1, n1)
  | Leq, NeqInt -> candn (c2, n2) (c1, n1)
  | Leq, Lt -> candn (c2, n2) (c1, n1)
  | Leq, Gt -> candn (c2, n2) (c1, n1)
  | Leq, Leq -> if n1 <= n2 then [Leq, n1] else [Leq, n2]
  | Leq, Geq -> if n1 = n2 then [EqInt, n1] else if n1 < n2 then [IBFalse, 0] else [Leq, n1; Geq, n2]

  | Geq, EqInt -> candn (c2, n2) (c1, n1)
  | Geq, NeqInt -> candn (c2, n2) (c1, n1)
  | Geq, Lt -> candn (c2, n2) (c1, n1)
  | Geq, Gt -> candn (c2, n2) (c1, n1)
  | Geq, Leq -> candn (c2, n2) (c1, n1)
  | Geq, Geq -> if n1 >= n2 then [Geq, n1] else [Geq, n2]

  | IBTrue, _ -> [c2, n2]
  | _, IBTrue -> [c1, n1]
  | IBFalse, _ | _, IBFalse -> [IBFalse, 0]

  | _ -> assert false

let rec candns cns =
  match cns with
    [] -> assert false
  | [cn] -> [cn]
  | cn::cns' ->
      let cns'' = candns cns' in
      let cns''' = List.unique (Util.concat_map (candn cn) cns'') in
      if Util.set_equiv cns''' (cn::cns'') then
        cns'''
      else
        candns cns'''
