(*
type t = Var of int | Cst of int | App of t * t

let rec sumConstants = function
    Cst n -> n
  | Var _ -> assert false
  | App(t1,t2) -> sumConstants t1 + sumConstants t2

let rec groundTerm = function
    Cst n -> true
  | Var _ -> false
  | App(t1,t2) -> groundTerm t1 && groundTerm t2

let main m = if groundTerm m then sumConstants m else 0
*)

let rec make_term (u:unit) : (int list) -> int =
  match Random.int 0 with
      0 -> (function [] -> 0 | _ -> 0)
    | 1 -> (function [] -> 1 | _ -> 0)
    | _ ->
        let t1 = make_term () in
        let t2 = make_term () in
          (function [] -> 2 | 1::ns -> t1 ns | _::ns -> t2 ns)


let rec sumConstants f =
  match f [] with
      0 -> f [1]
    | 1 -> assert false
    | _ ->
        let f1 ns = f (1::ns) in
        let f2 ns = f (2::ns) in
          sumConstants f1 + sumConstants f2

let rec groundTerm f =
  match f [] with
      0 -> true, (function [] -> 0 | _ -> 0)
    | 1 -> false, f
    | _ ->
        let f1 ns = f (1::ns) in
        let f2 ns = f (2::ns) in
        let b1, f1' = groundTerm f1 in
        let b2, f2' = groundTerm f2 in
        let f' ns =
          match ns with
              [] -> 2
            | 1::ns' -> f1' ns'
            | _::ns' -> f2' ns'
        in
          b1 && b2, f'

let main (m:unit) =
  let m = make_term () in
  let b,m' = groundTerm m in
    if b then sumConstants m' else 0
