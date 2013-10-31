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


let rec make_term (u:unit) =
  match Random.int 0 with
      0 ->
        let x = Random.int 0 in
          (function [] -> 0 | [1] -> x)
    | 1 ->
        let x = Random.int 0 in
          (function [] -> 1 | [1] -> x)
    | _ ->
        let t1 = make_term () in
        let t2 = make_term () in
          (function [] -> 2 | 1::ns -> t1 ns | 2::ns -> t2 ns)
    

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
      0 -> true
    | 1 -> false
    | _ ->
        let f1 ns = f (1::ns) in
        let f2 ns = f (2::ns) in
          groundTerm f1 && groundTerm f2

let main (m:unit) =
  let m = make_term () in
    if groundTerm m then sumConstants m else 0
