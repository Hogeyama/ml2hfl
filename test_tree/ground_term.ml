
type t = Var of int | Cst of int | App of t * t

let rec makeTerm () =
  match Random.int 3 with
      0 -> Cst (Random.int 0)
    | 1 -> Var (Random.int 0)
    | _ -> App(makeTerm (), makeTerm ())

let rec sumConstants = function
    Cst n -> n
  | Var _ -> assert false
  | App(t1,t2) -> sumConstants t1 + sumConstants t2

let rec groundTerm = function
    Cst n -> true
  | Var _ -> false
  | App(t1,t2) -> groundTerm t1 && groundTerm t2

let main n =
  let m = makeTerm () in
    if groundTerm m then sumConstants m else 0
