let rec fold_right (f:int->int->int) xs acc =
  match xs with
    [] -> acc
  | x::xs' -> f x (fold_right f xs' acc)

(*{SPEC}
type fold_right : ({x:int | x >= 0} -> y:int -> {r:int | r >= y}) -> {n:int | n >= 0} list -> z:int -> {r:int | r >= z}
{SPEC}*)
