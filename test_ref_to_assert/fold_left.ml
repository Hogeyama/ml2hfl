let rec fold_left (f:int->int->int) acc xs =
  match xs with
      [] -> acc
    | x::xs' -> fold_left f (f acc x) xs'

(*{SPEC}
type fold_left : (x:int -> {y:int | y >= 0} -> {r:int | r >= x}) -> z:int -> {n:int | n >= 0} list -> {r:int | r >= z}
{SPEC}*)
