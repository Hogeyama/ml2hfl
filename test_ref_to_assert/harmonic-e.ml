let rec div x y =
  assert (y <> 0);
  if x < y
  then 0
  else 1 + div (x-y) y

let rec fold_left (f:int->int->int) acc xs =
  match xs with
      [] -> acc
    | x::xs' -> fold_left f (f acc x) xs'

(*{SPEC}
type div : int -> {y:int | y <> 0} -> int
type fold_left : (int -> {y:int | y <> 0} -> int) -> int -> {n:int | n >= 0} list -> int
{SPEC}*)
