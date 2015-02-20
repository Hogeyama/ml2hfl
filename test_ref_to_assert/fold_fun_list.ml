let rec fold_right (f:(int->int)->(int->int)->(int->int)) xs (init:int->int) =
  match xs with
      [] -> init
    | x::xs' -> f x (fold_right f xs' init)

(*{SPEC}
type fold_right :
 ((x:int -> {r:int | r >= x}) -> (x:int -> {r:int | r >= x}) -> (x:int -> {r:int | r >= x})) ->
 (x:int -> {r:int | r >= x}) list ->
 (x:int -> {r:int | r >= x}) ->
 (x:int -> {r:int | r >= x})
{SPEC}*)
