let rec for_all f (xs:int list) =
  match xs with
      [] -> true
    | x::xs' ->
        f x && for_all f xs'

(*{SPEC}
type for_all : ({n:int | n >= 0} -> {r:bool | r}) -> {n:int | n >= 0} list -> {r:bool | r}
{SPEC}*)
