let rec for_all f (xs:(int*int) list) =
  match xs with
      [] -> true
    | x::xs' ->
        f x && for_all f xs'

(*{SPEC}
type for_all : ((n:int * {m:int | m = n}) -> {r:bool | r}) -> (n:int * {m:int | m = n}) list -> {r:bool | r}
{SPEC}*)
