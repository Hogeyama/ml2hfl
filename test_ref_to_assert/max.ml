let max max2 (x:int) (y:int) (z:int) : int = max2 (max2 x y) z

(*{SPEC}
type max :
 (x:int -> y:int -> {r:int | r >= x && r >= y}) ->
 x:int -> y:int -> z:int -> {r:int | r >= x && r >= y && r >= z}
{SPEC}*)
