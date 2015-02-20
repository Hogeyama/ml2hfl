let rec zip x y =
  if x = 0 then
    if y = 0 then
      x
    else
      assert false
  else
    if y = 0 then
      assert false
    else
      1 + zip (x - 1) (y - 1)
let rec map x =
  if x = 0 then x else 1 + map (x - 1)

(*{SPEC}
type zip : y:int -> {z:int | y = z} -> int
type map : x:int -> {y:int | y = x}
{SPEC}*)
