(*
USED: PLDI2011 as sum
USED: PEPM2013 as sum
*)
(* {SPEC}
val sum : (x:int) -> r:int[r >= x]
{SPEC} *)
(*

let rec sum n =
  if n <= 0
  then 0
  else n + sum (n-1)

let main n =
  assert (n <= sum n)
 *)


(*{SPEC}
val f : (x:int[x > 0]) -> r:int
{SPEC}*)

let rec f n =
  if n = 0
  then 0
  else f (n-1)
let main n =
  let r = read_int () in
  f r
