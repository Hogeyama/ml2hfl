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

<<<<<<< HEAD
=======
<<<<<<< HEAD
let main_1017 () = append (fun b -> mynot b, 0)
=======
>>>>>>> master

(*{SPEC}
val f : (x:int[x > 0]) -> r:int
{SPEC}*)

let rec f n =
<<<<<<< HEAD
  if n = 0
  then 0
  else f (n-1)
let main n =
  let r = read_int () in
  f r
=======
  let r = read_int () in
  if n = 0
  then r
  else f (n-1)
let main n = f n
>>>>>>> 3e6c8cf... fix to distinguish two versions of rand_int
>>>>>>> master
