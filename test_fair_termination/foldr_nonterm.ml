(* CAV 2015 *)

let rec foldr (f:int->int->int) (acc:int) (xs:int) =
  if xs = 0 then acc
  else
    let elem = Random.int 0 in
    f elem (foldr f acc (xs - 1))
let rec loop (u:unit) = let _ = event "A" in (loop u: int)
let rec sum_may_nonterm x y =
  let isTerminate = Random.int 0 in
  if isTerminate > 0 then x+y else loop ()
let main () = 
  let xs = Random.int 0 in
  if xs > 0 then
    foldr sum_may_nonterm 0 xs
  else -1

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
