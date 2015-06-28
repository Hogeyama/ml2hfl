(* CAV 2015 *)

let rec unfoldr_sum (f:int->bool*int*int) (seed:int) =
  let (isEnd, elem, next_seed) = f seed in
  if isEnd then 0
  else elem + unfoldr_sum f next_seed
let pred n = if n=0 then (false, 0, 0)
             else if n>0 then (true, 1, n-1)
             else let _ = event "A" in (true, 1, n-1)
let main r =
  unfoldr_sum pred r

(*{SPEC}
  fairness: (A, Never)
{SPEC}*)
