(* Taken from
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let rec print (x:int) :unit = event "Print";print x
and rumble x y =
(*  event "Rumble"; *)
  if x<y then
    if read_int () >0 then
       rumble (x+1) y
  (*else rumble x (y+1)*)
    else rumble x y
  else x

let main() =
  let a = read_int () in
  let b = read_int () in
    print (rumble a (rumble a b))

(* Property to be checked: event Print happens eventually in any infinite computation *)
(* The definition of "print" above has been modified from the original one
   to turn "Print eventually happens" into "Print happens infinitely often" *)
(*{SPEC}
   fairness: (Print, Never)
{SPEC}*)
