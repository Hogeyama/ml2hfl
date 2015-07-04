(* Taken from 
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let g i x = x-i

let app f x i =
   event "Call";
   event "P"; (* added to capture the fact that P happened before *)
   f x (g i)

let rec ha1 (x:int) =
   event "Call";
   event "P"; (* added to capture the fact that P happened before *)
(*   event "Ha1";*)
   event "Ha";
   ha1 x (* added to turn "Ha1 happens" to "Ha1 happens infinitely often" *)

let rec ha2 (x:int) =
   event "Call";
   event "P"; (* added to capture the fact that P happened before *)
(*   event "Ha2"; *)
   event "Ha";
   ha1 x (* added to turn "Ha1 happens" to "Ha1 happens infinitely often" *)
  
let rec walk x f =
(*  event "Walk";*)
  event "Call";
  event "P"; (* added to capture the fact that P happened before *)
  if x=0 then x
  else walk (f x) f

let rec run x f =
(*  event "Run";*)
  event "Call";
  event "P"; (* added to capture the fact that P happened before *)
  if x=0 then x
  else run (f(f x)) f

let rec life x =
  event "Call";
  if Random.int(0)>0 then
    (event "P"; 
     if x<0 then ha1 (app walk x 1)
     else ha2 (app run x 1))
  else life x

let main() = life (Random.int 0)
     
(* Property to be checked: event Foo occurs infinitely often *)
(* The original property described in their paper is
   G(P=>X (Walk U Ha1 \/ Run U Ha2))
   this can be expressed as the conjunction of the following properties:
    1. If P happens, then Ha1 or Ha2 happens eventually
    2. The next event after P is one of the events in {Ha1,Walk,Ha2,Run}
    3. After P and Walk happens, the next action is Walk or Ha1.
    4. After P and Run happens, the next action is Run or Ha2.
   The properties 2-4 are safety properties,
   which can be checked by plain MoCHi.
 *)
(*{SPEC}
   fairness: (Call, P)  (* either P does not happen, or *)
   fairness: (Ha, Never) (* Ha happens eventually *)
{SPEC}*)
