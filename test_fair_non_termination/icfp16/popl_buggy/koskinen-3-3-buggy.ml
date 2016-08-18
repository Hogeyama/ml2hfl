(* Taken from
   Koskinen and Terauchi, "Local Temporal Reasoning", CSL-LICS 2014, Figure 10 *)

let rec halt (): unit =
  event "Call";
  halt()

let rec bar x =
  event "Bar";
  (* if x>0 then bar (x-1) *)
  if x>0 then bar x (* Modified for fair infinite path *)
  else x

let rec foo x =
  event "Call";
(*  event "Foo"; *)
  if x<=0 then foo x
  else halt()

let main () =
  let t = read_int () in
  if read_int () >0 then foo 0
  else foo(bar t)

(* Property to be checked: event Bar occurs only finitely often *)
(* The original property described in their paper is FG(Foo | Step):
   this can be expressed as the conjunction of the following properties:
    1. "Foo" happens infinitely often
    2. "Halt" happens only finitely often
    3. "Bar" happens only finitely often
 *)
(*{SPEC}
   fairness: (Call, Bar)
{SPEC}*)
