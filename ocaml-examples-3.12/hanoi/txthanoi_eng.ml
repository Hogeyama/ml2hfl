(***********************************************************************)
(*                                                                     *)
(*                        Caml examples                                *)
(*                                                                     *)
(*            Pierre Weis                                              *)
(*                                                                     *)
(*                        INRIA Rocquencourt                           *)
(*                                                                     *)
(*  Copyright (c) 1994-2011, INRIA                                     *)
(*  All rights reserved.                                               *)
(*                                                                     *)
(*  Distributed under the BSD license.                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: txthanoi_eng.ml,v 1.5 2011-08-08 19:31:17 weis Exp $ *)

(* The Hanoi game animated in text mode. *)

let spaces n = String.make n ' ';;

let discus size =
  let half_right = String.make size '>'
  and half_left = String.make size '<' in
  half_left ^ "|" ^ half_right
;;

let discus_number n big_discus_size =
  let white_part = spaces (big_discus_size + 1 - n) in
  white_part ^ discus n ^ white_part
;;

let pin_basis big_discus_size =
  let half = String.make big_discus_size '_' in
  " " ^ half ^ "|" ^ half ^ " "
;;

let rec pin big_discus_size = function
  | (0, []) -> []
  | (0, head :: rest) ->
      discus_number head big_discus_size ::
      pin big_discus_size (0, rest)
  | (shift, list) ->
      discus_number 0 big_discus_size ::
      pin big_discus_size (shift - 1, list)
;;

let rec past l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> []
  | (t1 :: r1, t2 :: r2, t3 :: r3) -> (t1 ^ t2 ^ t3) :: past r1 r2 r3
  | _ -> failwith "past"
;;

let print line = print_string line; print_newline ();;

let print_game discus_number start midle destination =
  let drawing =
      past (pin discus_number start)
           (pin discus_number midle)
           (pin discus_number destination) in
  List.iter print drawing;
  let b = pin_basis discus_number in
  print (b ^ b ^ b)
;;

let add_discus discus (shift, discuss) =
    (shift - 1, discus :: discuss)
;;

let top = function
  | (_, top :: _rest) -> top
  | (_, []) -> failwith "top: empty pin"
;;

let pop_top = function
  | (shift, _top :: rest) -> (shift + 1, rest)
  | (_, []) -> failwith "pop_top: empty pin"
;;

let move (start_name, pin_start) (destination_name, destination_pin) =
  print ("I move a discus from " ^ start_name ^ " to " ^ destination_name);
  let moving_discus = top !pin_start in
  pin_start := pop_top !pin_start;
  destination_pin := add_discus moving_discus !destination_pin
;;

let empty_pin discus_number = (discus_number, []);;

let full_pin discus_number =
  let rec discuses i =
      if i <= discus_number then i :: discuses (i + 1) else [] in
  (0, discuses 1)
;;

let wait () =
  print_string "Press return to continue"; print_newline ();
  ignore (read_line ())
;;

let game discus_number =
  let left = ref (full_pin discus_number)
  and midle = ref (empty_pin discus_number)
  and right = ref (empty_pin discus_number) in
  let rec hanoi height start temp destination =
      if height > 0 then
       begin
         hanoi (height - 1) start destination temp;
         wait ();
         move start destination;
         print_game discus_number !left !midle !right;
         hanoi (height - 1) temp start destination
       end in
  print "I name the pins A, B, and C.";
  print "Starting position is";
  print_game discus_number !left !midle !right;
  hanoi discus_number
        ("A", left) ("B", midle) ("C", right)
;;

if !Sys.interactive then () else begin
  let usage () =
    prerr_endline "Usage: hanoi <number of discuses>";
    exit 2 in
  let l = Array.length Sys.argv in
  if l <= 1 then usage () else
  let n = int_of_string (Sys.argv.(1)) in
  if n <= 0 then usage () else
  game n;
  wait ();
  exit 0
end
;;
