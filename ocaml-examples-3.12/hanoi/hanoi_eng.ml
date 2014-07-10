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

(* $Id: hanoi_eng.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* The Hanoi towers game solved by the computer. *)

let move start destination =
  print_endline ("I move a discus from " ^ start ^ " to " ^ destination)
;;

let rec hanoi height start temp destination =
  if height > 0 then
   begin
     hanoi (height - 1) start destination temp;
     move start destination;
     hanoi (height - 1) temp start destination
   end
;;

let game height = hanoi height "A" "B" "C";;

if !Sys.interactive then () else begin
 let l = Array.length Sys.argv in
 if l <= 1 then begin
   prerr_endline "Usage: hanoi <number of discuses>";
   exit 2 end;
 game (int_of_string (Sys.argv.(1)));
 exit 0
end
;;
