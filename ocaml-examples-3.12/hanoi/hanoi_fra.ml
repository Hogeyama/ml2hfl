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

(* $Id: hanoi_fra.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

(* Le jeu des Tour de Hanoi r�solu par l'ordinateur. *)

let d�place d�part destination =
  print_endline ("Je d�place un disque de " ^ d�part ^ " � " ^ destination)
;;

let rec hanoi hauteur d�part interm�diaire destination =
  if hauteur > 0 then
   begin
     hanoi (hauteur - 1) d�part destination interm�diaire;
     d�place d�part destination;
     hanoi (hauteur - 1) interm�diaire d�part destination
   end
;;

let jeu hauteur = hanoi hauteur "A" "B" "C";;

if !Sys.interactive then () else begin
 let l = Array.length Sys.argv in
 if l <= 1 then begin
   prerr_endline "Usage: hanoi <nombre de disques>";
   exit 2 end;
 jeu (int_of_string (Sys.argv.(1)));
 exit 0
end
;;
