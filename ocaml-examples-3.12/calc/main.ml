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

(* $Id: main.ml,v 1.3 2011-08-08 19:31:17 weis Exp $ *)

let calc () =
  try
    while true do
      let result = Calc.parse () in
      print_int result; print_newline (); flush stdout
    done
  with _ -> ()
;;

if !Sys.interactive then () else begin
  calc ();
  exit 0
end
;;
