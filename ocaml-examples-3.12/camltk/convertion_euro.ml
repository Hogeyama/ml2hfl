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

(* $Id: convertion_euro.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Camltk;;

let synchronise_zones source dest taux_source taux_dest =
  function infos ->
    try
      let montant_source = float_of_string (Entry.get source) in
      let montant_dest =
        montant_source *. !taux_source /. !taux_dest in
      Entry.delete_range dest (At 0) End;
      Entry.insert dest (At 0)
                         (Printf.sprintf "%.2f" montant_dest)
    with Failure _ ->
      Entry.delete_range dest (At 0) End;
      Entry.insert dest (At 0) "erreur"
;;

let convertion_en_francs () =
  let fp = openTk () in

  let ligne1 = Frame.create fp []
  and ligne2 = Frame.create fp [] in

  let étiq1 = Label.create ligne1 [Text "Francs:"]
  and entrée1 = Entry.create ligne1 [TextWidth 10; Relief Sunken]

  and étiq2 = Label.create ligne2 [Text "Euros:"]
  and entrée2 = Entry.create ligne2 [TextWidth 10; Relief Sunken] in

  let quit = Button.create fp [Text "Quit"; Command closeTk] in

  let taux1 = ref 1.0     (* francs pour 1 franc *)
  and taux2 = ref 6.55957 (* francs pour 1 euro *) in

  bind entrée1 [[], KeyRelease]
       (BindSet([], synchronise_zones entrée1 entrée2 taux1 taux2));
  bind entrée2 [[], KeyRelease]
       (BindSet([], synchronise_zones entrée2 entrée1 taux2 taux1));

  pack [étiq1] [Side Side_Left]; pack [entrée1] [Side Side_Right];
  pack [étiq2] [Side Side_Left]; pack [entrée2] [Side Side_Right];
  pack [ligne1; ligne2] [Side Side_Top; Fill Fill_X];
  pack [quit] [Side Side_Bottom; Fill Fill_X];
  mainLoop ()
;;

if !Sys.interactive then () else begin convertion_en_francs (); exit 0 end;;
