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

(* $Id: stockage.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Code;;

exception Erreur of string;;

type �tat_de_l'assembleur =
   { mutable pc : int;
     mutable code : instruction array;
     mutable table_�tiq : (string, int) Hashtbl.t;
     mutable �_r�soudre : (int * string) list }
;;

let asm =
  { pc = 0; code = [||];
    table_�tiq = Hashtbl.create 1;
    �_r�soudre = [] }
;;

let initialise () =
  asm.pc <- 0;
  asm.code <- Array.make 100 Stop;
  asm.table_�tiq <- Hashtbl.create 17;
  asm.�_r�soudre <- []
;;

let d�code_adresse adr = adr / taille_du_mot;;

let assemble instruction =
  if asm.pc >= Array.length asm.code then begin
    let nouveau_code = Array.make (2 * Array.length asm.code) Stop in
    Array.blit asm.code 0 nouveau_code 0 (Array.length asm.code);
    asm.code <- nouveau_code
  end;
  asm.code.(d�code_adresse asm.pc) <- instruction;
  asm.pc <- asm.pc + taille_du_mot
;;

let d�finir_�tiquette nom_�tiq val_�tiq =
  if Hashtbl.mem asm.table_�tiq nom_�tiq then
    raise (Erreur ("�tiquette " ^ nom_�tiq ^ " red�finie"))
  else Hashtbl.add asm.table_�tiq nom_�tiq val_�tiq
;;

let poser_�tiquette nom_�tiq =
    d�finir_�tiquette nom_�tiq asm.pc
;;

let valeur_�tiquette nom_�tiq =
    try
       Hashtbl.find asm.table_�tiq nom_�tiq
    with Not_found ->
       asm.�_r�soudre <- (asm.pc, nom_�tiq) :: asm.�_r�soudre;
       0
;;

let r�soudre_�tiquette (adresse, nom_�tiq) =
  let valeur =
      try
        Hashtbl.find asm.table_�tiq nom_�tiq
      with Not_found ->
        raise (Erreur ("�tiquette " ^ nom_�tiq ^ " ind�finie")) in
  let nouvelle_instruction =
      match asm.code.(d�code_adresse adresse) with
      | Op(op�ration, reg1, _, reg2) ->
          Op(op�ration, reg1, Imm valeur, reg2)
      | Jmp(_, reg) ->
          Jmp(Imm valeur, reg)
      | Braz(reg, _) ->
          Braz(reg, valeur)
      | Branz(reg, _) ->
          Branz(reg, valeur)
      | Scall _ | Stop -> raise (Erreur "r�soudre_�tiquette") in
  asm.code.(d�code_adresse adresse) <- nouvelle_instruction
;;

let extraire_code () =
  List.iter r�soudre_�tiquette asm.�_r�soudre;
  Array.sub asm.code 0 (d�code_adresse asm.pc)
;;
