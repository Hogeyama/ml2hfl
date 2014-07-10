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

(* $Id: deep_ast.mli,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;

type unop =
   | Uabs
   | Uand
   | Uacos | Uacosh
   | Uasin | Uasinh
   | Uatan | Uatanh
   | Uceil
   | Ucolon
   | Ucos | Ucosh
   | Uexp
   | Uexpm
   | Ufloor
   | Uint
   | Uinv
   | Ulog | Ulog10 | Ulogm
   | Uor
   | Umin | Umax
   | Uminus
   | Unot
   | Ureshape
   | Uround
   | Usign
   | Usin | Usinh
   | Usizeprod
   | Usizecol
   | Usizerow
   | Usum
   | Usumcol
   | Usumrow
   | Usvd
   | Utranspose
   | Uctranspose
   | Usqrt
   | Utan | Utanh
   | Ueye
;;

type binop =
   | Badd | Bsub | Bmul | Bdiv | Bexp | Bldiv
   | Beq | Blt | Bgt | Ble | Bge | Bne
   | Blor | Bland | Blne
   | Bmin | Bmax | Batan2
   | Bmulm | Bdivm | Bldivm | Bexpm
   | Bhconc | Bvconc
   | Bextract | Bextract_all_columns | Bextract_all_rows
   | Bremove | Bremove_all_columns | Bremove_all_rows
   | Brange
   | Bassign
   | Bones
   | Bzeros
;;

type ternop =
   | Textract
   | Trange
   | Tassign
   | Tassign_all_columns
   | Tassign_all_rows
   | Tmatrix
   | Tdotif
;;

type sci_op =
   | Dotswitch
;;

type quaternop =
   | Qassign
   | Qdotswitchcase
;;

type label = string;;

type expression =
   | D_colon
   | D_dollar
   | D_number of string
   | D_string of string
   | D_matrix of expression list list
   | D_variable of string
   | D_parameter of int
   | D_accu of expression
   | D_if of expression * expression * expression
   | D_switch of switch_desc
   | D_unary of unop * expression
   | D_binary of binop * expression * expression
   | D_ternary of ternop * expression * expression * expression
   | D_quaternary of
       quaternop * expression * expression * expression * expression
   | D_apply of expression * expression list
   | D_record_access of expression * label
   | D_bracket of expression
   | D_paren of expression

and switch_desc = {
  dswitch_subject : expression;
  dswitch_cases : clauses;
  dswitch_default : expression;
}

and clauses = clause list

and clause = pattern list * expression

and pattern = expression
;;
