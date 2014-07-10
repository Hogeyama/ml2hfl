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

(* $Id: transl.ml,v 1.1 2011-08-08 19:09:58 weis Exp $ *)

open Ast;;
open Deep_ast;;

let transl_error s =
  Printf.eprintf "transl: fatal error %s\n" s;
  failwith "transl"
;;

let unop_list = [
  "abs", Uabs;
  "and", Uand;
  "acos", Uacos; "acosh", Uacosh;
  "asin", Uasin; "asinh", Uasinh;
  "atan", Uatan; "atanh", Uatanh;
  "ceil", Uceil;
  "cos", Ucos; "cosh", Ucosh;
  "exp", Uexp; "expm", Uexpm; "eye", Ueye;
  "fabs", Uabs;
  "floor", Ufloor;
  "int", Uint;
  "inv", Uinv;
  "ln", Ulog;
  "log", Ulog; "logm", Ulogm; "log10", Ulog10;
  "min", Umin;
  "max", Umax;
  "-", Uminus;
  "~", Unot;
  "or", Uor;
  "reshape", Ureshape;
  "round", Uround;
  "sgn", Usign;
  "sin", Usin; "sinh", Usinh;
  "size_prod", Usizeprod;
  "size_row", Usizerow;
  "size_col", Usizecol;
  "sum", Usum;
  "sum_row", Usumrow;
  "sum_col", Usumcol;
  "sqrt", Usqrt;
  "svd", Usvd;
  "tan", Utan; "tanh", Utanh;
  "\'", Utranspose;
  ".\'", Uctranspose;
  ":", Ucolon;
]
;;

let unop_table, unop_inv_table =
  let t = Hashtbl.create 31
  and xt = Hashtbl.create 31 in
  List.iter
    (fun (s, op) ->
     Hashtbl.add t s op;
     Hashtbl.add xt op s)
    unop_list;
  t, xt
;;

let unop_of_string s =
  try Hashtbl.find unop_table s
  with
  | Not_found -> assert false
;;

let string_of_unop = function
  | Utranspose -> "transpose"
  | Uctranspose -> "conj_transpose"
  | Ucolon -> "column_wise"
  | Uminus -> "negate"
  | Umin -> "min_mat"
  | Umax -> "max_mat"
  | Ueye -> "eye"
  | op ->
    try Hashtbl.find unop_inv_table op
    with
    | Not_found -> assert false
;;

let binop_list = [
  "+", Badd; "-", Bsub; "*", Bmul; "/", Bdiv;
  "**", Bexp; "^", Bexp;
  "\\", Bldiv;
  ":=", Bassign; "==", Beq; "<", Blt; ">", Bgt; "<=", Ble; ">=", Bge;
  "~=", Bne; "<>", Bne;
  "|", Blor;
  "&", Bland; "!=", Blne;
  "min", Bmin; "max", Bmax;
  "atan2", Batan2;
  ".*", Bmulm; "./", Bdivm;
  ".\\", Bldivm; ".^", Bexpm;
  ",", Bhconc; ";", Bvconc;
  "unary_extract", Bextract;
  "extract_all_columns", Bextract_all_columns;
  "extract_all_rows", Bextract_all_rows;
  "unary_remove", Bremove;
  "remove_all_columns", Bremove_all_columns;
  "remove_all_rows", Bremove_all_rows;
  ":", Brange;
  "ones", Bones;
  "zeros", Bzeros;
]
;;

let sci_op_list = [
  "dot_switch", Dotswitch;
]
;;

let is_prefix_binop = function
  | Bmin | Bmax | Batan2
  | Bextract | Bextract_all_columns | Bextract_all_rows
  | Bremove | Bremove_all_columns | Bremove_all_rows
  | Bones | Bzeros -> true
  | Bvconc | Bhconc | Brange | Bassign
  | Bexpm | Bldivm | Bdivm | Bmulm
  | Blne | Bland | Blor
  | Bne | Bge | Ble | Bgt | Blt | Beq
  | Bldiv | Bexp | Bdiv | Bmul | Bsub | Badd -> false
;;

let binop_table, binop_inv_table =
  let t = Hashtbl.create 31
  and xt = Hashtbl.create 31 in
  List.iter
    (fun (s, op) ->
       Hashtbl.add t s op;
       Hashtbl.add xt op s)
    binop_list;
  t, xt
;;

let binop_of_string s =
  try Hashtbl.find binop_table s with
  | Not_found -> assert false
;;

let string_of_binop op =
  try Hashtbl.find binop_inv_table op with
  | Not_found -> assert false
;;

let ternop_list = [
  "binary_extract", Textract;
  "step_range", Trange;
  "binary_assign", Tassign;
  "assign_all_columns", Tassign_all_columns;
  "assign_all_rows", Tassign_all_rows;
  "dot_if", Tdotif;
  "matrix", Tmatrix;
]
;;

let ternop_table, ternop_inv_table =
  let t = Hashtbl.create 31
  and xt = Hashtbl.create 31 in
  List.iter
    (fun (s, op) ->
       Hashtbl.add t s op;
       Hashtbl.add xt op s)
    ternop_list;
  t, xt
;;

let ternop_of_string s =
  try Hashtbl.find ternop_table s with
  | Not_found -> assert false
;;

let string_of_ternop op =
  try Hashtbl.find ternop_inv_table op with
  | Not_found -> assert false
;;

let quaternop_list = [
  "ternary_assign", Qassign;
  "dot_switch", Qdotswitchcase;
]
;;

let quaternop_table, quaternop_inv_table =
  let t = Hashtbl.create 31
  and xt = Hashtbl.create 31 in
  List.iter
    (fun (s, op) ->
       Hashtbl.add t s op;
       Hashtbl.add xt op s)
    quaternop_list;
  t, xt
;;

let quaternop_of_string s =
  try Hashtbl.find quaternop_table s with
  | Not_found -> assert false
;;

let string_of_quaternop op =
  try Hashtbl.find quaternop_inv_table op
  with
  | Not_found -> assert false
;;

let sci_op_table, sci_op_inv_table =
  let t = Hashtbl.create 31
  and xt = Hashtbl.create 31 in
  List.iter
    (fun (s, op) ->
       Hashtbl.add t s op;
       Hashtbl.add xt op s)
    sci_op_list;
  t, xt
;;

let sci_op_of_string s =
  try Hashtbl.find sci_op_table s with
  | Not_found -> assert false
;;

let string_of_sci_op op =
  try Hashtbl.find sci_op_inv_table op with
  | Not_found -> assert false
;;

let transl_prefix_op op =
  let op =
    match op with
    | "!" -> "~"
    | op -> op in
  unop_of_string op
;;

let transl_infix_op op =
  let op =
    match op with
    | "&&" -> "&"
    | "||" -> "|"
    | "!=" -> "~="
    | op -> op in
  binop_of_string op
;;

let mk_uminus n =
  let len = String.length n in
  assert (len > 0);
  match n.[0] with
  | '-' -> Printf.sprintf "%s" (String.sub n 1 (len - 1))
  | '+' -> Printf.sprintf "-%s" (String.sub n 1 (len - 1))
  | _ -> Printf.sprintf "-%s" n
;;

let make_paren exp =
  match exp with
  | D_dollar -> exp
  | D_colon -> exp
  | D_number s ->
    if String.length s = 0 then assert false else
    if s.[0] = '-' then D_paren exp else exp
  | D_string _ -> exp
  | D_matrix _ -> exp
  | D_variable _ -> exp
  | D_parameter _ -> exp
  | D_accu _ -> exp
  | D_if (_, _, _) | D_switch _ -> exp
  | D_unary (_, _)
  | D_binary (_, _, _)
  | D_ternary (_, _, _, _)
  | D_quaternary (_, _, _, _, _)
  | D_apply (_, _) -> D_paren exp
  | D_record_access (_, _) -> exp
  | D_bracket _ -> exp
  | D_paren _ -> exp
;;

let ( + ) e1 e2 = D_binary (Badd, e1, e2)
and ( - ) e1 e2 = D_binary (Bsub, e1, e2)
and ( * ) e1 e2 = D_binary (Bmulm, e1, e2)
and ( / ) e1 e2 = D_binary (Bdivm, e1, e2)
and ( ^ ) e1 e2 = D_binary (Bexpm, e1, e2)
;;

let sqrt e =
  let half = D_number "0.5" in
  make_paren e ^ half
;;

let square e = make_paren e * make_paren e;;

let int_part e = D_unary (Uint, e)

let size x =
  D_bracket
    (D_binary
      (Bhconc, D_unary (Usizerow, x), D_unary (Usizecol, x)))
;;

let transl_variable = function
  | "fabs" -> "abs"
  | "ln" -> "log"
  | "sgn" -> "sign"
  | op -> op
;;

let transl_parameter v = int_of_string v;;

type size_side =
   | Pleft of int
   | Pright of int
   | Pboth of int
   | Vleft of string
   | Vright of string
   | Vboth of string
   | Aboth of Deep_ast.expression
   | Aright of Deep_ast.expression
   | Aleft of Deep_ast.expression
   | Snone of string
;;

let transl_size_op = function
  | Aleft _ | Pleft _ | Vleft _ -> Usizerow
  | Aright _ | Pright _ | Vright _ -> Usizecol
  | Aboth _ | Pboth _ | Vboth _ -> Usizeprod
  | Snone _ -> assert false
;;

let ident_of_size_side = function
  | Pleft int | Pright int | Pboth int -> D_parameter int
  | Vleft v | Vright v | Vboth v -> D_variable v
  | Aboth exp | Aright exp | Aleft exp -> D_accu exp
  | Snone _ -> assert false
;;

let check_out_of_range i op =
  match i with
  | Pboth 0 ->
    failwith
      (Printf.sprintf
         "%s is used out of context"
         (if op = E_colon then ":" else "$"))
  | Pleft _ | Pright _ | Pboth _
  | Vleft _ | Vright _ | Vboth _
  | Aboth _ | Aright _ | Aleft _ -> ()
  | Snone s ->
    failwith
      (Printf.sprintf
         "%s is used out of context as argument of %s"
         (if op = E_colon then ":" else "$")
         s)
;;

let rec transl i =

  let rec loop exp =
    match exp with
    | E_paren (E_paren e) -> loop (E_paren e)
    | E_bracket (E_bracket exp) -> loop (E_bracket exp)
    | E_colon ->
      check_out_of_range i E_colon;
      let v = ident_of_size_side i
      and op = transl_size_op i in
      D_bracket (D_binary (Brange, D_number "1", D_unary (op, v)))
    | E_dollar ->
      check_out_of_range i E_dollar;
      let v = ident_of_size_side i
      and op = transl_size_op i in
      D_unary (op, v)
    | E_number n -> D_number n
    | E_string s -> D_string s
    | E_matrix exps -> D_matrix (List.map (List.map loop) exps)
    | E_variable v -> D_variable (transl_variable v)
    | E_parameter v -> D_parameter (transl_parameter v)
    | E_if (cond, e_then, e_else) ->
      D_if (loop cond, loop e_then, loop e_else)
    | E_switch {
        switch_subject = s;
        switch_cases = [];
        switch_default = d;
      } ->
      loop (
        E_switch {
          switch_subject = s;
          switch_cases = [ [s], d ];
          switch_default = d;
        }
      )
    | E_switch {
        switch_subject = s;
        switch_cases = cls;
        switch_default = d;
      } ->
      let transl_clauses cls =
        let transl_clause (pats, e) =
          List.map loop pats, loop e in
        List.map transl_clause cls in
      let s = loop s in
      let cls = transl_clauses cls in
      let d = loop d in
      D_switch {
        dswitch_subject = s;
        dswitch_cases = cls;
        dswitch_default = d;
      }
    | E_apply (E_parameter p, e_args)
    | E_apply (E_paren (E_parameter p), e_args) ->
      let p_idx = transl_parameter p in
      let p_e = D_parameter p_idx in
      let e_args = List.map transl_parens e_args in
      begin match e_args with
      | [ e_arg; ] ->
        D_binary (Bextract, p_e, transl (Pboth p_idx) e_arg)
      | [ E_colon; E_colon; ] -> p_e
      | [ E_colon; e_arg2; ] ->
        D_binary (Bextract_all_rows, p_e, transl (Pright p_idx) e_arg2)
      | [ e_arg1; E_colon; ] ->
        D_binary (Bextract_all_columns, p_e, transl (Pleft p_idx) e_arg1)
      | [ e_arg1; e_arg2; ] ->
        D_ternary
          (Textract, p_e,
           transl (Pleft p_idx) e_arg1, transl (Pright p_idx) e_arg2)
      | _ ->
        let nargs = List.length e_args in
        transl_error
          (Printf.sprintf
             "illegal extraction of parameter %d with %d arguments"
             p_idx nargs)
      end

    | E_apply (E_variable v, e_args)
    | E_apply (E_paren (E_variable v), e_args) ->
      let name_f = transl_variable v in
      let e_f = D_variable name_f in
      let e_args = List.map transl_parens e_args in
      begin match e_args with
      | [ e_arg; ] ->
        D_apply (e_f, [ transl (Vboth name_f) e_arg; ])
      | [ e_arg1; e_arg2; ] ->
        D_apply (e_f,
          [
            transl (Vleft name_f) e_arg1;
            transl (Vright name_f) e_arg2;
          ]
        )
      | e_args ->
        D_apply (e_f, List.map (transl (Snone name_f)) e_args)
      end

    | E_apply (E_paren (E_record_access (e1, label)), e_args)
    | E_apply (E_record_access (e1, label), e_args) ->
      let v =
        begin match e1 with
        | E_variable v -> v
        | _ ->
          transl_error
            (Printf.sprintf
               "illegal record access to field %s of a non \
                variable expression"
               label)
        end in
      let e_f = Printf.sprintf "%s.%s" v label in
      let e_args = List.map transl_parens e_args in
      begin match e_args with
      | [ e_arg; ] ->
        D_apply (D_record_access (loop e1, label),
          [ transl (Vboth e_f) e_arg ])
      | [ e_arg1; e_arg2; ] ->
        D_apply (D_record_access (loop e1, label),
          [ transl (Vleft e_f) e_arg1; transl (Vright e_f) e_arg2 ])
      | e_args ->
        D_apply (D_record_access (loop e1, label),
          List.map (transl (Snone e_f)) e_args)
      end

    | E_apply (E_paren e_f, e_args)
    | E_apply (e_f, e_args) ->
      let e_f = loop e_f in
      let e_args = List.map transl_parens e_args in
      begin match e_args with
      | [ e_arg; ] ->
        D_binary (Bextract, e_f, transl (Aboth e_f) e_arg)
      | [ E_colon; E_colon; ] -> e_f
      | [ E_colon; e_arg2; ] ->
        D_binary (Bextract_all_rows, e_f, transl (Aright e_f) e_arg2)
      | [ e_arg1; E_colon; ] ->
        D_binary (Bextract_all_columns, e_f, transl (Aleft e_f) e_arg1)
      | [ e_arg1; e_arg2; ] ->
        D_ternary
          (Textract, e_f,
           transl (Aleft e_f) e_arg1, transl (Aright e_f) e_arg2)
      | _ ->
        let nargs = List.length e_args in
        transl_error
          (Printf.sprintf
             "illegal extraction of expression with %d arguments" nargs)
      end

    | E_unary ("size", x) ->
      let x = transl_parens x in
      size (loop x)
    | E_unary ("sum", x) ->
      let x = transl_parens x in
      D_unary (Usum, loop x)
    | E_unary ("+", x) -> loop x
    | E_unary ("-", E_number n) -> D_number (mk_uminus n)
    | E_unary ("sqrt", x) -> let x = loop x in sqrt x
    | E_unary (op, x) ->
      let x = transl_parens x in
      D_unary (transl_prefix_op op, loop x)
    | E_binary (":", x, E_binary (":", y, z)) ->
      begin match z with
      | E_binary (":", _, _) ->
        failwith "too many `:' operators"
      | _ ->
        D_ternary (Trange,
          loop (transl_parens x),
          loop (transl_parens y),
          loop (transl_parens z))
      end
    | E_binary (":=", e1, (E_bracket (E_matrix []) as e2)) ->
      begin match e1 with
      | E_apply (_, [ E_colon; E_colon; ])
      | E_unary (":", _) -> loop e2
      | E_apply (_e_f, [ _ ]) ->
        let e1 = loop e1 in
        begin match e1 with
        | D_binary (Bextract, e_f, e_arg1) ->
          D_binary (Bremove, e_f, e_arg1)
        | D_apply (e_f, [e_arg1]) ->
          D_binary (Bremove, e_f, e_arg1)
        | _ -> assert false
        end
      | E_apply (_e_f, [ E_colon; _; ]) ->
        let e1 = loop e1 in
        begin match e1 with
        | D_binary (Bextract_all_rows, e_f, e_arg2) ->
          D_binary (Bremove_all_rows, e_f, e_arg2)
        | D_apply (e_f, [ _; e_arg2]) ->
          D_binary (Bremove_all_rows, e_f, e_arg2)
        | _ -> assert false
        end
      | E_apply (_e_f, [ _; E_colon; ]) ->
        let e1 = loop e1 in
        begin match e1 with
        | D_binary (Bextract_all_columns, e_f, e_arg1) ->
          D_binary (Bremove_all_columns, e_f, e_arg1)
        | D_apply (e_f, [e_arg1; _]) ->
          D_binary (Bremove_all_columns, e_f, e_arg1)
        | _ -> assert false
        end
      | _ ->
        failwith "wrong assignment usage for remove"
      end

    | E_binary (":=", e1, e2) ->
      begin match e1 with
      (** loop e2 * ones (size_row (loop e1), size_col (loop e1)) *)
      | E_apply (_, [ E_colon; E_colon; ])
      | E_unary (":", _) -> loop e2
      | E_apply (_e_f, [ E_colon; _; ]) ->
        let e1 = loop e1 in
        let e2 = loop e2 in
        begin match e1 with
        | D_binary (Bextract_all_rows, e_f, e_arg2) ->
          D_ternary (Tassign_all_rows, e_f, e_arg2, e2)
        | D_apply (e_f, [ _; e_arg2]) ->
          D_ternary (Tassign_all_rows, e_f, e_arg2, e2)
        | _ -> assert false
        end
      | E_apply (_e_f, [ _; E_colon; ]) ->
        let e1 = loop e1 in
        let e2 = loop e2 in
        begin match e1 with
        | D_binary (Bextract_all_columns, e_f, e_arg1) ->
          D_ternary (Tassign_all_columns, e_f, e_arg1, e2)
        | D_apply (e_f, [ e_arg1; _]) ->
          D_ternary (Tassign_all_columns, e_f, e_arg1, e2)
        | _ -> assert false
        end
      | E_apply (_e_f, _args) ->
        let e1 = loop e1 in
        let e2 = loop e2 in
        begin match e1 with
        | D_binary (Bextract, e_f, e_arg1) ->
          D_ternary (Tassign, e_f, e_arg1, e2)
        | D_ternary (Textract, e_f, e_arg1, e_arg2) ->
          D_quaternary (Qassign, e_f, e_arg1, e_arg2, e2)
        | D_apply (e_f, [e_arg1]) ->
          D_ternary (Tassign, e_f, e_arg1, e2)
        | D_apply (e_f, [e_arg1; e_arg2]) ->
          D_quaternary (Qassign, e_f, e_arg1, e_arg2, e2)
        | _ -> failwith "invalid assignment"
        end
      | _ ->
        failwith "wrong assignment usage"
      end

    | E_binary ("size", x, E_number "1") ->
      D_unary (Usizerow, loop (transl_parens x))
    | E_binary ("size", x, E_number "2") ->
      D_unary (Usizecol, loop (transl_parens x))
    | E_binary ("size", _, _) -> failwith "invalid size index"
    | E_binary ("min", x, y) ->
      D_binary (Bmin, loop (transl_parens x), loop (transl_parens y))
    | E_binary ("max", x, y) ->
      D_binary (Bmax, loop (transl_parens x), loop (transl_parens y))
    | E_binary ("sum", x, E_number "1") ->
      D_unary (Usumrow, loop (transl_parens x))
    | E_binary ("sum", x, E_number "2") ->
      D_unary (Usumcol, loop (transl_parens x))
    | E_binary ("sum", _, _) -> failwith "invalid sum index"

    | E_bracket (E_binary (":", x, y)) -> loop (E_binary (":", x, y))

    | E_binary (op, x, y) ->
        let x = loop x and y = loop y in
        begin match op with
        | "hypot" -> sqrt (square x + square y)
        | "pow" | "power" -> make_paren x ^ make_paren y
        | "rem" ->
          x - int_part (make_paren ((make_paren x / make_paren y))) *
          make_paren y
        | ":" ->  D_bracket (D_binary (transl_infix_op op, x, y))
        | op -> D_binary (transl_infix_op op, x, y)
        end
      | E_record_access (e1, label) ->
        D_record_access (loop e1, label)
      | E_bracket exp -> D_bracket (loop exp)
      | E_paren exp -> D_paren (loop exp)

  and transl_parens = function
    | E_paren e -> transl_parens e
    | e -> e in

  loop
;;

let translate exp = transl (Snone "") exp;;
