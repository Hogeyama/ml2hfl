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

(* $Id: main.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

open Types;;

Utils.debug_flag := false;;

let read_out_board () = [|
  [| None; None; None;
     None; Some Four; Some Seven;
     None; Some Eight; None; |];
  [| None; Some Six; None;
     None; None; None;
     None; None; None; |];
  [| None; None; Some Two;
     Some Eight; None; None;
     Some One; None; Some Nine; |];
  [| None; None; None;
     None; None; None;
     None; None; None; |];
  [| Some One; None; None;
     Some Six; None; None;
     Some Five; None; None; |];
  [| None; Some Three; None;
     None; None; Some Four;
     None; Some Nine; None; |];
  [| Some Eight; Some Seven; Some Nine;
     None; None; None;
     None; Some Three; None; |];
  [| None; None; None;
     None; Some Three; None;
     Some Four; None; None; |];
  [| None; None; Some Four;
     None; Some Seven; None;
     None; None; None; |];
|]
;;

let out_board = read_out_board ()
;;

let expert_board =
 [|
  [| None; None; None;
     None; Some Four; Some Seven;
     None; Some Eight; None; |];
  [| None; Some Six; None;
     None; None; None;
     None; None; None; |];
  [| None; None; Some Two;
     Some Eight; None; None;
     Some One; None; Some Nine; |];
  [| None; None; None;
     None; None; None;
     None; None; None; |];
  [| Some One; None; None;
     Some Six; None; None;
     Some Five; None; None; |];
  [| None; Some Three; None;
     None; None; Some Four;
     None; Some Nine; None; |];
  [| Some Eight; Some Seven; Some Nine;
     None; None; None;
     None; Some Three; None; |];
  [| None; None; None;
     None; Some Three; None;
     Some Four; None; None; |];
  [| None; None; Some Four;
     None; Some Seven; None;
     None; None; None; |];
|]
;;

let easy_board =
 [|
  [| Some One; None; None;
     None; Some Two; Some Nine;
     None; None; None; |];
  [| None; Some Seven; Some Eight;
     None; None; None;
     None; None; None; |];
  [| None; None; None;
     None; None; None;
     Some Six; Some Four; Some Three; |];
  [| None; Some Five; None;
     None; None; None;
     None; None; None; |];
  [| Some Four; None; None;
     Some Eight; Some One; None;
     None; None; None; |];
  [| Some Three; Some One; None;
     None; None; Some Two;
     None; Some Five; Some Seven; |];
  [| None; Some Four; Some Nine;
     None; None; None;
     None; None; None; |];
  [| None; None; None;
     None; Some Seven; None;
     Some One; None; None; |];
  [| Some Two; None; None;
     None; Some Nine; None;
     Some Four; None; None; |];
|]
;;

let read_board = function
  | 0 ->
    (* Expert *)
    expert_board
  | 1 ->
    (* Facile *)
    easy_board
  | 528 | 2 ->
    (* Très facile *)
    Read.board_of_string  "
            -------------
            |1  | 29|   |
            | 78|   |   |
            |   |   |643|
            -------------
            | 5 |   |   |
            |4  |81 |   |
            |31 |  2| 57|
            -------------
            | 49|   |   |
            |   | 7 |1  |
            |2  | 9 |4  |
            ------------- "
  | 529 | 3 ->
    (* Très difficile *)
    Read.board_of_string  "
            -------------
            |2  | 87|   |
            | 5 |   |  7|
            |  8|9  |21 |
            -------------
            |   |   |   |
            |1  |4  |3 9|
            |94 |  2|  1|
            -------------
            |835|   |   |
            |   | 5 |6 2|
            |  6| 41|   |
            ------------- "
  | 530 | 4 ->
    (* Virtuose *)
    Read.board_of_string  "
            -------------
            |   | 1 |  5|
            |   |4  |7 2|
            |   | 8 |6  |
            -------------
            |  8|   |4  |
            | 6 |  5|   |
            |1  |9  | 3 |
            -------------
            |34 | 6 | 1 |
            |   |2  |9  |
            | 5 |  7|   |
            ------------- "
  | _ -> failwith "read_board: unknown sudoku problem."
;;

let out_board = read_board 530;;

let in_board = Utils.in_of_out_board out_board;;

(************************** Comment out, if you want to follow the resolution.

Utils.debug "@.***** Before saturation ****@." in_board
;;

Solve.sature_board in_board
;;

Utils.debug "***** After saturation ****" in_board
;;

Solve.deduce_board in_board
;;

Utils.deduce_board "***** After deduction ****" in_board
;;
****************************)

let result_in_board = Resolve.resolve_board in_board;;

(****************************
let result_out_board = Utils.out_of_in_board result_in_board;;

Utils.debug "***** After resolution ****" result_out_board
;;
****************************)

Utils.print_result "***** Final result ****" result_in_board
;;
