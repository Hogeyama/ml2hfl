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

(* $Id: main.ml,v 1.4 2011-08-08 19:31:17 weis Exp $ *)

open Types;;

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
  | 528 | 0 ->
    (* Very easy: easy_board. *)
    Read.board_of_string 3 3 "
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
  | 529 | 2 ->
    (* Très difficile *)
    Read.board_of_string 3 3 "
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
  | 3 ->
    (* Expert: expert_board. *)
    Read.board_of_string 3 3 "
            -------------
            |   | 47| 8 |
            | 6 |   |   |
            |  2|8  |1 9|
            -------------
            |   |   |   |
            |1  |6  |5  |
            | 3 |  4| 9 |
            -------------
            |879|   | 3 |
            |   | 3 |4  |
            |  4| 7 |   |
            ------------- "
  | 4 ->
    (* St Hubert riche en omega 3 *)
    Read.board_of_string 2 3 "
            ---------
            | 3 |   |
            |6  |5 1|
            ---------
            | 1 |  3|
            |3  | 5 |
            ---------
            |4 2|  6|
            |   | 2 |
            ---------    "
  | 5 ->
    (* Expert *)
    Read.board_of_string 3 3 "
            -------------
            |123|456|789|
            |   |   |   |
            |  7|   |1  |
            -------------
            |   |   |   |
            |   |6  |5  |
            | 3 |  4| 9 |
            -------------
            |   |   | 3 |
            |   | 3 |4  |
            |  4| 7 |   |
            ------------- "
  | 6 ->
    (* Expert *)
    Read.board_of_string 3 3 "
            -------------
            |123|4 6|789|
            |456|7 9|123|
            |   |   |   |
            -------------
            | 3 |8  |9 7|
            |76 |  2|8  |
            |  8|   | 3 |
            -------------
            |3 5|6 8|   |
            |   |31 |6  |
            |   |   | 18|
            ------------- "
  | 7 | 548 ->
    (* Expert *)
    Read.board_of_string 3 3 "
            -------------
            |8  | 61|   |
            |  1|   |   |
            |  5|93 | 6 |
            -------------
            |   |   |3 8|
            |6  |7  |5  |
            |34 |  8| 92|
            -------------
            |126|   |   |
            |   | 9 |2  |
            |  3| 8 |   |
            ------------- "
  | 8 ->
    (* Expert *)
    Read.board_of_string 3 3 "
            -------------
            |16 |   | 9 |
            |427|   |  8|
            | 53|8  |  7|
            -------------
            |  6|49 |  6|
            |   |751|  5|
            |   | 86|2 4|
            -------------
            |   |  9|7 3|
            |   |   |18 |
            |   |   | 29|
            ------------- "
  | 530 | 9 ->
    (* Virtuose *)
    Read.board_of_string 3 3 "
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

let usage () =
  prerr_endline "sudoku [-d] -n <number>";
  prerr_endline "option -d fires the debug mode";
  prerr_endline "  solves the problem number <number>.";
  prerr_endline "sudoku -f <h> <w> <file name>";
  prerr_endline "  solves the problem of size h x w written into <file name>.";
  prerr_endline "  (See problem.txt for an example.)";
;;

let solve_and_print_result out_board =
  Utils.debug_message "Board read";
  let in_board = Utils.in_of_out_board out_board in

  Utils.debug "@.***** Before saturation ****@." in_board;
  Solve.sature_board in_board;
  Utils.debug "***** After saturation ****" in_board;

  Solve.deduce_board in_board;
  Utils.debug "***** After deduction ****" in_board;

  let solution = Resolve.resolve_board in_board in
  match solution with
  | No_solution ->
    prerr_endline "There is no solution to this Sudoku.";
  | Unique_solution result_in_board ->
    Utils.debug_message "***** The unique solution is ****";
    Utils.print_result result_in_board;
  | Multiple_solutions (result_in_board, sols) ->
    prerr_endline (
      Printf.sprintf
        "There is %d solutions to this Sudoku."
        (1 + List.length sols));
    prerr_endline "***** The solutions are ****";
    Utils.iter_i 1
     (fun i result_in_board ->
      prerr_endline (Printf.sprintf "Solution %d" i);
      Utils.print_result result_in_board)
     (result_in_board :: sols)
;;

let set_file_to_read, get_file_to_read =
  let fname = ref None in
  (function s -> fname := Some s),
  (function () -> !fname)
;;

let set_problem_num, get_problem_num =
  let problem_num = ref None in
  (function n -> problem_num := Some n),
  (function () -> !problem_num)
;;

let set_problem_height, get_problem_height =
  let problem_height = ref None in
  (function n -> problem_height := Some n),
  (function () -> !problem_height)
;;
let set_problem_width, get_problem_width =
  let problem_width = ref None in
  (function n -> problem_width := Some n),
  (function () -> !problem_width)
;;

let get_specified_board () =
  match get_problem_num (), get_file_to_read () with
  | None, None -> usage (); exit 1
  | None, Some fname ->
    begin match get_problem_height (), get_problem_width () with
    | Some h, Some w -> Read.board_of_file h w fname
    | _ -> usage (); exit 2 end
  | Some n, None -> read_board n
  | Some _, Some _ -> usage (); exit 3
;;

let parse_command_line () =
  let args = Sys.argv in
  if Array.length args < 2 then begin usage (); exit 4 end else
  Arg.parse [
   "-n", Arg.Int set_problem_num, "\
         <int> solves the Sudoku problem numbered with integer <int>.";
   "-f", Arg.Tuple [
           Arg.Int set_problem_height;
           Arg.Int set_problem_width;
           Arg.String set_file_to_read;
         ], "\
         <w> <h> <fname> solves the Sudoku problem of size h x w, \
         written into the file <fname>.";
   "-d", Arg.Unit Utils.set_debug, "\
         fires the debug mode.";
(*   "-h", Arg.Unit usage, "prints this list of options.";
   "-help", Arg.Unit usage, "prints usage of the command.";*)
  ]
  (function s ->
     prerr_endline (Printf.sprintf "sudoku: %s argument not understood." s);
     usage ();
     exit 5)
    "sudoku <options> <arguments>"
(*  "sudoku -n <number>
     solves the Sudoku problem number <number>.
   sudoku -f <h> <w> <file name>
     solves the Sudoku problem of size h x w, written into <file name>.
     (See problem.txt in the source directory for an example.)"
*)
;;

let main () =
  parse_command_line ();
  solve_and_print_result (get_specified_board ());
  exit 0;
;;

main ();;
