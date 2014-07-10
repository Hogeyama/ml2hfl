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

(* $Id: wc.ml,v 1.6 2011-08-08 19:31:17 weis Exp $ *)

(**
- Variables exist in Caml.

  A variable [x] is defined using the ``ref'' variable constructor
  applied to its initial value (the initial value is mandatory).
  For instance,

     [let x = ref 0]

  defines the (integer) variable [x] with initial value [0].

- Once a variable has been defined, its contents can be changed using the
  assignment operator ``:=''.
  For instance,

     [x := 3]

  gives [3] as the new contents of variable [x].

- A variable and its contents are not confused in Caml:
  a variable containing an integer has type [int ref],
  its contents has type [int].

- The current contents of variable [x] is given by [!x].

Note:
  - the type of the contents of a variable is the type of its initial
    value.
    For instance, it is not possible to change the contents of an integer
    variable to give it a string contents.
*)

let chars = ref 0
and words = ref 0
and lines = ref 0
(** The variables that respectively contain the number of characters,
   resp. number of words, resp. number of lines read so far. *)
;;

(**
- New type definitions are introduced by the keyword type.

- To define an enumerated type, just list the set of alternatives.
*)
type state =
   | Inside_word
   | Outside_word
(** The (new) type [state] introduces two cases,

     [Inside_word] and [Outside_word],

   that will serve to denote if we are currently scanning a word or not. *)
;;

(**
- Case analysis is introduced by the keyword ``match''.
  A case analysis is a list of clauses
    [| pat -> e]
  meaning that if [pat] is the case at hand, [e] should be returned.

  For instance, to return the integer [1] if the character [c] is ['a']
  and [2] if the character [c] is ['b'], use

     match c with
     | 'a' -> 1
     | 'b' -> 2

  A catch all case is introduced by the special pattern ``_''. Hence,

     match c with
     | 'a' -> true
     | _ -> false

  tests if the character [c] is ['a'].

- A character can be read from an input channel using
  the primitive [input_char].

- The primitive [incr], increments a variable which contains an integer.

- Since the current value of the variable [x] is denoted by [!x],
  [incr x] is equivalent to [x := !x + 1].
*)
let count_channel in_channel =
  let rec count status =
    let c = input_char in_channel in
    incr chars;
    match c with
    | '\n' ->
      incr lines; count Outside_word
    | ' ' | '\t' ->
      count Outside_word
    | _ ->
      if status = Outside_word then incr words;
      count Inside_word in
  try count Outside_word with
  | End_of_file -> ()
;;

(**
- The primitive [open_in] opens an input channel reading from a file.
*)
let count_file fname =
  let ic = open_in fname in
  count_channel ic;
  close_in ic
;;

let print_result () =
  print_int !chars; print_string " characters, ";
  print_int !words; print_string " words, ";
  print_int !lines; print_string " lines";
  print_newline ()
;;

let count name =
  count_file name;
  print_result ()
;;

if !Sys.interactive then () else
try
  if Array.length Sys.argv <= 1 then
    (* No command-line arguments, we assume counting standard input. *)
    count_channel stdin
  else
    (* We have a command line argument: this argument is the file name to
       read from. *)
    for i = 1 to Array.length Sys.argv - 1 do
      count_file Sys.argv.(i)
    done;
  print_result ();
with
| Sys_error s ->
  print_string "I/O error: ";
  print_string s;
  print_newline ()
;;
