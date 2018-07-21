(* To use printf without referencing the model name *)
open Util
open Combinator
open Printf
open Minisat

(*
 * Returns the string obtained from [str] by dropping the first
 * [n] characters. [n] must be smaller than or equal to the length
 * of the string [str].
 *)
let drop str n =
  let l = String.length str in
  String.sub str n (l - n)

(*
 * Returns the first [n] characters of the string [str] as a string.
 * [n] must be smaller than or equal to the length of the string [str].
 *)
let take str n = String.sub str 0 n

(*
 * Splits a string [str] whenever a character [ch] is encountered.
 * The returns value is a list of strings.
 *)
let split str ch =
  let rec split' str l =
    try
      let i = String.index str ch in
      let t = take str i in
      let str' = drop str (i+1) in
      let l' = t::l in
      split' str' l'
    with Not_found ->
      List.rev (str::l)
  in
  split' str []

(*
 * Processes the content of [file], adds the variables and clauses to Minisat
 * and returns a mapping between names and Minisat variables.
 *)
let process_file solver file =

  (* Mapping between variable names and indices. *)
  let vars = Hashtbl.create 0 in

  (* Processes a line containing a variable definition. *)
  let process_var line =
    let l = String.length line in
    assert (l > 2);
    assert (line.[1] = ' ');
    let name = drop line 2 in
    let v = solver#new_var in
    Hashtbl.add vars name v
  in

  (* Processes a line containing a clause. *)
  let process_clause line =
    let l = String.length line in
    assert (l > 2);
    assert (line.[1] = ' ');
    let lits =
        List.map
          (fun lit ->
            if lit.[0] = '-' then
              (false, drop lit 1)
            else
              (true, lit)
          )
          (split (drop line 2) ' ')
    in
    let clause =
        List.map
          (fun (sign, name) ->
            let var = Hashtbl.find vars name in
            if sign then
              Minisat.pos_lit var
            else
              Minisat.neg_lit var
          )
          lits
    in
    solver#add_clause clause
  in

  (* Read a new line and processes its content. *)
  let rec process_line () =
    try
      let line = input_line file in
      if line = "" then
        ()
      else
        (match line.[0] with
        | 'v' -> process_var line
        | 'c' -> process_clause line
        | '#' -> ()
        | _   -> assert false
        );
      process_line ()
    with End_of_file ->
      ()
  in
  process_line ();
  vars

(* Reads a given file and solves the instance. *)
let solve file =
  let ans = ref [] in
  let inchan = open_in file in
  let solver = new solver in
  let vars = process_file solver inchan in
  solver#simplify;
  begin
    match solver#solve with
    | Minisat.UNSAT -> printf "<unsat>\n"
    | Minisat.SAT   ->
      printf "<sat>\n";
      Hashtbl.iter
	(fun name v ->
	  let b = (Minisat.string_of_value (solver#value_of v)) in
          (* printf "  %s=%s\n" name b; *)
	  let f = Idnt.make name
	          |> flip Formula.mk_var [] in
	  ans := (f, bool_of_string b)::!ans
        )
	vars
  end;
  printf "[statistics]\n";
  solver#print_stats;
  printf "\n";
  !ans

let _ =
  SATSolver.ext_solve_minisat := solve;
