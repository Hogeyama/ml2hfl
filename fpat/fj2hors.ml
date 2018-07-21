open Fpat
open Util
open Combinator

let parse_file filename =
  let inchan = open_in filename in
  let lb = Lexing.from_channel inchan in
  lb.Lexing.lex_curr_p <-
    { Lexing.pos_fname = Filename.basename filename;
      Lexing.pos_lnum = 1;
      Lexing.pos_cnum = 0;
      Lexing.pos_bol = 0 };
  let tl = JavaParser.prog JavaLexer.token lb in
  close_in inchan;
  tl

let parse_string str =
  let lb = Lexing.from_string str in
  lb.Lexing.lex_curr_p <-
    { Lexing.pos_fname = "";
      Lexing.pos_lnum = 1;
      Lexing.pos_cnum = 0;
      Lexing.pos_bol = 0 };
  JavaParser.prog JavaLexer.token lb

let _ =
  let filename =
    if Array.length Sys.argv = 2 then
      Sys.argv.(1)
    else
      failwith "invalid arguments"
  in
  parse_file filename
  |> JavaProg.embed
  |> MLExp.elim_fun
  |> Logger.pprintf "%a@,@," MLExp.pr
  |> MLExp.elim_def_with_mult_args
  |> Logger.pprintf "%a@,@," MLExp.pr
  |> MLExp.cps (fun _ -> MLExp.mk_end)
  |> Logger.pprintf "%a@,@," MLExp.pr
  |> MLExp.lift
  |> Logger.pprintf "%a@,@," MLExp.pr
  |> MLExp.inline []
  |> Logger.pprintf "%a@,@," MLExp.pr
  |> Logger.printf "%a@," HORS.pr
