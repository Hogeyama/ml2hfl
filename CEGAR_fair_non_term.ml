open HORS_syntax
open Util

type tree =
  | Node   of string * tree
  | Branch of string * tree * tree
  | Leaf

let rec pp_tree fm = function
  | Node (l, t) ->
     Format.fprintf fm "(%s %a)" l pp_tree t
  | Branch (l, t1, t2) ->
     Format.fprintf fm "(%s %a %a)" l pp_tree t1 pp_tree t2
  | Leaf ->
     Format.fprintf fm "()"

let rec subst (x, ex1) ex2 =
  match ex1 with
  | Var s ->
     if s = x then
       ex2
     else
       Var s
  | Apply (e1, e2) ->
     Apply (subst (x, e1) ex2, subst (x, e2) ex2)
  | Abst (s, e1) ->
     Abst (s, subst (x, e1) ex2)

let rec eval env n i expr =
  let get_fun f =
    try
      Some (List.assoc f env)
    with Not_found ->
      None in

  if i >= n then
    (Leaf, expr)
  else match expr with
  | Var s ->
     begin match get_fun s with
     | None ->
        (Leaf, expr)
     | Some e ->
        eval env n (i + 1) e
     end
  | Apply (e1, e2) ->
     begin
       match eval env n (i+1) e1 with
       | (l, Var s) when s = "br_exists" ->
          failwith "TODO"
       | (l, Var s) ->
          let (t, e) = eval env n (i+1) e2 in
          (Node (s, t), e)
       | (l, Abst (x, e)) ->
          eval env n (i + 1) (subst (x, e) e2)
       | _ -> assert false
     end
  | Abst _ ->
     (Leaf, expr)

let expand n env =
  let (l, _) = eval env n 0 (Var "Main") in
  l




let show_pos fname filebuf =
  let pos = filebuf.Lexing.lex_start_p in
  Printf.eprintf "File \"%s\", line %d, character %d:\n"
    fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let show_error fmt =
  let go str =
    Printf.eprintf "error: %s\n" str;
    exit 1 in
  Printf.ksprintf go fmt

let cegar () =
  let fname = (Filename.chop_extension !Flag.filename) ^ "_error.hors" in
  let inchan = open_in fname in
  let filebuf = Lexing.from_channel inchan in
  try
    let rules = HORS_parser.top HORS_lexer.token filebuf in
    Format.printf "%a@." (Format.pp_print_list HORS_syntax.pp_rule) rules;
    let labels = expand 100 rules in
    Format.printf "Result: %a@." pp_tree labels
  with
  | HORS_lexer.LexerError msg ->
     (show_pos fname filebuf;
      show_error "lex: %s" msg)
  | Failure "parse error" ->
     begin
       show_pos fname filebuf;
       let s = Lexing.lexeme filebuf in
       Format.eprintf "error: parser : syntax error near '%s'@." s;
       if s = "->" then
         Format.eprintf "Did you forget '.' at end of line?@."
     end
