%{
let print_error_information () =
  let st = Parsing.symbol_start_pos () in
  let en = Parsing.symbol_end_pos () in
  print_string ("File \"" ^ st.Lexing.pos_fname);
  Format.printf "\", line %d" st.Lexing.pos_lnum;
  Format.printf ", characters %d-%d:\n"
    (st.Lexing.pos_cnum - st.Lexing.pos_bol)
    (en.Lexing.pos_cnum - en.Lexing.pos_bol)

let parse_error _ = print_error_information ()
%}

%token EOF
%token <string> IDENT
%token LPAREN
%token RPAREN
%token BR_EXISTS
%token BR_FORALL
%token UNIT
%token BOT
%token SATISFIED
%token UNSATISFIED
%token THE_SIZE_OF_TYPING
%token A_COUNTEREXAMPLE_IS

/* priority : low -> high */

%start output
%type <[`Satisfied | `Unsatisfied of HorSat_syntax.result_tree]> output

%%

output:
  SATISFIED EOF { `Satisfied }
| UNSATISFIED THE_SIZE_OF_TYPING A_COUNTEREXAMPLE_IS counterexample EOF { `Unsatisfied $4 }

counterexample:
  LPAREN counterexample RPAREN
  { $2 }
| BR_EXISTS counterexample counterexample
  { HorSat_syntax.Exists($2, $3) }
| BR_FORALL counterexample BOT
  { HorSat_syntax.Forall(0, $2) }
| BR_FORALL BOT counterexample
  { HorSat_syntax.Forall(1, $3) }
| IDENT counterexample
  { HorSat_syntax.Label($1, $2) }
| UNIT
  { HorSat_syntax.End }
