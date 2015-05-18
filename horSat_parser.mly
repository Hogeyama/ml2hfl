%{
open HorSat_syntax
%}

%token EOF
%token <int> INT
%token <int> STATE
%token <string> IDENT
%token LPAREN
%token RPAREN
%token COMMA
%token DOT
%token BR_EXISTS
%token BR_FORALL
%token UNIT
%token FAIL
%token BOT
%token SATISFIED
%token UNSATISFIED
%token THE_SIZE_OF_TYPING
%token A_COUNTEREXAMPLE_IS

/* priority : low -> high */

%start output_apt
%type <HorSat_syntax.result> output_apt
%start output
%type <HorSat_syntax.result> output

%%

output_apt:
  SATISFIED EOF { Satisfied }
| UNSATISFIED THE_SIZE_OF_TYPING A_COUNTEREXAMPLE_IS counterexample_apt EOF { UnsatisfiedAPT $4 }

counterexample_apt:
  LPAREN counterexample_apt RPAREN
  { $2 }
| BR_EXISTS counterexample_apt counterexample_apt
  { HorSat_syntax.Exists($2, $3) }
| BR_FORALL counterexample_apt BOT
  { HorSat_syntax.Forall(0, $2) }
| BR_FORALL BOT counterexample_apt
  { HorSat_syntax.Forall(1, $3) }
| IDENT counterexample_apt
  { HorSat_syntax.Label($1, $2) }
| UNIT
  { HorSat_syntax.End }
| FAIL BOT
  { HorSat_syntax.Fail }

output:
  SATISFIED EOF { Satisfied }
| UNSATISFIED THE_SIZE_OF_TYPING INT A_COUNTEREXAMPLE_IS counterexample EOF { Unsatisfied $5 }

id:
  IDENT
  { $1 }
| FAIL
  { "event_fail" }

counterexample:
  { [] }
| LPAREN id COMMA INT RPAREN counterexample
  { ($2, $4) :: $6 }
