{
open Util
open HorSat_parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z' '_']
let upper = ['A'-'Z']
let symbol = ['(' ')' '*' '?' '|' '+' ',' '!' ';' '.' ':' '#']

rule header = parse
  "The property is satisfied." { SATISFIED }
| "The property is NOT satisfied." { UNSATISFIED }
| _ { header lexbuf }
| eof { failwith "lex error" }

and token = parse
  "HorSat" { header lexbuf }
| "The size of typing:" { THE_SIZE_OF_TYPING }
| "A counterexample is:" { A_COUNTEREXAMPLE_IS }
| space+ { token lexbuf }
| '(' { LPAREN }
| ')' { RPAREN }
| "br_exists" { BR_EXISTS }
| "br_forall" { BR_FORALL }
| '_' { BOT }
| "unit" { UNIT }
| "event_fail" { FAIL }
| (lower|upper) (digit|lower|upper)* { IDENT (Lexing.lexeme lexbuf) }
| digit* { token lexbuf }
| "Elapsed Time:" { footer lexbuf }
| _ { Format.printf "ERROR:%s@." (Lexing.lexeme lexbuf); failwith "lex error" }

and footer = parse
  eof { EOF }
| _ { footer lexbuf }
