%{
  open Util
  open Combinator

  let print_error_information () =
    let st = Parsing.symbol_start_pos () in
    let en = Parsing.symbol_end_pos () in
    print_string ("File \"" ^ st.Lexing.pos_fname);
    Format.printf "\", line %d" st.Lexing.pos_lnum;
    Format.printf ", characters %d-%d:\n"
                  (st.Lexing.pos_cnum - st.Lexing.pos_bol)
                  (en.Lexing.pos_cnum - en.Lexing.pos_bol)

  type 'a t =
    | Field of Idnt.t * Idnt.t
    | Const of (Idnt.t -> (Idnt.t * Idnt.t) list -> unit)
    | Method of Idnt.t * Idnt.t * (Idnt.t * Idnt.t) list * 'a JavaExp.t option

%}

%token CLASS
%token EXTENDS
%token RETURN
%token IF
%token IF_
%token THEN_
%token ELSE_
%token FAIL
%token NEW
%token THIS
%token SUPER
%token MAIN

%token NOT
%token ASSERT

%token <Idnt.t> LIDENT
%token <Idnt.t> UIDENT
%token <int> INT
%token INT_
%token TRUE
%token FALSE
%token EQUAL
%token ADD
%token SUB
%token MUL
%token COMMA
%token PERIOD
%token SEMICOLON
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%type <int list JavaProg.t> prog
%start prog

%%

prog:
| classes MAIN LPAREN args RPAREN LBRACE RETURN exp SEMICOLON RBRACE
    { JavaProg.Prog($1, JavaMain.Main([],$4,$8)) }
| classes MAIN LPAREN args RPAREN LBRACE ASSERT exp SEMICOLON RBRACE
    { JavaProg.Prog($1, JavaMain.Main([],$4,JavaExp.Assert([],$8))) }
| error
    { print_error_information ();
      raise (Failure "Syntax error") }

classes:
| /* empty */
    { [] }
| classdef classes
    { $1 :: $2 }

classdef:
| CLASS UIDENT EXTENDS UIDENT LBRACE defs RBRACE
    { let flds, con, meths =
        List.fold_left
          (fun (flds, con, meths) def ->
           match def with
           | Field(id1, id2) ->
              flds @ [id1, id2], con, meths
           | Const(f) ->
              assert(con = None);
              flds, Some(f), meths
           | Method(id1, id2, args, exp) ->
              flds, con, meths @ [id1, id2, args, exp])
          ([], None, [])
        $6
      in
      let Some(f) = con in
      f $2 flds;
      $2, ($4, flds, meths) }

defs:
| /* empty */
    { [] }
| UIDENT LIDENT SEMICOLON defs
    { Field($1, $2) :: $4 }
| UIDENT LPAREN args RPAREN
  LBRACE
  SUPER LPAREN lidents RPAREN SEMICOLON
  inits
  RBRACE
  defs
    { Const
        (fun id flds ->
         assert
           (id = $1
            && (*flds = $3Ç©ÇÁsuper classÇÃà¯êîÇà¯Ç¢ÇΩÇ‡ÇÃ*) true
            && (*@todo $8Ç™super classÇÃà¯êîÇ∆ìØÇ∂*) true
            && (*@todo permutationÇ‡ãñÇ∑Ç◊Ç´*)
              List.map snd flds = $11))
      :: $13 }
| UIDENT LIDENT LPAREN args RPAREN LBRACE body SEMICOLON RBRACE defs
    { (Method($1, $2, $4, $7))::$10 }

body:
| RETURN exp
    { Some($2) }
| ASSERT exp
    { Some(JavaExp.Assert([], $2)) }

args:
| /* empty */
    { [] }
| args_
    { $1 }

args_:
| UIDENT LIDENT
    { [$1, $2] }
| UIDENT LIDENT COMMA args_
    { ($1, $2) :: $4 }

lidents:
| /* empty */
    { [] }
| lidents_
    { $1 }

lidents_:
| LIDENT
    { [$1] }
| LIDENT COMMA lidents_
    { $1 :: $3 }

inits:
| /* empty */
    { [] }
| THIS PERIOD LIDENT EQUAL LIDENT SEMICOLON inits
    { assert($3 = $5); $3 :: $7 }

exp:
| exp_
    { $1 }
| LPAREN UIDENT RPAREN exp
    { JavaExp.Cast([], $2, $4) }
| NOT exp
    { JavaExp.Not([], $2) }

exp_:
| TRUE
    { JavaExp.Bool([], true) }
| FALSE
    { JavaExp.Bool([], false) }
| LIDENT
    { JavaExp.Var([], $1) }
| THIS
    { JavaExp.Var([], Idnt.make "this") }
| NEW UIDENT LPAREN exps RPAREN
    { JavaExp.New([], $2, $4) }
| exp_ PERIOD LIDENT
    { JavaExp.Field([], $1, $3) }
| exp_ PERIOD LIDENT LPAREN exps RPAREN
    { JavaExp.Method([], $1, $3, $5) }
| LPAREN exp RPAREN
    { $2 }
| INT
    { JavaExp.Int([], $1) }
| INT_
    { JavaExp.Int_([]) }
| FAIL
    { JavaExp.Fail([]) }
| IF exp_ THEN_ exp_ ELSE_ exp_
    { JavaExp.IF([],$2,$4,$6) }
| IF_ THEN_ exp_ ELSE_ exp_
    { JavaExp.IF_([],$3,$5) }
| exp_ EQUAL exp_
    { JavaExp.Eq([],$1,$3) }
| exp_ ADD exp_
    { JavaExp.Add([],$1,$3) }
| exp_ SUB exp_
    { JavaExp.Sub([],$1,$3) }
| exp_ MUL exp_
    { JavaExp.Mul([],$1,$3) }


exps:
| /* empty */
    { [] }
| exps_
    { $1 }

exps_:
| exp
    { [$1] }
| exp COMMA exps_
    { $1::$3 }
