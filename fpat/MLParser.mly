%{
open ExtList

(** Simple ML expression parser *)

let print_error_information () =
  let st = Parsing.symbol_start_pos () in
  let en = Parsing.symbol_end_pos () in
  print_string ("File \"" ^ st.Lexing.pos_fname);
  Format.printf "\", line %d" st.Lexing.pos_lnum;
  Format.printf ", characters %d-%d:\n"
    (st.Lexing.pos_cnum - st.Lexing.pos_bol)
    (en.Lexing.pos_cnum - en.Lexing.pos_bol)

type t =
  Var of Id.t
| Kon of Id.t * t option
| Tuple of t list

let rec make_pattern_match pat exp =
  match pat with
    Var(id) ->
      id, exp
  | Kon(id1, None) ->
      let id = Id.gen_exp_var () in
      id, Exp.Match([], id, [Pattern.Kon(id1, None), exp])
  | Kon(id1, Some(pat1)) ->
      let id = Id.gen_exp_var () in
      let id', exp = make_pattern_match pat1 exp in
      id, Exp.Match([], id, [Pattern.Kon(id1, Some(id')), exp])
  | Tuple(pats) ->
      let id = Id.gen_exp_var () in
      let ids, exp =
        List.fold_left
          (fun (ids, exp) pat ->
            let id, exp =
              make_pattern_match pat exp in
            ids @ [id], exp)
          ([], exp)
          pats in
      id, Exp.Match([], id, [Pattern.Tuple(ids), exp])

let translate (pat, exp) =
  match pat with
    Var(id) ->
      Pattern.Var(id), exp
  | Kon(id1, None) ->
      Pattern.Kon(id1, None), exp
  | Kon(id1, Some(pat1)) ->
      let id, exp = make_pattern_match pat1 exp in
      Pattern.Kon(id1, Some(id)), exp
  | Tuple(pats) ->
      let ids, exp =
        List.fold_left
          (fun (ids, exp) pat ->
            let id, exp =
              make_pattern_match pat exp in
            ids @ [id], exp)
          ([], exp)
          pats in
      Pattern.Tuple(ids), exp

%}

%token FUN
%token FIX
%token LET
%token IN
%token MATCH
%token WITH
%token FAIL
%token AS

%token TYPE
%token OF
%token WHERE
%token TINT
%token TUNIT
%token TT
%token FF
%token SLASH_BSLASH
%token BSLASH_SLASH

%token <int> INTEGER
%token <Id.t> LIDENT
%token <Id.t> UIDENT
%token UNDERSCORE
%token EQUAL
%token COMMA
%token PERIOD
%token COLON
%token SEMICOLON
%token BAR
%token PLUS
%token MINUS
%token AST
%token SLASH
%token MOD
%token LANGLE
%token RANGLE
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE_EQUAL
%token RANGLE_EQUAL
%token LANGLE_RANGLE
%token MINUS_RANGLE
%token EQUAL_RANGLE
%token COLON_COLON
%token EOF

%token REC
%token AND
%token IF
%token THEN
%token ELSE

%right prec_pair
%right prec_let
%right prec_if
%right COMMA
%left EQUAL LANGLE RANGLE LANGLE_EQUAL RANGLE_EQUAL
%left PLUS MINUS
%left AST SLASH MOD
%right prec_unary_minus
%left prec_app

/* polymorphic types are not allowed */
%type
<[`MLType of HindleyMilner.ty | `MLTypeSchema of HindleyMilner.ts | `TypeInst of HindleyMilner.ty list | `TypeTemp of ExpType.t | `TypeTempInst of ExpType.t list] list TopLevel.t list> top_level
%start top_level

%%

top_level:
  /* empty */
    { [] }
| TYPE LIDENT top_level
    { (TopLevel.Type($2, [](*TODO*)))::$3 }
| LET rec_opt lident_opt arg_list EQUAL exp top_level
    { if Util.duplicated $4 then
        (print_error_information ();
        raise (Global.Syntax_error "duplicated variable"))
      else
        let exp = if $4 = [] then $6 else List.fold_right (fun id exp -> Exp.Abs([], id, exp)) $4 $6 in
        let exp = if $2 then Exp.Fix([], $3, exp) else exp in
        (TopLevel.Let($3, exp))::$7 }
| error
    { print_error_information ();
      raise (Global.Syntax_error "Syntax error") }

lident_opt:
  LIDENT
    { $1 }
| UNDERSCORE
    { Id.gen_exp_var () }

arg_list:
  /* empty */
    { [] }
| lident_opt arg_list
    { $1::$2 }

rec_opt:
  /* empty */
    { false }
| REC
    { true }

sexp:
  simple_exp
    { $1 }
| MINUS INTEGER
    %prec prec_unary_minus
    { Exp.Con([], string_of_int (-$2)) }
| MINUS simple_exp
    %prec prec_unary_minus
    { Exp.App([], Exp.Con([], "(~-)"), $2) }
| sexp PLUS sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(+)"), $1), $3) }
| sexp MINUS sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(-)"), $1), $3) }
| sexp AST sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(*)"), $1), $3) }
| sexp SLASH sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(/)"), $1), $3) }
| sexp MOD sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(mod)"), $1), $3) }
| sexp EQUAL sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(=)"), $1), $3) }
| sexp LANGLE sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(<)"), $1), $3) }
| sexp LANGLE_EQUAL sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(<=)"), $1), $3) }
| sexp RANGLE sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(>)"), $1), $3) }
| sexp RANGLE_EQUAL sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(>=)"), $1), $3) }
| sexp LANGLE_RANGLE sexp
    { Exp.App([], Exp.App([], Exp.Con([], "(<>)"), $1), $3) }
/*
| sexp COLON_COLON sexp
    { Exp.Kon([], "Cons", Some(Exp.Tuple([], [$1; $3]))) }
*/
| FUN pattern MINUS_RANGLE exp
    { let id, exp = make_pattern_match $2 $4 in
      Exp.Abs([], id, exp) }
| simple_exp simple_exp_list
    %prec prec_app
    { match $1 with
        Exp.Kon([], id, None) -> let [exp] = $2 in Exp.Kon([], id, Some(exp))
      | _ -> List.fold_left (fun exp exp' -> Exp.App([], exp, exp')) $1 $2 }
| FIX LIDENT PERIOD exp
    { Exp.Fix([], $2, $4) }
| LET rec_opt pattern arg_list EQUAL exp IN exp
    %prec prec_let
    { if Util.duplicated $4 then
        (print_error_information ();
        raise (Global.Syntax_error "duplicated variable"))
      else
        if $4 = [] then begin
          assert (not $2);
				      let id, exp = make_pattern_match $3 $8 in
				      match $6 with Exp.Var(_, id') -> Exp.rename [id, id'] exp | _ -> Exp.Let([], id, $6, exp)
        end else
				      let id, exp2 = make_pattern_match $3 $8 in
				      let exp1 = List.fold_right (fun id exp -> Exp.Abs([], id, exp)) $4 $6 in
		        let exp1 = if $2 then Exp.Fix([], id, exp1) else exp1 in
				      Exp.Let([], id, exp1, exp2)}
| MATCH exp WITH patmat_list
    %prec prec_if
    { match $2 with
        Exp.Var(_, id) ->
          Exp.Match([], id, List.map translate $4)
      | _ ->
          let id = Id.gen_exp_var () in
          Exp.Let([], id, $2, Exp.Match([], id, List.map translate $4)) }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { let id = Id.gen_exp_var () in
      Exp.Let([], id, $2, Exp.Match([], id, [Pattern.Kon("True", None), $4; Pattern.Kon("False", None), $6])) }
| FAIL
    { Exp.Fail([]) }

exp:
  sexp
    { $1 }
| exp_list
    { Exp.Tuple([], $1) }

exp_list:
  sexp COMMA sexp
    { [$1; $3] }
| sexp COMMA exp_list
    { $1::$3 }

simple_exp:
  LIDENT
    { Exp.Var([], $1) }
| UIDENT
    { Exp.Kon([], $1, None) }
/*
| simple_exp PERIOD INTEGER
    { Exp.Sel([], $1, $3) }
*/
| LPAREN exp AS exp_type RPAREN /*????*/
    { Exp.Ascr([], $2, $4) }
| LPAREN exp RPAREN
    { $2 }
| LPAREN RPAREN
    { Exp.Kon([], "Unit", None) }
| INTEGER
    { Exp.Con([], string_of_int $1) }
/*
| LBRACKET RBRACKET
    { Exp.Kon([], "Nil", None) }
*/

simple_exp_list:
  simple_exp_list simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

pattern:
  LPAREN pattern RPAREN
    { $2 }
| lident_opt
    { Var($1) }
| UIDENT
    { Kon($1, None) }
| UIDENT pattern
    { Kon($1, Some($2)) }
| LPAREN pattern_list RPAREN
    { Tuple($2) }

patmat_list:
  pattern MINUS_RANGLE exp
    { [$1, $3] }
| pattern MINUS_RANGLE exp BAR patmat_list
    { ($1, $3)::$5 }

pattern_list:
  pattern COMMA pattern
    { [$1; $3] }
| pattern COMMA pattern_list
    { $1::$3 }

exp_type:
  /* empty */
    { ExpType.Adt("", [], "int", Vfol.G(Fol.True)) }
