%{
  open Util
  open Combinator
  open Formula
  open PredVar
  open HornClause
  open HCCS
  open NumAtom

  (** HCCS parser *)

let rec mk_idnt_t str =
  try
    let m = Str.search_backward
        (Str.regexp "\\[") str (String.length str)
    in
    let id = Str.string_before str m in
    let aft = Str.string_after str m in
    let [uid; arity] = Str.split (Str.regexp "[^0-9]+") aft in
    let idnt = mk_idnt_t id in
    Idnt.T(idnt, int_of_string uid, int_of_string arity)
  with
  | Not_found ->
    Idnt.make str
%}

%token <int> INT
%token <float> FLOAT
%token <string> COEFF VAR CON VAR_T
%token PLUS FPLUS MINUS FMINUS
%token TIMES FTIMES DIV FDIV
%token EQ NOTEQ NOT GT LT GEQ LEQ COMMA
%token AND OR
%token LPAREN RPAREN LBRA RBRA
%token DECLARE QUERY
%token BOT TOP
%token DOT CL EOF

%left OR
%left AND

%nonassoc EQ NOTEQ GT LT GEQ LEQ

%left PLUS FPLUS MINUS FMINUS
%left TIMES FTIMES DIV FDIV
%nonassoc UNARY
%left COMMA

%nonassoc NOT
%nonassoc VAR VAR_T CON INT FLOAT

%start parser_main
%type <HCCS.t> parser_main
%start atom
%type <Formula.t> atom
%%

parser_main:
 | hcs EOF { $1 }
 | error {
     Printer.print_parser_error_information ();
     raise (Global.Syntax_error "Parse error")
   }

hcs:
 | /* empty */ { [] }
 | hc hcs { $1 :: $2 }

hc:
 | head DOT
   { HornClause.of_formulas (Formula.fvs $1) $1 [] }
 | head DECLARE body DOT
   { HornClause.of_formulas (List.concat_map Formula.fvs ($1 :: $3)) $1 $3 }
 | QUERY body DOT
   { HornClause.of_formulas (List.concat_map Formula.fvs $2) Formula.mk_false $2 }

head:
 | BOT
   { Formula.mk_false }
 | VAR
   { Pva.make (Idnt.make $1) [] |> Pva.to_formula }
 | VAR LPAREN terms RPAREN
   { Pva.make (Idnt.make $1) $3 |> Pva.to_formula }
 | VAR_T LPAREN terms RPAREN
   { Pva.make (mk_idnt_t $1) $3 |> Pva.to_formula }

body:
 | formula { [$1] }
 | formula COMMA body { $1 :: $3 }

formula:
 | atom { $1 }

atom:
 /*
 | VAR
   { Pva.make (Idnt.make $1) [] |> Pva.to_formula }
   */
 | VAR LPAREN terms RPAREN
   { Pva.make (Idnt.make $1) $3 |> Pva.to_formula }
 | VAR_T LPAREN terms RPAREN
   { Pva.make (mk_idnt_t $1) $3 |> Pva.to_formula }
 | term
   { Formula.of_term (fst $1) }

term:
 | VAR
   { Term.mk_var (Idnt.make $1),
     Type.mk_unknown }
 | VAR_T /* e.g. foo[0:0] */
   { Term.mk_var (mk_idnt_t $1),
     Type.mk_unknown }
 | COEFF
   { Term.mk_var (Idnt.mk_coeff $1),
     Type.mk_int (* @assume coeffs are integers *) }
 | LPAREN RPAREN
   { UnitTerm.make, Type.mk_unit }
 | BOT
   { BoolTerm.mk_false, Type.mk_bool }
 | TOP
   { BoolTerm.mk_true, Type.mk_bool }
 | INT
   { IntTerm.make $1, Type.mk_int }
 | CON LPAREN terms RPAREN
   { let ts, tys = List.split $3 in
     let ty = Type.mk_fun_args_ret tys Type.mk_unknown in
     Term.mk_app
       (Term.mk_const (Const.Con(ty, Idnt.make $1)))
       ts,
     Type.mk_top }
 | CON
   { Term.mk_const (Const.Con(Type.mk_unknown, Idnt.make $1)),
     Type.mk_top }
 | MINUS term %prec UNARY
   { IntTerm.neg (fst $2), Type.mk_int }
 | term PLUS term
   { IntTerm.add (fst $1) (fst $3), Type.mk_int }
 | term TIMES term
   { IntTerm.mul (fst $1) (fst $3), Type.mk_int }
 | term MINUS term
   { IntTerm.sub (fst $1) (fst $3), Type.mk_int }
 | term DIV term
   { IntTerm.div (fst $1) (fst $3), Type.mk_int }
 | term EQ term
   { Formula.eq (Type.meet (snd $1) (snd $3)) (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term NOTEQ term
   { Formula.neq (Type.meet (snd $1) (snd $3)) (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term GT term
   { IntFormula.gt (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term LT term
   { IntFormula.lt (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term LEQ term
   { IntFormula.leq (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term GEQ term
   { IntFormula.geq (fst $1) (fst $3) |> Formula.term_of,
     Type.mk_bool }
 | term AND term
   { ($1, $3) 
     |> Pair.lift (fst >> Formula.of_term)
     |> uncurry2 Formula.mk_and
     |> Formula.term_of,
     Type.mk_bool }
 | term OR term
   { ($1, $3) 
     |> Pair.lift (fst >> Formula.of_term)
     |> uncurry2 Formula.mk_or
     |> Formula.term_of,
     Type.mk_bool }
 | NOT term
   { $2 |> fst |> Formula.of_term
        |> Formula.bnot
        |> Formula.term_of,
     Type.mk_bool }
 | LPAREN term RPAREN { $2 }

terms:
 | term { [$1] }
 | term COMMA terms { $1 :: $3 }
