%{
open Type
open Syntax

let print_error_information () =

  let st =try
 Parsing.symbol_start_pos ()
with _ ->
 Parsing.symbol_start ();
  assert false
  in
(*
  let en = Parsing.symbol_end_pos () in
*)

  Format.printf "File \"%s\", line %d, characters %d-:\n"
    st.Lexing.pos_fname
    st.Lexing.pos_lnum
    (st.Lexing.pos_cnum - st.Lexing.pos_bol)
(*    (en.Lexing.pos_cnum - en.Lexing.pos_bol)*)

let parse_error _ = print_error_information ()

let make_tmp_id s = Id.make 0 s typ_unknown
let make_id_typ s typ = Id.make 0 s typ
let make_self_id typ = Id.new_var "x" typ
let orig_id x = {x with Id.id = 0}
%}

%token <string> IDENT
%token <int> INT
%token LPAREN
%token RPAREN
%token LSQUAR
%token RSQUAR
%token ARROW
%token SEMI
%token COLON
%token INLINE
%token INLINEF
%token TUNIT
%token TBOOL
%token TINT
%token LIST
%token EQUAL
%token LTHAN
%token GTHAN
%token LEQ
%token GEQ
%token OR
%token AND
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token VAL
%token EOF

/* priority : low -> high */
%right ARROW
%left OR
%left AND
%nonassoc NOT
%nonassoc EQUAL LTHAN GTHAN LEQ GEQ
%left PLUS MINUS
%left TIMES
%left LIST



%start spec
%type <Spec.spec> spec

%%

exp:
  id
  { make_var $1 }
| LPAREN exp RPAREN
  { $2 }
| INT
  { make_int $1 }
| MINUS exp
  { make_sub (make_int 0) $2 }
| exp EQUAL exp
  { make_eq $1 $3 }
| exp LTHAN exp
  { make_lt $1 $3 }
| exp GTHAN exp
  { make_gt $1 $3 }
| exp LEQ exp
  { make_leq $1 $3 }
| exp GEQ exp
  { make_geq $1 $3 }
| exp AND exp
  { make_and $1 $3 }
| exp OR exp
  { make_or $1 $3 }
| exp PLUS exp
  { make_add $1 $3 }
| exp MINUS exp
  { make_sub $1 $3 }
| exp TIMES exp
  { make_sub $1 $3 }
| NOT exp
  { make_not $2 }
| id id /* for length */
  {
    if (Id.name $1 <> "length") then raise Parse_error;
    make_app (make_var length_var) [make_var $2]
  }


id:
| IDENT { make_tmp_id $1 }

spec:
  spec_list EOF { $1 }

spec_list:
  { Spec.init }
| typedef spec_list
  { {$2 with Spec.abst_env = $1::$2.Spec.abst_env} }
| inline spec_list
  { {$2 with Spec.inlined = $1::$2.Spec.inlined} }
| inlinef spec_list
  { {$2 with Spec.inlined_f = $1::$2.Spec.inlined_f} }

typedef:
| VAL id COLON typ
  { $2, Id.typ $4 }

inline:
| INLINE id
  { $2 }

inlinef:
| INLINEF id
  { $2 }

simple_type_core:
| TUNIT { TUnit }
| TBOOL { TBool }
| TINT { TInt }
| LPAREN typ LIST RPAREN { TList(Id.typ $2) }

id_simple_type:
| simple_type_core { make_self_id $1 }
| simple_type_core LSQUAR pred_list RSQUAR { make_self_id (TPred(make_self_id $1, $3)) }
| id COLON simple_type_core { Id.new_var (Id.name $1) $3 }
| id COLON simple_type_core LSQUAR pred_list RSQUAR
  {
    let x = $1 in
    let typ = $3 in
    let ps = $5 in
    let x' = Id.new_var (Id.name x) typ in
    let ps' = List.map (subst x (make_var (Id.set_typ x' (elim_tpred typ)))) ps in
      Id.new_var (Id.name x) (TPred(x', ps'))
  }

typ:
| LPAREN typ RPAREN { $2 }
| id_simple_type { $1 }
| typ TIMES typ { make_self_id (TPair($1, Id.typ $3)) }
| typ ARROW typ
  {
    let x = $1 in
    let r = $3 in
    let typ1 = Id.typ x in
    let typ2 = Id.typ r in
    let typ2' = subst_type (orig_id x) (make_var (Id.set_typ x (elim_tpred typ1))) typ2 in
    let typ2'' = subst_type r (make_var (Id.set_typ abst_var (elim_tpred typ2))) typ2' in
      make_self_id (TFun(x, typ2''))
  }
| typ LIST
  { make_self_id (TList(Id.typ $1)) }

pred_list:
  { [] }
| exp
  { [$1] }
| exp SEMI pred_list
  { $1::$3 }
