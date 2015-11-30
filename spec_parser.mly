%{
open Type
open Syntax
open Term_util

module RT = Ref_type

let print_error_information () =
  try
    let st = Parsing.symbol_start_pos () in
    let en = Parsing.symbol_end_pos () in
    Format.printf "%s, line %d, characters %d-%d\n"
                  st.Lexing.pos_fname
                  st.Lexing.pos_lnum
                  (st.Lexing.pos_cnum - st.Lexing.pos_bol)
                  (en.Lexing.pos_cnum - en.Lexing.pos_bol)
  with _ -> ()

let parse_error _ = print_error_information ()

let make_tmp_id s = Id.make 0 s typ_unknown
let make_id_typ s typ = Id.make 0 s typ
let make_self_id typ = Id.new_var ~name:"_" typ
let orig_id x = {x with Id.id = 0}

let ref_base b = Ref_type.Base(b, Id.new_var typ_unknown, true_term)
let ref_list typ = RT.List(dummy_var, true_term, dummy_var, true_term, typ)
%}

%token <string> IDENT
%token <string> EVENT
%token <int> INT
%token LPAREN
%token RPAREN
%token LSQUAR
%token RSQUAR
%token LBRACE
%token RBRACE
%token ARROW
%token SEMI
%token COLON
%token COMMA
%token INLINE
%token INLINEF
%token TUNIT
%token TRESULT
%token TBOOL
%token TINT
%token LIST
%token EQUAL
%token LTHAN
%token GTHAN
%token LEQ
%token GEQ
%token NEQ
%token OR
%token AND
%token NOT
%token PLUS
%token MINUS
%token TIMES
%token BAR
%token TYPE
%token VAL
%token VALCPS
%token VALCEGAR
%token EXTERNAL
%token FAIRNESS
%token TRUE
%token FALSE
%token INTER
%token UNION
%token EOF

/* priority : low -> high */
%left UNION
%right ARROW
%left INTER
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
| TRUE
  { true_term }
| FALSE
  { false_term }
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
| exp NEQ exp
  { make_neq $1 $3 }
| exp AND exp
  { make_and $1 $3 }
| exp OR exp
  { make_or $1 $3 }
| exp PLUS exp
  { make_add $1 $3 }
| exp MINUS exp
  { make_sub $1 $3 }
| exp TIMES exp
  { make_mul $1 $3 }
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
| ref_type spec_list
  { {$2 with Spec.ref_env = $1::$2.Spec.ref_env} }
| ext_ref_type spec_list
  { {$2 with Spec.ext_ref_env = $1::$2.Spec.ext_ref_env} }
| typedef spec_list
  { {$2 with Spec.abst_env = $1::$2.Spec.abst_env} }
| typedef_cps spec_list
  { {$2 with Spec.abst_cps_env = $1::$2.Spec.abst_cps_env} }
| typedef_cegar spec_list
  { {$2 with Spec.abst_cegar_env = $1::$2.Spec.abst_cegar_env} }
| inline spec_list
  { {$2 with Spec.inlined = $1::$2.Spec.inlined} }
| inlinef spec_list
  { {$2 with Spec.inlined_f = $1::$2.Spec.inlined_f} }
| fairness spec_list
  { {$2 with Spec.fairness = $1::$2.Spec.fairness} }

ref_type:
| TYPE id COLON ref_typ
  { $2, $4 }

ext_ref_type:
| EXTERNAL id COLON ref_typ
  { $2, $4 }

typedef:
| VAL id COLON typ
  { $2, Id.typ $4 }

typedef_cps:
| VALCPS id COLON typ
  { $2, Id.typ $4 }

typedef_cegar:
| VALCEGAR id COLON typ
  { $2, Id.typ $4 }

fairness:
| FAIRNESS COLON LPAREN EVENT COMMA EVENT RPAREN
  { $4, $6 }

inline:
| INLINE id
  { $2 }

inlinef:
| INLINEF id
  { $2 }

simple_type_core:
| TUNIT { TUnit }
| TRESULT { typ_result }
| TBOOL { TBool }
| TINT { TInt }
| LPAREN typ LIST RPAREN { TList(Id.typ $2) }

id_simple_type:
| simple_type_core { make_self_id $1 }
| simple_type_core LSQUAR pred_list RSQUAR { make_self_id (TPred(make_self_id $1, $3)) }
| id COLON simple_type_core { Id.new_var ~name:(Id.name $1) $3 }
| id COLON simple_type_core LSQUAR pred_list RSQUAR
  {
    let x = $1 in
    let typ = $3 in
    let ps = $5 in
    let x' = Id.new_var ~name:(Id.name x) typ in
    let ps' = List.map (subst_var x @@ Id.set_typ x' @@ elim_tpred typ) ps in
    Id.new_var ~name:(Id.name x) (TPred(x', ps'))
  }

typ:
| LPAREN typ RPAREN { $2 }
| id_simple_type { $1 }
| typ TIMES typ
  {
    let x = $1 in
    let r = $3 in
    let typ1 = Id.typ x in
    let typ2 = Id.typ r in
    let typ2' = subst_type_var (orig_id x) (Id.set_typ x (elim_tpred typ1)) typ2 in
    let typ2'' = subst_type_var r (Id.set_typ abst_var (elim_tpred typ2)) typ2' in
    make_self_id @@ TTuple [x; Id.new_var typ2'']
  }
| typ ARROW typ
  {
    let x = $1 in
    let r = $3 in
    let typ1 = Id.typ x in
    let typ2 = Id.typ r in
    let typ2' = subst_type_var (orig_id x) (Id.set_typ x @@ elim_tpred typ1) typ2 in
    let typ2'' = subst_type_var r (Id.set_typ abst_var @@ elim_tpred typ2) typ2' in
    make_self_id @@ TFun(x, typ2'')
  }
| typ LIST
  { make_self_id @@ TList(Id.typ $1) }

ref_base:
| TUNIT { RT.Unit }
| TBOOL { RT.Bool }
| TINT { RT.Int }

ref_simple:
| ref_base { ref_base $1 }
| LBRACE id COLON ref_base BAR exp RBRACE { RT.Base($4, $2, $6) }
| LPAREN ref_typ RPAREN { $2 }
| ref_simple LIST { RT.List(dummy_var,true_term,dummy_var,true_term,$1) }
| index_ref ref_simple length_ref LIST
  {
    let y,p_i = $1 in
    let typ = $2 in
    let x,p_len = $3 in
    let typ' = RT.subst_var (orig_id x) x typ in
    let p_i' = subst_var (orig_id x) x p_i in
    RT.List(x,p_len,y,p_i',typ')
  }

index_ref:
| LSQUAR id RSQUAR { Id.new_var ~name:(Id.name $2) TInt, true_term }
| LSQUAR id COLON exp RSQUAR
  {
    let x = $2 in
    let x' = Id.new_var ~name:(Id.name x) TInt in
    x', subst_var x x' $4
  }

length_ref:
| BAR id BAR { Id.new_var ~name:(Id.name $2) TInt, true_term }
| BAR id COLON exp BAR
  {
    let x = $2 in
    let x' = Id.new_var ~name:(Id.name x) TInt in
    x', subst_var x x' $4
  }

ref_typ:
| ref_simple { $1 }
| id COLON ref_simple TIMES ref_typ { RT.Tuple[$1, $3; dummy_var, $5] }
| ref_typ TIMES ref_typ
  {
    let x  =
      match $1 with
      | RT.Base(_,y,_) -> y
      | _ -> Id.new_var @@ RT.to_simple $1
    in
    RT.Tuple[x, $1; dummy_var, $3]
  }
| id COLON ref_simple ARROW ref_typ { RT.Fun($1, $3, $5) }
| LPAREN id COLON ref_simple RPAREN ARROW ref_typ { RT.Fun($2, $4, $7) }
| ref_typ ARROW ref_typ
  {
    let x  =
      match $1 with
      | RT.Base(_,y,_) -> y
      | _ -> Id.new_var @@ RT.to_simple $1
    in
    RT.Fun(x, $1, $3)
  }
| ref_typ UNION ref_typ { RT.Union[$1; $3] }
| ref_typ INTER ref_typ { RT.Inter[$1; $3] }

pred_list:
  { [] }
| exp
  { [$1] }
| exp SEMI pred_list
  { $1::$3 }
