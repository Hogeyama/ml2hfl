%{
  open Util
  open Combinator
  open Tag
  open ExtString

  let denv = ref [] (* data type environment *)
  let ctenv = ref [] (* data type constructor's type and type environment *)
  let pvpole = ref [] (* predicate variables' pole *)
  let pvprior = ref [] (* priority constaint *)
  let pvtempl = ref [] (* specify template size *)

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

%token <string> VAR
%token <string> VAR_T
%token <string> CONST
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> RECOGNIZER
%token <string * int> ACCESSOR
%token <string * string> SIZE

%token MAXIMIZE
%token MINIMIZE
%token PRIORITIZE
%token TEMPLATE

%token IPTAG
%token IMTAG
%token OTAG

%token LET
%token IN
%token EQUAL
%token NOTEQUAL
%token COMMA
%token LPAREN // '('
%token RPAREN // ')'
%token MID // '|'

%token PROJ

%token FORALL EXISTS MU NU
%token DOT
%token AND OR IMP NOT
%token LT GT LEQ GEQ
%token ADD SUB AST DIV
%token FADD FSUB FMUL FDIV
%token EQEQ

%token LBRA
%token RBRA
%token COLON // ':'
%token COLCOL // "::"
%token SLASLA // "//"
%token ARROW
%token LARROW
//%token PAIR

%token MEMBER
%token SUBSET
%token SUNION
%token SINTERSECT
%token SDIFF
%token SEMPTY
%token SCOMP

%token LBRACKET
%token RBRACKET

%token QUESTION
%token <Idnt.t> LIDENT
%token <Idnt.t> UIDENT

%token LABRA
%token RABRA
%token ASET
%token AMAKE

%token DATA
%token OF

%token ASSERT

%token COLMIN // ":-"
%token QMIN // "?-"

%token SEMICOLON
%token SEQ
%token TSEQ
%token EOF

%nonassoc LEFT RIGHT
%nonassoc IN COMMA DOT


%right ARROW
%left LARROW
%right prec_pair

%nonassoc PROJ

%nonassoc RECOGNIZER

%right IMP
%left OR
%left AND
%nonassoc NOT
%nonassoc FORALL EXISTS MU NU

%nonassoc ACCESSOR
%nonassoc SIZE

%left EQUAL NOTEQUAL LT GT LEQ GEQ
%left EQEQ

%left SINTERSECT
%left SUNION SDIFF 
%nonassoc SCOMP

%left ADD SUB FADD FSUB
%left AST DIV FMUL FDIV
%nonassoc UNARY
%left ASSERT

%left CONST
%left LBRA LPAREN 
%nonassoc VAR VAR_T INT FLOAT BOOL SEMPTY

%nonassoc MAXIMIZE
%nonassoc MINIMIZE
%nonassoc PRIORITIZE

%start main types hcs_main sizes temp gettag ranks ty wrg lemmas fml prop pred_def value
%type < TypEnv.t * Fdef.t list * RefTypEnv.t * PredVarPoles.t * PredVarPriority.t * (Idnt.t * (int * int)) list * (Idnt.t * TreeAutomaton.t) list * HCCS.t > main
%type <RefTypEnv.t> types
%type <HCCS.t * PredVarPoles.t * PredVarPriority.t * (Idnt.t * (int * int)) list> hcs_main
%type <SizeFun.t> sizes
%type <Formula.t> temp
%type <TypEnv.t * Fdef.t list * RefTypEnv.t * HCCS.t * (Idnt.t * Tag.t list) list> gettag
%type <RankFun.t> ranks
%type <RefTyp.t> ty
%type <PredSubst.t * Idnt.t list * (Idnt.t * Idnt.t) list> wrg
%type <(PredVar.t list * (PredVar.t * int) option * Formula.t * ProofTree.h) list> lemmas
%type <Formula.t list * (Term.t option * Term.t option)> fml
%type <Formula.t> prop
%type <PredSubst.elem> pred_def
%type <Term.t> value
%%

main:
 | datas exp TSEQ types TSEQ hcs TSEQ regs EOF { 
   let hcs =
     $6
     |> HCCS.map_phi
       (Formula.term_of
        >> ADTFormula.typing_term !ctenv
        >> Formula.of_term)
   in
   let (r, _, _) =
     List.fold_left
       (fun x y -> 
          let (r, wl, wl') = x in
          let (t, wl, wl') = RegTreeExp.to_ta $8 (snd y) wl wl' [] in
          (((fst y), t) :: r, wl, wl'))
       ([], [], []) $8
   in
   (!ctenv, $2, RefTypEnv.alpha [] $4, !pvpole, !pvprior, !pvtempl, r, hcs)
 }

  | datas exp TSEQ types TSEQ hcs EOF {
      let hcs =
        $6
        |> HCCS.map_phi
          (Formula.term_of
           >> ADTFormula.typing_term !ctenv
           >> Formula.of_term)
      in
      (!ctenv, $2, RefTypEnv.alpha [] $4, !pvpole, !pvprior, !pvtempl, [], hcs)
    }

 | exp TSEQ types TSEQ hcs EOF {
     (!ctenv, $1, RefTypEnv.alpha [] $3, !pvpole, !pvprior, !pvtempl, [], $5)
   }

 | datas exp TSEQ types EOF { 
     (!ctenv, $2, RefTypEnv.alpha [] $4, !pvpole, !pvprior, !pvtempl, [], [])
   }

 | exp TSEQ types EOF { 
     (!ctenv, $1, RefTypEnv.alpha [] $3, !pvpole, !pvprior, !pvtempl, [], [])
   }

 | error {
     Printer.print_parser_error_information ();
     raise (Global.Syntax_error "Parse error")
   }
;

exp:
 | expr { [$1] }
 | expr SEQ { [$1] }
 | expr SEQ exp { $1 :: $3 }
;

expr:
 | VAR patterns MID prop MID /* to avoid conflics */ EQUAL term { Fdef.make $1 $2 $4 $7 }
 | VAR patterns EQUAL term { Fdef.make $1 $2 Formula.mk_true $4  }
;

value:
 /* this causes reduce/reduce conflict on RPAREN
 | BOOL { BoolTerm.make $1 } */
 | INT { IntTerm.make $1 }
 | FLOAT { RealTerm.make $1 }
 | VAR {
       if String.starts_with $1 "?" then
         Term.mk_var (Idnt.mk_coeff $1) (* (user defined) coefficient variables (for size abstractions) *)
       else Term.var_of_string $1 (* ordinary variables *)
 }
 | VAR_T /* e.g. foo[0:0] (@todo support foo[0:0][0:0]) */{
     Term.mk_var (mk_idnt_t $1)
   }
 | LPAREN VAR ids RPAREN {
     CunTerm.mk_ufun
       (Idnt.make $2, Type.mk_unknown)
       (List.map Term.mk_var $3)
   }
 | CONST LPAREN values RPAREN {
     let x = Idnt.make $1 in
     ADTTerm.mk_kon (x, ADTFormula.lookup x !ctenv) $3
   }
 | CONST { 
     let x = Idnt.make $1 in
     ADTTerm.mk_kon (x, ADTFormula.lookup x !ctenv) []
   }
 | LPAREN values RPAREN { TupTerm.mk_tuple2 $2 }
 | SUB value %prec UNARY { IntTerm.neg $2 }
 | value ADD value { IntTerm.add $1 $3 }
 | value SUB value { IntTerm.sub $1 $3 }
 | value AST value { IntTerm.mul $1 $3 }
 | value DIV value { IntTerm.div $1 $3 }
 | value FADD value { RealTerm.add $1 $3 }
 | value FSUB value { RealTerm.sub $1 $3 }
 | value FMUL value { RealTerm.mul $1 $3 }
 | value FDIV value { RealTerm.div $1 $3 }
 | value EQEQ value { Formula.term_of (Formula.eq Type.mk_int $1 $3) }
 | ACCESSOR value {
     let id,n = $1 in
     ADTTerm.mk_accessor 
       (ADTFormula.lookup (Idnt.make ("_get_"^id^"_"^string_of_int n)) !ctenv)
       (Idnt.make id) n $2
   }
 | SEMPTY { SetTerm.mk_empty (Type.mk_unknown) }
 | LBRA values RBRA {
     List.fold_left
       (flip (SetTerm.mk_add Type.mk_unknown))
       (SetTerm.mk_empty Type.mk_unknown)
       $2
   }
 | value SUNION value { SetTerm.mk_union Type.mk_unknown $1 $3 }
 | value SINTERSECT value { SetTerm.mk_intersect Type.mk_unknown $1 $3 }
 | value SDIFF value { SetTerm.mk_diff Type.mk_unknown $1 $3 }
 | SCOMP value { SetTerm.mk_comp Type.mk_unknown $2 }
 | PROJ LPAREN INT COMMA value RPAREN {
     TupTerm.mk_proj [] $3 $5
 }
 | SIZE value {
     let size_var = (fst $1) ^ "_" ^ (snd $1) in
     CunTerm.mk_ufun
       (Idnt.make size_var, Type.mk_fun [Type.mk_unknown; Type.mk_int])
       [$2]
   }

;

values:
 | value { [$1] }
 | value COMMA values { $1 :: $3 }
;

term:
 | value { $1 }
 | term value { MLExp.mk_app $1 [$2] }
 | LET VAR EQUAL term IN term { 
     MLExp.mk_let
       Type.mk_unknown
       (Idnt.make $2, Type.mk_unknown)
       $4
       $6
   }
 | ASSERT term { MLExp.mk_assert $2 }
 | LABRA values RABRA {
     ArrayTerm.mk_array $2
   }
 | ASET VAR DOT LPAREN value RPAREN LARROW value IN term {
     ArrayTerm.mk_aset (Term.var_of_string $2) $5 $8 $10
   }
 | VAR DOT LPAREN value RPAREN {
     ArrayTerm.mk_aget (Term.var_of_string $1) $4
   }
;


patterns:
 | pattern { [$1] }
 | pattern patterns { $1::$2 }
;
pcomma:
 | pattern_aux { [$1] }
 | pattern_aux COMMA pcomma { $1 :: $3 }
;

pattern:
 | CONST { Pattern.K (Idnt.make $1, Pattern.U) }
 | VAR { Pattern.V (Idnt.make $1) }
 | LPAREN pcomma RPAREN { Pattern.of_list $2 }
;

pattern_aux:
 | pattern { $1 }
 | CONST pattern { Pattern.K (Idnt.make $1, $2) }
;

ids:
 | VAR { [Idnt.make $1] }
 | VAR ids { Idnt.make $1 :: $2 }
;

prop:
  | LPAREN prop RPAREN { $2 }
  | atom { $1 }
// | BOOL { if $1 then Formula.mk_true else Formula.mk_false }
  | NOT prop { Formula.bnot $2 }
  | prop AND prop { Formula.band [$1; $3] }
  | prop OR prop { Formula.bor [$1; $3] }
  | prop IMP prop { Formula.imply $1 $3 }
  | LBRACKET VAR RBRACKET prop { Formula.box $2 $4 }
  | LT VAR GT prop { Formula.diamond $2 $4 }
  | MU VAR DOT prop { Formula.mu (Idnt.make $2) $4 }
  | NU VAR DOT prop { Formula.nu (Idnt.make $2) $4 }
  | FORALL ids DOT prop {
    Formula.forall (List.map (flip Pair.make Type.mk_unknown) $2) $4
  }
  | EXISTS ids DOT prop {
    Formula.exists (List.map (flip Pair.make Type.mk_unknown) $2) $4
  }
;

temp:
  | LPAREN temp RPAREN { $2 }
  | VAR { Idnt.make $1 |> flip Formula.mk_var []}
  | BOOL { if $1 then Formula.mk_true else Formula.mk_false }
  | NOT temp { Formula.bnot $2 }
  | temp AND temp { Formula.band [$1;$3] }
  | temp OR temp { Formula.bor [$1;$3] }
  | temp IMP temp { Formula.imply $1 $3 }
  | FORALL ids DOT temp {
    Formula.forall (List.map (flip Pair.make Type.mk_unknown) $2) $4
  }
  | EXISTS ids DOT temp {
    Formula.exists (List.map (flip Pair.make Type.mk_unknown) $2) $4
  }
;

gettag:
  | exp TSEQ types TSEQ tags EOF {
    let tenv, hccs =
      SimTypInfer.infer_hccs !ctenv []
    in
    (tenv, $1, RefTypEnv.alpha [] $3, hccs, $5)
  }
;

tags:
  | tag { $1 }
  | tag tags { $1@$2 }
;

tag:
  | SLASLA VAR LPAREN tgs RPAREN { [Idnt.make $2, $4] }
;

tgs:
  | tg { [$1] }
  | tg COMMA tgs { $1 :: $3 }
;

tg:
  | IPTAG { Ip }
  | IMTAG { Im }
  | OTAG  { O }
;

atoms:
  | atom { [$1] }
  | atom AND atoms { $1::$3 }
;

atom:
  | BOOL { if $1 then Formula.mk_true else Formula.mk_false }
  | VAR LPAREN RPAREN /* to avoid reduce/reduce conflict on RPAREN */ {
    Term.mk_var (Idnt.make $1)
    |> Formula.of_term
  }
  | VAR LPAREN values RPAREN { 
      Pva.make
        (Idnt.make $1)
        (List.map (flip Pair.make Type.mk_unknown) $3)
      |> Pva.to_formula
    }
  | VAR_T LPAREN values RPAREN {
      Pva.make
        (mk_idnt_t $1)
        (List.map (flip Pair.make Type.mk_unknown) $3)
      |> Pva.to_formula

    }
  | atom_or_var EQUAL    atom_or_var { Formula.eq Type.mk_unknown $1 $3 }
  | atom_or_var NOTEQUAL atom_or_var { Formula.neq Type.mk_unknown $1 $3 }
//  | value EQUAL value { Formula.eq Type.mk_unknown $1 $3 }
//  | value NOTEQUAL value { Formula.neq Type.mk_unknown $1 $3 }
  | value LT value { NumFormula.lt Type.mk_unknown $1 $3 }
  | value GT value { NumFormula.gt Type.mk_unknown $1 $3 }
  | value LEQ value { NumFormula.leq Type.mk_unknown $1 $3 }
  | value GEQ value { NumFormula.geq Type.mk_unknown $1 $3 }
  | value IN value { SetFormula.mk_mem Type.mk_unknown $1 $3 }
  | RECOGNIZER value {
      ADTFormula.mk_recognizer
        (ADTFormula.lookup (Idnt.make ("_is_"^$1)) !ctenv)
        (Idnt.make $1)
      $2
    }
  | value SUBSET value { SetFormula.mk_subset Type.mk_unknown $1 $3 }
  | value IN treeautomaton {
      TreeAutomatonFormula.mem $1 (TreeAutomatonTerm.make $3)
    }
;

atom_or_var:
  | atom { $1 |> Formula.term_of }
  | value { $1 }
  ;

treeautomaton:
  | regtreeexp {
      let (t, _, _) = RegTreeExp.to_ta [] $1 [] [] [] in
      t
  }
;

regtreeexp:
  | LPAREN RPAREN { RegTreeExp.Nil }
  | LIDENT { RegTreeExp.Var($1) }
  | regtreeexp COMMA regtreeexp { RegTreeExp.Concat($1, $3) }
  | UIDENT LBRACKET RBRACKET { RegTreeExp.Label($1, RegTreeExp.Nil) }
  | UIDENT LBRACKET regtreeexp RBRACKET { RegTreeExp.Label($1, $3) }
  | regtreeexp MID regtreeexp { RegTreeExp.Alter($1, $3) }
  | regtreeexp AST { RegTreeExp.Kleene($1) }
  | regtreeexp QUESTION { RegTreeExp.Option($1) }
  | LPAREN regtreeexp RPAREN { $2 }
;

pvar:
 | VAR { Idnt.make $1 }
 | VAR_T { mk_idnt_t $1 }
 | CONST { Idnt.make $1 }
;
pvcstr:
 | MAXIMIZE LPAREN pvar RPAREN DOT {
     pvpole := ($3, false) :: !pvpole; []
   }
 | MINIMIZE LPAREN pvar RPAREN DOT {
     pvpole := ($3, true) :: !pvpole; []
   }
 | PRIORITIZE LPAREN pvar COMMA VAR RPAREN DOT {
     pvprior := ($3, Idnt.make $5) :: !pvprior; []
   }
 | TEMPLATE LPAREN pvar COMMA INT RPAREN DOT {
     pvtempl := ($3, ($5, 1)) :: !pvtempl; []
   }
 | TEMPLATE LPAREN pvar COMMA INT COMMA INT RPAREN DOT {
     pvtempl := ($3, ($5, $7)) :: !pvtempl; []
   }
 | TEMPLATE LPAREN INT COMMA INT RPAREN DOT {
     Template.num_conj := $3;
     Template.num_disj := $5; []
   }
// | WF LPAREN VAR RPAREN DOT {}
// | DWF LPAREN VAR RPAREN DOT {}

;

hcs_main:
 | hcs { ($1, !pvpole, !pvprior, !pvtempl) }
 | error {
     Printer.print_parser_error_information ();
     raise (Global.Syntax_error "Parse error (hcs_main)")
     }

hcs:
 | pvcstr { $1 }
 | hcs pvcstr { $1 @ $2 }
 | hc { [$1] }
 | hcs hc { $1 @ [$2] }
; 

hc:
  | VAR LPAREN pcomma RPAREN DOT {
    let tl = (* get type of Head *)
      List.map
        (fun p ->
           let x =
             match p with
             | Pattern.V v -> v
             | Pattern.K (v, tl) -> v
           in
           ADTFormula.lookup x !ctenv |> Type.ret_of)
        $3
    in
    let vl = List.map (fun _ -> Idnt.new_var ()) $3 in
    (* generate new variables for arguments of Head *)
    let formula_recognizers =
      List.concat_map2
        ADTFormula.mk_recognizers_wo_typing
        $3
        (List.map Term.mk_var vl)
    in
    (* let fs = (\* make formulas for recognizers *\) *)
    (*   List.concat_map2 *)
    (*     mk_recognizers *)
    (*     $3 *)
    (*     (List.map Term.mk_var vl) *)
    (* in *)
    HornClause.mk_def
      (PredVar.make (Idnt.make $1) (List.combine vl tl))
      []
      (Formula.band formula_recognizers)
  }
  | VAR LPAREN pcomma RPAREN COLMIN atoms DOT {
    let pvars, f = List.partition (Formula.is_pva (List.concat_map Formula.fvs $6)) $6 in
    let tl =
      List.map
        (fun p ->
           let x =
             match p with
             | Pattern.V v -> v
             | Pattern.K (v, tl) -> v
           in
           ADTFormula.lookup x !ctenv |> Type.ret_of)
        $3
    in
    let vl = List.map (fun _ -> Idnt.new_var ()) $3 in
    let formula_recognizers =
      List.concat_map2
        ADTFormula.mk_recognizers_wo_typing
        $3
        (List.map Term.mk_var vl)
    in
    let subst_accessors =
      List.concat_map2
        ADTFormula.mk_accessors_wo_typing
        $3
        (List.map Term.mk_var vl)
    in
    (* let fs = *)
    (*   List.concat_map2 *)
    (*     mk_recognizers *)
    (*     $3 *)
    (*     (List.map Term.mk_var vl) *)
    (* in *)
    (* let subst = *)
    (*   List.concat_map2 *)
    (*     mk_accessors *)
    (*     $3 *)
    (*     (List.map Term.mk_var vl) *)
    (* in *)
    HornClause.mk_def
      (PredVar.make (Idnt.make $1) (List.combine vl tl))
      (pvars
       |> List.map (Formula.subst subst_accessors >> Pva.of_formula))
      (formula_recognizers
       |> (@) f
       |> Formula.band
       |> Formula.subst subst_accessors)
      (* (List.map Formula.pva_of (List.map (Formula.subst subst) pvars)) *)
      (* (Formula.band (fs @ f) *)
      (*  |> Formula.subst subst) *)
  }
  | CONST LPAREN pcomma RPAREN COLMIN atoms DOT {
    let pvars, f = List.partition (Formula.is_pva (List.concat_map Formula.fvs $6)) $6 in
    let tl =
      List.map
        (fun p ->
           let x =
             match p with
             | Pattern.V v -> v
             | Pattern.K (v, tl) -> v
           in
           ADTFormula.lookup x !ctenv |> Type.ret_of)
        $3
    in
    let vl = List.map (fun _ -> Idnt.new_var ()) $3 in
    let formula_recognizers =
      List.concat_map2
        ADTFormula.mk_recognizers_wo_typing
        $3
        (List.map Term.mk_var vl)
    in
    let subst_accessors =
      List.concat_map2
        ADTFormula.mk_accessors_wo_typing
        $3
        (List.map Term.mk_var vl)
    in
    (* let fs = *)
    (*   List.concat_map2 *)
    (*     mk_recognizers *)
    (*     $3 *)
    (*     (List.map Term.mk_var vl) *)
    (* in *)
    (* let subst = *)
    (*   List.concat_map2 *)
    (*     mk_accessors *)
    (*     $3 *)
    (*     (List.map Term.mk_var vl) *)
    (* in *)
    HornClause.mk_def
      (PredVar.make (Idnt.make $1) (List.combine vl tl))
      (pvars
       |> List.map (Formula.subst subst_accessors >> Pva.of_formula))
      (formula_recognizers
       |> (@) f
       |> Formula.band
       |> Formula.subst subst_accessors)
      (* (List.map Formula.pva_of (List.map (Formula.subst subst) pvars)) *)
      (* (Formula.band (fs @ f) *)
      (*  |> Formula.subst subst) *)
  }
  | EXISTS pcomma DOT VAR LPAREN pcomma RPAREN DOT {
    let tl = (* get type of Head *)
      List.map
        (fun p -> Type.mk_int) (* @todo *)
      (* let x = *)
      (*   match p with *)
      (*   | Pattern.V v -> v *)
      (*   | Pattern.K (v, tl) -> v *)
      (* in *)
      (* ADTFormula.lookup x !ctenv |> Type.ret_of ) *)
        $6
    in
    let vl =
      List.map (fun _ -> Idnt.new_var ()) $6
    in (* generate new variables for arguments of Head *)
    let vtl = List.combine vl tl in
    let env_map = List.combine $6 vtl in
    Format.printf
      "$2: %a@.$6: %a@."
      (List.pr Pattern.pr ", ") $2
      (List.pr Pattern.pr ", ") $6;
    let tenv = List.map (fun id -> List.assoc id env_map) $2 in
    let formula_recognizers =
      List.concat_map2
        ADTFormula.mk_recognizers_wo_typing
        $6
        (List.map Term.mk_var vl)
    in
    let hc =
      HornClause.mk_def
        ~tenv:tenv
        (PredVar.make (Idnt.make $4) vtl)
        []
        (Formula.band formula_recognizers)
    in
    Format.printf "parsed: %a@." HornClause.pr hc;
    hc
  }
  | QMIN atoms DOT {
    HornClause.of_formulas (List.concat_map Formula.fvs $2) Formula.mk_false $2
  }
;

types:
  | typ { $1 }
  | typ types { $1 @ $2 }
  | error {
    Printer.print_parser_error_information ();
    raise (Global.Syntax_error "Parse error")
  }

;

typ:
  | VAR COLCOL ty { RefTypEnv.of_rty_env [Idnt.make $1, $3] }
  | CONST COLCOL ty { RefTypEnv.of_rty_env [Idnt.make $1, $3] }
;

ty:
  | VAR {
    ADTFormula.type_of_string $1 !denv
    |> RefTyp.of_simple_type
  }
  | LBRA VAR COLON VAR MID prop RBRA {
    let ty = ADTFormula.type_of_string $4 !denv in
    ignore (Type.base_or_adt_of ty);
    RefTyp.mk_base (Idnt.make $2) ty $6
  }
  | ty ARROW ty {
    RefTyp.mk_fun [Idnt.new_var (), $1; Idnt.new_var (), $3]
  }
  | LPAREN VAR COLON ty RPAREN ARROW ty {
    RefTyp.mk_fun [Idnt.make $2, $4; Idnt.new_var (), $7]
  }
  | ty AST ty
    %prec prec_pair {
    match $3 with
    | RefTyp.Tuple (tl, t) ->
      RefTyp.Tuple ((Idnt.new_var (), $1) :: tl, t)
    | _ ->
      RefTyp.Tuple ([Idnt.new_var (), $1], $3)
  }
  | LPAREN VAR COLON ty RPAREN AST ty
    %prec prec_pair {
    match $7 with
    | RefTyp.Tuple (tl, t) ->
      RefTyp.Tuple ((Idnt.make $2, $4) :: tl, t)
    | _ ->
      RefTyp.Tuple ([Idnt.make $2, $4], $7)
  }
;


regs:
  | reg {
    [$1]
  }
  | reg regs {
    [$1] @ $2
  }
;

reg:
  | LIDENT EQUAL regtreeexp {
    ($1, $3)
  }

datas:
  | data {
    let (x, tenv) = $1 in
    denv := (x, tenv |> List.hd |> snd |> Type.ret_of) :: !denv;
    ctenv := tenv @ ADTFormula.mk_accessors_recognizers tenv @ !ctenv
  }
  | data datas {
    let (x, tenv) = $1 in
    denv := (x, tenv |> List.hd |> snd |> Type.ret_of) :: !denv;
    ctenv := tenv @ ADTFormula.mk_accessors_recognizers tenv @ !ctenv;
    $2
  }
;

data:
  | DATA VAR EQUAL bodies {
    (Idnt.make $2,
     let tyc = Type.mk_adt (Idnt.make $2) (List.map fst $4) in
     List.map
       (fun (cname, bd) ->
          (cname,
           bd
           |> List.map
             (fun x ->
                if x = $2 then
                  tyc
                else
                  ADTFormula.type_of_string x !denv)
           |> flip Type.mk_fun_args_ret tyc))
     $4)
  }
 ;

bodies:
  | body { [$1] }
  | body MID bodies { $1 :: $3 }
;

body:
  | CONST { (Idnt.make $1, []) }
  | CONST OF ptypes { (Idnt.make $1, $3) }
;

ptypes:
  | ptype { [$1] }
  | ptype AST ptypes { $1 :: $3 }
;
ptype:
  | VAR { $1 }
;

sizes:
  | size { [$1] }
  | size sizes { $1 :: $2 }
;

size:
  | CONST EQUAL size_defs { (Idnt.make $1, $3) }
  | error {
    Printer.print_parser_error_information ();
    raise (Global.Syntax_error "Parse error")
  }
;

size_defs:
  | size_def { [$1] }
  | size_def MID size_defs { $1 :: $3 }
;

size_def:
  | CONST COLON ids_comma DOT value { (Idnt.make $1, ($3, $5)) }
  | CONST COLON value { (Idnt.make $1, ([], $3)) }
;

ids_comma:
  | VAR { [Idnt.make $1] }
  | VAR COMMA ids_comma { (Idnt.make $1) :: $3 }
;

ranks:
  | rank { [$1] }
  | rank ranks { $1 :: $2 }
  | error {
    Printer.print_parser_error_information ();
    raise (Global.Syntax_error "Parse error")
  }
;

rank:
  | VAR LPAREN ids_comma RPAREN COLON rank_elems { RankFun.Rank (Idnt.make $1, ($3, $6)) }
  | VAR LPAREN ids_comma RPAREN EQUAL rank_elems { RankFun.Rank (Idnt.make $1, ($3, $6)) }
  | VAR LBRACKET INT RBRACKET LPAREN ids_comma RPAREN COLON rank_elems {
    RankFun.Rank_game ($3, Idnt.make $1, ($6, $9))
  }
  | VAR LBRACKET INT RBRACKET LPAREN ids_comma RPAREN EQUAL rank_elems {
    RankFun.Rank_game ($3, Idnt.make $1, ($6, $9))
  }
;

rank_elems:
  | value { [$1] }
  | value COMMA rank_elems { $1 :: $3 }
;

pred_def_list:
  | pred_def { [$1] }
  | pred_def COMMA pred_def_list { $1 :: $3 }
;

pred_def:
  | VAR LPAREN tenv RPAREN EQUAL prop { (Idnt.make $1, ($3, $6)) }
;

tenv:
  | VAR { [Idnt.make $1, Type.mk_unknown] }
  | VAR COLON sty { [Idnt.make $1, $3] }
  | VAR COMMA tenv { (Idnt.make $1, Type.mk_unknown) :: $3 }
  | VAR COLON sty COMMA tenv { (Idnt.make $1, $3) :: $5 }
;

sty:
  | VAR { ADTFormula.type_of_string $1 !denv }
  | sty ARROW sty { Type.mk_fun [$1; $3] }
  | sty AST sty %prec prec_pair { Type.mk_tuple [$1; $3] }
;

ord_def_list:
  | ord_def { [$1] }
  | ord_def COMMA ord_def_list { $1 :: $3 }
;

ord_def:
  | VAR LEQ VAR { (Idnt.make $1, Idnt.make $3) }
  | VAR GEQ VAR { (Idnt.make $3, Idnt.make $1) }
;

// weak recurrence games
wrg:
  | pred_def_list SEMICOLON ids_comma SEMICOLON ord_def_list {
    ($1, $3, $5)
  }
;

pvfs:
  | pvf { [$1] }
  | pvf AND pvfs { $1 :: $3 }
;

pvf:
 | VAR LPAREN values RPAREN { 
     Pva.make
       (Idnt.make $1)
       (List.map (flip Pair.make Type.mk_unknown) $3)
     |> Pva.pvar_of
   }
 | VAR_T LPAREN values RPAREN {
     let [s1;s2;s3] = Str.split (Str.regexp "['[' ':']") $1 in
     let i2, i3 =
       String.sub s3 0 (String.rindex s3 ']')
       |> Pair.make s2
       |> Pair.lift int_of_string
     in
     Pva.make
       (Idnt.ret_args (Idnt.make s1) i2 i3 |> fst)
       (List.map (flip Pair.make Type.mk_unknown) $3)
     |> Pva.pvar_of
   }
;

h:
 | VAR LPAREN values RPAREN { 
     Pva.make
       (Idnt.make $1)
       (List.map (flip Pair.make Type.mk_unknown) $3)
     |> ProofTree.mk_hP
   }
 | VAR_T LPAREN values RPAREN {
     let [s1;s2;s3] = Str.split (Str.regexp "['[' ':']") $1 in
     let i2, i3 =
       String.sub s3 0 (String.rindex s3 ']')
       |> Pair.make s2
       |> Pair.lift int_of_string
     in
     Pva.make
       (Idnt.ret_args (Idnt.make s1) i2 i3 |> fst)
       (List.map (flip Pair.make Type.mk_unknown) $3)
     |> ProofTree.mk_hP
   }
 | BOOL { ProofTree.mk_hB }
;

lemmas:
  | lemma { [$1] }
  | lemma lemmas { $1 :: $2 }
  | error {
    Printer.print_parser_error_information ();
    raise (Global.Syntax_error "Parse error")
  }
;

lemma:
  | pvfs IMP h {
    let pvs, fs = List.split $1 in
    pvs, None, Formula.band fs, $3
  }
  | pvfs COMMA atoms IMP h {
    let pvs, fs = List.split $1 in
    pvs, None, Formula.band (fs@$3), $5
  }

// formula inputs (.fml) for testing interpolation or something
fml:
  | props { ($1, (None, None)) }
  | props TSEQ fml_opt { ($1, $3) }
  | error {
    Printer.print_parser_error_information ();
    raise (Global.Syntax_error "Parse error")
  }
;
props:
  | prop DOT { [$1] }
  | prop DOT props { $1 :: $3 }
;

fml_opt: // @todo support multi-objective optimization
  |  { (None, None) }
  | MAXIMIZE LPAREN value RPAREN DOT { (Some($3), None) }
  | MINIMIZE LPAREN value RPAREN DOT { (None, Some($3)) }
