open Util
open Syntax
open Type

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let get_fv = Syntax.get_fv

let occur_in x t =
  Id.mem x @@ get_fv t

(*** TERM CONSTRUCTORS ***)

let typ_result = TData "X"
let typ_event = Ty.(fun_ unit unit)
let typ_event' = Ty.(fun_ unit typ_result)
let typ_event_cps = Ty.(funs [unit; fun_ unit typ_result] typ_result)
let typ_exn = TData "exn"

let abst_var = Id.make (-1) "v" [] typ_unknown
let abst_var_int = Id.set_typ abst_var Ty.int
let abst_var_bool = Id.set_typ abst_var Ty.bool

let make_attr ?(attrs=const_attr) ts =
  let check a = List.for_all (fun {attr} -> List.mem a attr) ts in
  let make a = if check a then Some a else None in
  List.filter_map make attrs

let rec is_value t =
  match t.desc with
  | Const _ -> true
  | Var _ -> true
  | Fun _ -> true
  | Nil -> true
  | Cons(t1,t2) -> is_value t1 && is_value t2
  | Tuple ts -> List.for_all is_value ts
  | _ -> false

let end_of_definitions = {desc=Const End_of_definitions; typ=Ty.unit; attr=[]}
let unit_term = {desc=Const Unit; typ=Ty.unit; attr=const_attr}
let true_term = {desc=Const True; typ=Ty.bool; attr=const_attr}
let false_term = {desc=Const False; typ=Ty.bool; attr=const_attr}
let cps_result = {desc=Const CPS_result; typ=typ_result; attr=const_attr}
let fail_term = {desc=Event("fail",false); typ=typ_event; attr=[]}
let fail_term_cps = {desc=Event("fail",true); typ=typ_event_cps; attr=[]}
let make_bool b = if b then true_term else false_term
let make_bottom typ = {desc=Bottom; typ=typ; attr=[]}
let make_event s = {desc=Event(s,false); typ=typ_event; attr=[]}
let make_event_cps s = {desc=Event(s,true); typ=typ_event_cps; attr=[]}
let make_var x = {desc=Var x; typ=Id.typ x; attr=const_attr}
let make_int n = {desc=Const(Int n); typ=Ty.int; attr=const_attr}
let make_string s = {desc=Const(String s); typ=TData "string"; attr=const_attr}
let rec make_app t ts =
  let check typ1 typ2 =
    if not (Flag.Debug.check_typ => Type.can_unify typ1 typ2) then
      begin
        Format.eprintf "make_app:@[@ %a@ <=/=>@ %a@." Print.typ typ1 Print.typ typ2;
        Format.eprintf "fun: %a@." Print.term t;
        Format.eprintf "arg: %a@." Print.term @@ List.hd ts;
        assert false
      end
  in
  match t.desc, tfuns_to_tfun @@ elim_tattr t.typ, ts with
  | _, _, [] -> t
  | App(t1,ts1), TFun(x,typ), t2::ts2 ->
      check (Id.typ x) t2.typ;
      make_app {desc=App(t1,ts1@[t2]); typ; attr=[]} ts2
  | App(t1,ts1), typ, t2::ts2 when typ = typ_unknown || is_tvar typ ->
      make_app {desc=App(t1,ts1@[t2]); typ; attr=[]} ts2
  | _, TFun(x,typ), t2::ts ->
      check (Id.typ x) t2.typ;
      make_app {desc=App(t,[t2]); typ; attr=[]} ts
  | _, typ, t2::ts when typ = typ_unknown || is_tvar typ ->
      make_app {desc=App(t,[t2]); typ; attr=[]} ts
  | _ when not Flag.Debug.check_typ -> {desc=App(t,ts); typ=typ_unknown; attr=[]}
  | _ ->
      Format.eprintf "Untypable(make_app): %a@." Print.term' {desc=App(t,ts);typ=typ_unknown; attr=[]};
      assert false
let make_app_raw t ts =
  let t' = make_app t ts in
  {t' with desc=App(t,ts)}
let make_local decl t =
  if decl = Decl_type [] || decl = Decl_let [] then
    t
  else
    {desc=Local(decl,t); typ=t.typ; attr=[]}
let make_let bindings t2 =
  make_local (Decl_let bindings) t2
let make_let_s bindings t2 =
  let bindings' = List.filter (fun (f,_) -> List.exists (snd |- occur_in f) bindings || occur_in f t2) bindings in
  make_let bindings' t2
let make_lets_s bindings t2 =
  List.fold_right (make_let_s -| List.singleton) bindings t2
let make_let_type decls t2 =
  make_local (Decl_type decls) t2
let make_lets_type decls t2 =
  List.fold_right (make_let_type -| List.singleton) decls t2
let make_lets bindings t2 =
  List.fold_right (make_let -| List.singleton) bindings t2
let make_seq t1 t2 =
  if is_value t1 then
    t2
  else
    make_let [Id.new_var ~name:"u" t1.typ, t1] t2
let make_ignore t =
  if t.typ = Ty.unit then
    t
  else
    make_seq t unit_term
let fail_unit_term = make_app fail_term [unit_term]
let make_fail typ = make_seq fail_unit_term @@ make_bottom typ
let make_fun x t = {desc=Fun(x,t); typ=TFun(x,t.typ); attr=[]}
let make_funs = List.fold_right make_fun
let make_not t =
  match t.desc with
  | Const True -> false_term
  | Const False -> true_term
  | _ -> {desc=Not t; typ=(TBase TBool); attr=make_attr[t]}
let make_and t1 t2 =
  if t1 = false_term then
    false_term
  else if t1 = true_term then
    t2
  else if t2 = true_term then
    t1
  else if t2 = false_term && List.Set.subset [ANotFail;ATerminate] t1.attr then
    false_term
  else
    {desc=BinOp(And, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_ands ts = List.fold_right make_and ts true_term
let make_or t1 t2 =
  if t1 = true_term then
    true_term
  else if t1 = false_term then
    t2
  else if t2 = false_term then
    t1
  else if t2 = true_term && List.Set.subset [ANotFail;ATerminate] t1.attr then
    true_term
  else
    {desc=BinOp(Or, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_ors ts = List.fold_right make_or ts false_term
let make_add t1 t2 =
  if t2.desc = Const (Int 0) then
    t1
  else if t1.desc = Const (Int 0) then
    t2
  else
    {desc=BinOp(Add, t1, t2); typ=TBase TInt; attr=make_attr[t1;t2]}
let make_sub t1 t2 =
  if t2.desc = Const (Int 0) then
    t1
  else
    {desc=BinOp(Sub, t1, t2); typ=TBase TInt; attr=make_attr[t1;t2]}
let make_mul t1 t2 =
  if t1.desc = Const (Int 0) && List.Set.subset [ANotFail;ATerminate] t2.attr then
    make_int 0
  else if t2.desc = Const (Int 0) && List.Set.subset [ANotFail;ATerminate] t1.attr then
    make_int 0
  else if t2.desc = Const (Int 1) then
    t1
  else if t1.desc = Const (Int 1) then
    t2
  else
    {desc=BinOp(Mult, t1, t2); typ=TBase TInt; attr=make_attr[t1;t2]}
let make_div t1 t2 = {desc=BinOp(Div, t1, t2); typ=TBase TInt; attr=make_attr[t1;t2]}
let make_neg t = make_sub (make_int 0) t
let make_if_ t1 t2 t3 =
  assert (Flag.Debug.check_typ => Type.can_unify t1.typ (TBase TBool));
  assert (Flag.Debug.check_typ => Type.can_unify t2.typ t3.typ);
  match t1.desc with
  | Const True -> t2
  | Const False -> t3
  | _ ->
      let typ =
        match has_pred t2.typ, has_pred t3.typ with
        | _, false -> t2.typ
        | false, true -> t3.typ
        | true, true ->
            if t2.typ <> t3.typ
            then warning @@ Format.asprintf " @[<hv 2>if-branches have different types@ %a and@ %a@]" Print.typ t2.typ Print.typ t3.typ;
            t2.typ
      in
      {desc=If(t1, t2, t3); typ=typ; attr=make_attr[t1;t2;t3]}
let make_eq t1 t2 =
  assert (Flag.Debug.check_typ => Type.can_unify t1.typ t2.typ);
  match t1.desc, t2.desc with
  | Const c1, Const c2 -> make_bool (c1 = c2)
  | _ ->
      {desc=BinOp(Eq, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_neq t1 t2 =
  make_not (make_eq t1 t2)
let make_lt t1 t2 =
  {desc=BinOp(Lt, t1, t2); typ=(TBase TBool); attr=make_attr[t1;t2]}
let make_gt t1 t2 =
  {desc=BinOp(Gt, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_leq t1 t2 =
  {desc=BinOp(Leq, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_geq t1 t2 =
  {desc=BinOp(Geq, t1, t2); typ=TBase TBool; attr=make_attr[t1;t2]}
let make_binop op t1 t2 =
  let f =
    match op with
    | Eq -> make_eq
    | Lt -> make_lt
    | Gt -> make_gt
    | Leq -> make_leq
    | Geq -> make_geq
    | And -> make_and
    | Or -> make_or
    | Add -> make_add
    | Sub -> make_sub
    | Mult -> make_mul
    | Div -> make_div
  in
  f t1 t2
let make_proj i t = {desc=Proj(i,t); typ=proj_typ i t.typ; attr=make_attr[t]}
let make_tuple ts = {desc=Tuple ts; typ=make_ttuple@@List.map Syntax.typ ts; attr=[]}
let make_fst t = {desc=Proj(0,t); typ=proj_typ 0 t.typ; attr=[]}
let make_snd t = {desc=Proj(1,t); typ=proj_typ 1 t.typ; attr=[]}
let make_pair t1 t2 = {desc=Tuple[t1;t2]; typ=make_tpair t1.typ t2.typ; attr=[]}
let make_nil typ = {desc=Nil; typ=TApp(TList, [typ]); attr=[]}
let make_nil2 typ = {desc=Nil; typ=typ; attr=[]}
let make_cons t1 t2 =
  assert (Flag.Debug.check_typ => Type.can_unify (TApp(TList, [t1.typ])) t2.typ);
  {desc=Cons(t1,t2); typ=t2.typ; attr=[]}
let rec make_list ts =
  match ts with
  | [] -> make_nil typ_unknown
  | [t1] -> make_cons t1 @@ make_nil t1.typ
  | t1::ts' -> make_cons t1 @@ make_list ts'
let make_pany typ = {pat_desc=PAny; pat_typ=typ}
let make_pvar x = {pat_desc=PVar x; pat_typ=Id.typ x}
let make_pconst t = {pat_desc=PConst t; pat_typ=t.typ}
let make_ppair p1 p2 = {pat_desc=PTuple[p1;p2]; pat_typ=make_tpair p1.pat_typ p2.pat_typ}
let make_ptuple ps = {pat_desc=PTuple ps; pat_typ=make_ttuple @@ List.map (fun p -> p.pat_typ) ps}
let make_pnil typ = {pat_desc=PNil; pat_typ=make_tlist typ}
let make_pnil2 typ = {pat_desc=PNil; pat_typ=typ}
let make_pcons p1 p2 = {pat_desc=PCons(p1,p2); pat_typ=p2.pat_typ}
let make_label_aux info t = {desc=Label(info,t); typ=t.typ; attr=[]}
let make_label ?(label="") info t =
  t
  |> make_label_aux info
  |& (label <> "") &> make_label_aux (InfoString label)
let make_ref t = {desc=Ref t; typ=make_tref t.typ; attr=[]}
let make_deref t = {desc=Deref t; typ=ref_typ t.typ; attr=[]}
let make_setref r t = {desc=SetRef(r, t); typ=TBase TUnit; attr=[]}
let make_construct c ts typ =
  {desc=Constr(c,ts); typ; attr=[]}
let make_record fields typ =
  {desc=Record fields; typ; attr=[]}
let make_field ?ty t s =
  let typ =
    match ty with
    | None ->
        t.typ
        |> decomp_trecord
        |> List.assoc s
        |> snd
    | Some ty -> ty
  in
  {desc=Field(t,s); typ; attr=[]}
let make_event_unit s = make_app (make_event s) [unit_term]
let make_raise t typ = {desc=Raise t; typ; attr=[]}

let make_imply t1 t2 = make_or (make_not t1) t2

let make_eq_dec t1 t2 =
  assert (Flag.Debug.check_typ => Type.can_unify t1.typ t2.typ);
  let aux t =
    match t.desc with
    | Var x -> make_var x, Std.identity
    | _ ->
        let x = Id.new_var t.typ in
        make_var x, make_let [x,t]
  in
  let rec make t1 t2 =
    match t1.typ with
    | TBase _ -> make_eq t1 t2
    | TTuple xs ->
        let n = List.length xs in
        List.fromto 0 n
        |> List.map (fun i -> make (make_proj i t1) (make_proj i t2))
        |> List.fold_left make_and true_term
    | _ -> assert false
  in
  let t1',k1 = aux t1 in
  let t2',k2 = aux t2 in
  k1 @@ k2 @@ make t1' t2'

let is_length_var x = Id.name x = "List.length"
let make_length_var typ =
  let x = Id.make (-1) "l" [] typ in
  Id.make (-1) "List.length" [] (TFun(x, Ty.int))
let make_length t =
  {(make_app (make_var @@ make_length_var t.typ) [t]) with attr=[ANotFail;ATerminate]}

let make_module decls =
  let decls' = List.filter_out (fun decl -> decl = Decl_type [] || decl = Decl_let []) decls in
  let typ =
    let aux decl =
      match decl with
      | Decl_let defs -> List.map (fun (x,_) -> Id.name x, Id.typ x) defs
      | _ -> []
    in
    TModule(List.flatten_map aux decls')
  in
  {desc=Module decls'; typ; attr=[]}

let make_randvalue typ = {desc=Const(RandValue(typ,false)); typ=Ty.(fun_ unit typ); attr=[]}

let rec make_randvalue_unit typ =
  match typ with
  | TBase TUnit -> unit_term
  | TBase TBool -> make_eq (make_randvalue_unit Ty.int) (make_int 0)
  | TTuple [] -> make_tuple []
  | TTuple tys -> make_tuple @@ List.map (Id.typ |- make_randvalue_unit) tys
  | TFun(x,ty) -> make_fun x @@ make_randvalue_unit ty
  | TAttr(_,ty) -> make_randvalue_unit ty
  | _ -> {desc=App(make_randvalue typ, [unit_term]); typ; attr=[ANotFail;ATerminate]}

let make_randvalue_cps typ =
  {desc=Const(RandValue(typ,true)); typ=Ty.(funs [unit; fun_ typ typ_result] typ_result); attr=[]}

let make_randint_cps b =
  let attr = if b then [AAbst_under] else [] in
  {(make_randvalue_cps Ty.int) with attr}

let randint_term = make_randvalue Ty.int
let randint_unit_term = make_randvalue_unit Ty.int
let randbool_unit_term = make_randvalue_unit Ty.bool

let rec make_term typ =
  match elim_tattr typ with
  | TBase TUnit -> unit_term
  | TBase TBool -> true_term
  | TBase TInt -> make_int 0
  | TFun(x,typ) -> make_fun x (make_term typ)
  | TTuple xs -> make_tuple @@ List.map (make_term -| Id.typ) xs
  | TData "X" -> cps_result
  | TData "char" -> {desc=Const(Char '\000'); typ; attr=[]}
  | TData "string" -> {desc=Const(String ""); typ; attr=[]}
  | TData "float" -> {desc=Const(Float 0.); typ; attr=[]}
  | TApp(TList, [typ']) -> make_nil typ'
  | _ -> Format.eprintf "ERROR: %a@." Print.typ typ; assert false


let none_flag = false_term
let some_flag = true_term
(*
let none_flag = make_int 0
let some_flag = make_int 1
 *)
let opt_typ typ = TTuple [Id.new_var none_flag.typ; Id.new_var typ]
let get_opt_typ typ = snd_typ typ
let is_none t =
  match t.desc with
  | Tuple [t1;t2] -> t1 = none_flag
  | _ -> false
let decomp_some t =
  match t.desc with
  | Tuple [t1;t2] when t1 = some_flag -> Some t2
  | _ -> None
let make_none typ = make_pair none_flag (make_term typ)
let make_some t = make_pair some_flag t
let make_is_none t = make_eq (make_fst t) none_flag
let make_is_some t = make_not (make_is_none t)
let make_get_val t = make_snd t
let decomp_is_none t =
  match t.desc with
  | BinOp(Eq, {desc=Proj(0,t1)}, t2) when t2 = none_flag -> Some t1
  | _ -> None
let decomp_get_val t =
  match t.desc with
  | Proj(1, t) -> Some t
  | _ -> None

let is_randint_unit t =
  match t.desc with
  | App(t1, [{desc=Const Unit}]) -> t1.desc = randint_term.desc
  | _ -> false
let is_randbool_unit t =
  match t.desc with
  | BinOp((Eq|Leq|Geq|Lt|Gt), t, {desc=Const _})
  | BinOp((Eq|Leq|Geq|Lt|Gt), {desc=Const _}, t) -> is_randint_unit t
  | _ -> false
let rec defs_of_term t =
  match t.desc with
  | Const End_of_definitions -> []
  | Local(Decl_let def, t') -> def :: defs_of_term t'
  | _ -> invalid_arg "defs_of_term"






(*** AUXILIARY FUNCTIONS ***)

let is_base_var x = is_base_typ @@ Id.typ x
let is_fun_var x = is_fun_typ @@ Id.typ x

let decomp_var t =
  match t.desc with
  | Var x -> Some x
  | _ -> None
let is_var t = Option.is_some @@ decomp_var t

let is_fun t = [] <> fst @@ decomp_funs t

let rec decomp_lets t =
  match t.desc with
  | Local(Decl_let bindings, t2) ->
      let fbindings,t2' = decomp_lets t2 in
      bindings::fbindings, t2'
  | _ -> [], t


let get_int = make_col [] (@@@)
let get_int_term t =
  match t.desc with
  | Const (Int n) -> [n]
  | _ -> get_int.col_term_rec t
let get_int_typ typ = []
let () = get_int.col_term <- get_int_term
let () = get_int.col_typ <- get_int_typ
let get_int t = List.unique @@ get_int.col_term t




let rec get_args ty =
  match elim_tattr ty with
  | TFun(x,typ) -> x :: get_args typ
  | _ -> []

let rec get_argvars ty =
  match elim_tattr ty with
  | TFun(x,typ) -> x :: get_argvars (Id.typ x) @ get_argvars typ
  | _ -> []

let rec get_argtyps ty =
  match elim_tattr ty with
  | TFun(x,typ) -> Id.typ x :: get_argtyps typ
  | _ -> []

let arg_num typ = List.length (get_args typ)



let is_poly_typ = make_col false (||)

let is_poly_typ_typ typ =
  match elim_tattr typ with
  | TVar({contents=None},_) -> true
  | _ -> is_poly_typ.col_typ_rec typ

let () = is_poly_typ.col_typ <- is_poly_typ_typ
let is_poly_typ = is_poly_typ.col_typ



let subst = make_trans2 ()

(* [x |-> t], [t/x] *)
let subst_term (x,t) t' =
  match t'.desc with
  | Var y when Id.(x = y) -> t
  | Fun(y, t1) when Id.(x = y) -> t'
  | Local(Decl_let bindings, t2) when List.exists (fst |- Id.same x) bindings -> t'
  | Local(Decl_let bindings, t2) ->
      let aux (f,t1) = subst.tr2_var (x,t) f, subst.tr2_term (x,t) t1 in
      let bindings' = List.map aux bindings in
      let t2' = subst.tr2_term (x,t) t2 in
      let desc = Local(Decl_let bindings', t2') in
      {t' with desc}
  | Match(t1,pats) ->
      let aux (pat,cond,t1) =
        let xs = get_bv_pat pat in
        if List.exists (Id.same x) xs
        then pat, cond, t1
        else pat, subst.tr2_term (x,t) cond, subst.tr2_term (x,t) t1
      in
      let desc = Match(subst.tr2_term (x,t) t1, List.map aux pats) in
      {t' with desc}
  | _ -> subst.tr2_term_rec (x,t) t'


let subst_int = make_trans2 ()

let subst_int_term (n,t) t' =
  match t'.desc with
  | Const (Int m) when n = m -> t
  | Const (Int m) -> make_add t @@ make_int (m-n)
  | _ -> subst_int.tr2_term_rec (n,t) t'

let () = subst_int.tr2_term <- subst_int_term
let subst_int n t t' = subst_int.tr2_term (n,t) t'


let subst_map = make_trans2 ()

let subst_map_term map t =
  match t.desc with
  | Var y -> if Id.mem_assoc y map then Id.assoc y map else t
  | Fun(y, t1) ->
      let map' = List.filter_out (fst |- Id.same y) map in
      let t1' = subst_map.tr2_term map' t1 in
      make_fun y t1'
  | Local(Decl_let bindings, t2) ->
      let map' = List.filter (fun (x,_) -> not (List.exists (fst |- Id.same x) bindings)) map in
      let bindings' = List.map (Pair.map_snd @@ subst_map.tr2_term map') bindings in
      let t2' = subst_map.tr2_term map' t2 in
      make_let bindings' t2'
  | Match(t1,pats) -> (* TODO: fix *)
      let aux (pat,cond,t1) = pat, cond, subst_map.tr2_term map t1 in
      {desc=Match(subst_map.tr2_term map t1, List.map aux pats); typ=t.typ; attr=[]}
  | _ -> subst_map.tr2_term_rec map t

let () = subst_map.tr2_term <- subst_map_term
let subst_map map t =
  if map = [] then
    t
  else
    subst_map.tr2_term map t


let () = subst.tr2_term <- subst_term
let subst_type x t typ = subst.tr2_typ (x,t) typ
let subst_type_var x y typ = subst_type x (make_var y) typ
let subst x t1 t2 = subst.tr2_term (x,t1) t2
let subst_var x y t = subst x (make_var y) t
let subst_var_map map t =
  if map = [] then
    t
  else
    subst_map (List.map (Pair.map_snd make_var) map) t

let subst_var_without_typ =
  let tr = make_trans2 () in
  let tr_desc (x,y) desc =
    match desc with
    | Var z when Id.(x = z) -> Var (Id.set_typ y (Id.typ z))
    | Fun(z, t1) when Id.(x = z) -> desc
    | Local(Decl_let bindings, t2) when List.exists (fst |- Id.same x) bindings -> desc
    | Local(Decl_let bindings, t2) ->
        let aux (f,t1) = tr.tr2_var (x,y) f, tr.tr2_term (x,y) t1 in
        let bindings' = List.map aux bindings in
        let t2' = tr.tr2_term (x,y) t2 in
        Local(Decl_let bindings', t2')
    | Match(t1,pats) ->
        let aux (pat,cond,t1) =
          let xs = get_bv_pat pat in
          if List.exists (Id.same x) xs
          then pat, cond, t1
          else pat, tr.tr2_term (x,y) cond, tr.tr2_term (x,y) t1
        in
        Match(tr.tr2_term (x,y) t1, List.map aux pats)
    | _ -> tr.tr2_desc_rec (x,y) desc
  in
  tr.tr2_desc <- tr_desc;
  fun x y t -> tr.tr2_term (x,y) t
let subst_var_map_without_typ map t =
  List.fold_right (Fun.uncurry subst_var_without_typ) map t


let make_match t1 pats =
  match pats with
  | [{pat_desc=PAny}, {desc=Const True}, t2] -> make_seq t1 t2
  | [{pat_desc=PVar x}, {desc=Const True}, t2] ->
      if Id.mem x @@ get_fv t1 then
        let x' = Id.new_var_id x in
        make_let [x',t1] @@ subst_var x x' t2
      else
        make_let [x,t1] t2
  | _ -> {desc=Match(t1,pats); typ=Syntax.typ@@Triple.trd@@List.hd pats; attr=[]}
let make_single_match ?(total=false) t1 p t2 =
  let rec is_total p =
    match p.pat_desc with
    | PAny -> true
    | PVar _ -> true
    | PTuple ps -> List.for_all is_total ps
    | PRecord fields -> List.for_all (snd |- is_total) fields
    | PAlias(p, _) -> is_total p
    | _ -> false
  in
  if total || is_total p
  then make_match t1 [p, true_term, t2]
  else make_match t1 [p, true_term, t2; make_pany p.pat_typ, true_term, make_fail t2.typ]
let make_trywith t x pats =
  if List.Set.subset [ANotFail;ATerminate] t.attr then
    t
  else
    let handler = make_fun x @@ make_match (make_var x) pats in
    {desc=TryWith(t, handler); typ=t.typ; attr=[]}
let make_trywith_simple t handler = {desc=TryWith(t, handler); typ=t.typ; attr=[]}




let get_tapred typ =
  match typ with
  | TAttr(attrs, _) ->
      Option.of_list @@ List.filter_map (function TAPred(x,ps) -> Some (x,ps) | _ -> None) attrs
  | _ -> None

let add_tapred x ps typ =
  let attrs',typ' =
    match get_tapred typ, typ with
    | None, _ -> [TAPred(x,ps)], typ
    | Some(x',ps'), TAttr(attrs, typ') ->
        let attrs' = List.filter (function TAPred _ -> false | _ -> true) attrs in
        let ps' = (List.map (subst_var x' x) ps') @ ps in
        TAPred(x,ps')::attrs', typ'
    | _ -> assert false
  in
  _TAttr attrs' typ'



let max_pat_num = make_col 0 max

let max_pat_num_term t =
  match t.desc with
  | Match(t,pats) ->
      let m = max (List.length pats) (max_pat_num.col_term t) in
      let aux acc (_,cond,t) = max acc (max (max_pat_num.col_term t) (max_pat_num.col_term cond)) in
      List.fold_left aux m pats
  | _ -> max_pat_num.col_term_rec t

let () = max_pat_num.col_term <- max_pat_num_term
let max_pat_num = max_pat_num.col_term





let same_list same xs ys = List.length xs = List.length ys && List.for_all2 same xs ys

let rec same_const c1 c2 =
  match c1,c2 with
  | RandValue(typ1, b1) , RandValue(typ2, b2) -> b1 = b2 && same_shape typ1 typ2
  | _ -> c1 = c2
and same_term t1 t2 = same_desc t1.desc t2.desc
and same_desc t1 t2 =
  match t1,t2 with
  | Const c1, Const c2 -> same_const c1 c2
  | Var x, Var y -> Id.(x = y)
  | Fun(x,t1), Fun(y,t2) -> Id.(x = y) && same_term t1 t2
  | App(t1,ts1), App(t2,ts2) -> same_list same_term (t1::ts1) (t2::ts2)
  | If(t11,t12,t13), If(t21,t22,t23) -> same_term t11 t21 && same_term t12 t22 && same_term t13 t23
  | Local(Decl_let bindings1,t1), Local(Decl_let bindings2,t2) ->
     let same_binding (f,t1) (g,t2) = Id.(f = g) && same_term t1 t2 in
     same_list same_binding bindings1 bindings2 && same_term t1 t2
  | BinOp(op1,t11,t12), BinOp(op2,t21,t22) -> op1 = op2 && same_term t11 t21 && same_term t12 t22
  | Not t1, Not t2 -> same_term t1 t2
  | Event(s1,b1), Event(s2,b2) -> s1 = s2 && b1 = b2
  | Record _, Record _ -> unsupported "same_term 2"
  | Field _, Field _ -> unsupported "same_term 3"
  | SetField _, SetField _ -> unsupported "same_term 4"
  | Nil, Nil -> true
  | Cons _, Cons _ -> unsupported "same_term 5"
  | Constr(c1, ts1), Constr(c2, ts2) -> c1 = c2 && List.for_all2 same_term ts1 ts2
  | Match(t1,pats1), Match(t2,pats2) ->
      let eq (pat1,cond1,t1) (pat2,cond2,t2) = pat1 = pat2 && same_term cond1 cond2 && same_term t1 t2 in
      same_term t1 t2 && List.eq ~eq pats1 pats2
  | Raise t1, Raise t2 -> same_term t1 t2
  | TryWith(t11,t12), TryWith(t21,t22) -> same_term t11 t21 && same_term t12 t22
  | Tuple ts1, Tuple ts2 -> List.length ts1 = List.length ts2 && List.for_all2 same_term ts1 ts2
  | Proj(i,t1), Proj(j,t2) -> i = j && same_term t1 t2
  | Bottom, Bottom -> true
  | Label _, Label _ -> unsupported "same_term 11"
  | _ -> false

and same_info i1 i2 = unsupported "same_term"
and same_type_kind k1 k2 = unsupported "same_term"

and same_typed_pattern p1 p2 = same_pattern p1.desc p2.desc
and same_pattern p1 p2 = unsupported "same_term"

let same_term' t1 t2 = try same_term t1 t2 with _ -> false




let merge_tattrs attr1 attr2 =
  let attrs =
    let eq x y =
      match x, y with
      | TAPred _, TAPred _ -> true
      | TAPureFun,  TAPureFun -> true
      | TARefPred _, TARefPred _ -> true
      | _ -> false
    in
    List.classify ~eq (attr1 @ attr2)
  in
  let merge a1 a2 =
    match a1, a2 with
    | TAPred(x1,ps1), TAPred(x2,ps2) ->
        let merge_preds ps1 ps2 =
          let aux p ps =
            if List.exists (same_term p) ps
            then ps
            else p::ps
          in
          List.fold_right aux ps1 ps2
        in
        let ps2' = List.map (subst_var x2 x1) ps2 in
        TAPred(x1, merge_preds ps1 ps2')
    | TARefPred(x1,p1), TARefPred(x2,p2) ->
        let p2' = subst_var x2 x1 p2 in
        let p = if same_term p1 p2' then p1 else make_and p1 p2' in
        TARefPred(x1, p)
    | TAPureFun, TAPureFun -> TAPureFun
    | _ -> assert false
  in
  let aux attr =
    match attr with
    | [] -> assert false
    | [a] -> a
    | a::attr' -> List.fold_left merge a attr'
  in
  List.map aux attrs

let rec merge_typ typ1 typ2 =
  match typ1,typ2 with
  | TVar({contents=Some typ1},_), typ2
  | typ1, TVar({contents=Some typ2},_) -> merge_typ typ1 typ2
  | TVar({contents=None},_), _ -> typ2
  | _, TVar({contents=None},_) -> typ1
  | _ when typ1 = typ_unknown -> typ2
  | _ when typ2 = typ_unknown -> typ1
  | TBase b1, TBase b2 when b1 = b2 -> TBase b1
  | TAttr(attr1,typ1), TAttr(attr2,typ2) ->
      TAttr(merge_tattrs attr1 attr2, merge_typ typ1 typ2)
  | TAttr(attr, typ'), typ
  | typ, TAttr(attr, typ') -> TAttr(attr, merge_typ typ typ')
  | TFuns(xs1,typ1), TFuns(xs2,typ2) ->
      let xs = List.map2 (fun x1 x2 -> Id.new_var ~name:(Id.name x1) @@ merge_typ (Id.typ x1) (Id.typ x2)) xs1 xs2 in
      TFuns(xs, merge_typ typ1 typ2)
  | TFun(x1,typ1), TFun(x2,typ2) ->
      let x_typ = merge_typ (Id.typ x1) (Id.typ x2) in
      let x = Id.new_var ~name:(Id.name x1) x_typ in
      let typ = merge_typ (subst_type_var x1 x typ1) (subst_type_var x2 x typ2) in
      TFun(x, typ)
  | TApp(c1,typs1), TApp(c2,typs2) ->
      assert (c1 = c2);
      TApp(c1, List.map2 merge_typ typs1 typs2)
  | TTuple xs1, TTuple xs2 ->
      let aux x1 x2 xs =
        let x = Id.set_typ x1 @@ merge_typ (Id.typ x1) (Id.typ x2) in
        List.map (Id.map_typ @@ subst_type_var x2 x1) @@ x::xs
      in
      TTuple (List.fold_right2 aux xs1 xs2 [])
  | TData _, TData _ -> assert (typ1 = typ2); typ1
  | TVariant(poly1,labels1), TVariant(poly2,labels2) ->
      assert (poly1 = poly2);
      let labels = List.map2 (fun (s1,typs1) (s2,typs2) -> assert (s1=s2); s1, List.map2 merge_typ typs1 typs2) labels1 labels2 in
      TVariant(poly1, labels)
  | TRecord fields1, TRecord fields2 ->
      let fields = List.map2 (fun (s1,(f1,typ1)) (s2,(f2,typ2)) -> assert (s1=s2 && f1=f2); s1, (f1, merge_typ typ1 typ2)) fields1 fields2 in
      TRecord fields
  | _ -> Format.eprintf "typ1:%a, typ2:%a@." Print.typ typ1 Print.typ typ2; assert false

let make_if t1 t2 t3 =
  assert (Flag.Debug.check_typ => Type.can_unify t1.typ Ty.bool);
  if Flag.Debug.check_typ && not @@ Type.can_unify t2.typ t3.typ then
    (Format.eprintf "%a <=/=> %a@." Print.typ t2.typ Print.typ t3.typ;
     assert false);
  match t1.desc with
  | Const True -> t2
  | Const False -> t3
  | _ when List.Set.subset [ANotFail;ATerminate] t2.attr && same_term' t2 t3 -> t2
  | _ -> {desc=If(t1, t2, t3); typ=merge_typ t2.typ t3.typ; attr=make_attr[t1;t2;t3]}
let make_assert t = make_if t unit_term fail_unit_term
let make_assume t1 t2 = make_if t1 t2 (make_bottom t2.typ)
let make_br t2 t3 = make_if randbool_unit_term t2 t3

let rec get_top_funs acc = function
  | {desc=Local(Decl_let defs, t)} ->
      let acc' = List.fold_left (fun acc (f,_) -> f::acc) acc defs in
      get_top_funs acc' t
  | _ -> acc
let get_top_funs = get_top_funs []

let rec get_top_rec_funs acc = function
  | {desc=Local(Decl_let defs, t)} ->
      let acc' = List.fold_left (fun acc (f,_) -> f::acc) acc defs in
      get_top_rec_funs acc' t
  | _ -> acc
let get_top_rec_funs = get_top_rec_funs []


let has_no_effect =
  let col = make_col true (&&) in
  let col_term t =
    match t.desc with
    | Const _ -> true
    | Var _ -> true
    | Fun _ -> true
    | App _ -> false
    | Local(Decl_let bindings,t) -> col.col_term t && List.for_all (snd |- col.col_term) bindings
    | Field _ -> false
    | SetField _ -> false
    | Raise _ -> false
    | Bottom -> false
    | Ref _ -> false
    | Deref _ -> false
    | SetRef _ -> false
    | _ -> col.col_term_rec t
  in
  col.col_term <- col_term;
  col.col_term



let rec is_simple_aexp t =
  if elim_tattr t.typ <> Ty.int then
    false
  else
    match t.desc with
    | Const _ -> true
    | Var _ -> true
    | BinOp(_, t1, t2) -> is_simple_aexp t1 && is_simple_aexp t2
    | _ -> false

and is_simple_bexp t =
  if elim_tattr t.typ <> Ty.bool then
    false
  else
    match t.desc with
    | Const _ -> true
    | Var _ -> true
    | BinOp(_, t1, t2) ->
        is_simple_bexp t1 && is_simple_bexp t2 ||
        is_simple_aexp t1 && is_simple_aexp t2
    | Not t -> is_simple_bexp t
    | _ -> false





let rec var_name_of_term t =
  match t.desc with
  | Bottom -> "bot"
  | Var x -> Id.name x
  | Local(_,t) -> var_name_of_term t
  | Tuple([]) -> "nil"
  | Tuple(ts) -> String.join "__" @@ List.map var_name_of_term ts
  | Proj(i,t) ->
      let n = tuple_num t.typ in
      let names = String.nsplit (var_name_of_term t) "__" in
      if n = Some (List.length names) && List.nth names i <> ""
      then List.nth names i
      else var_name_of_term t ^ "_" ^ string_of_int i
  | App({desc=Var f},_) -> "r" ^ "_" ^ Id.name f
  | _ -> Type.var_name_of @@ elim_tattr t.typ

let new_var_of_term t = Id.new_var ~name:(var_name_of_term t) t.typ



let make_let' t1 make_t2 =
  let x = new_var_of_term t1 in
  make_let [x,t1] @@ make_t2 x


let col_same_term = make_col2 [] (@@@)

let col_same_term_term t1 t2 =
  let b = try same_term t1 t2 with _ -> false in
  if b then [t2] else col_same_term.col2_term_rec t1 t2

let () = col_same_term.col2_term <- col_same_term_term
let col_same_term = col_same_term.col2_term



let col_info_id = make_col [] (@@@)

let col_info_id_desc desc =
  match desc with
  | Label(InfoId x, t) -> x::col_info_id.col_term t
  | _ -> col_info_id.col_desc_rec desc

let () = col_info_id.col_desc <- col_info_id_desc
let col_info_id = col_info_id.col_term



let subst_rev = make_trans2 ()

(* [t1 |-> x] *)
let subst_rev_term (t1,x) t2 =
  if same_term' t1 t2 || t1 = t2
  then make_var x
  else subst_rev.tr2_term_rec (t1,x) t2

let () = subst_rev.tr2_term <- subst_rev_term
let subst_rev t1 x t2 = subst_rev.tr2_term (t1,x) t2


(* replace t1 with t2 in t3 *)
let replace_term t1 t2 t3 =
  let x = Id.new_var t1.typ in
  subst x t2 @@ subst_rev t1 x t3


(* for debug *)
let is_id_unique t =
  let bv = get_bv t in
  let rec check xs =
    match xs with
    | [] -> true
    | x::xs' ->
        if Id.mem x xs'
        then (Format.eprintf "%a" Id.print x; false)
        else check xs'
  in
  check bv
(*
  List.length bv = List.length (List.unique ~cmp:Id.same bv)
*)



let rec is_bottom_def f t =
  let xs,t = decomp_funs t in
  match xs, t.desc with
  | _::_, App({desc=Var g},ts) ->
      Id.(f = g) && List.for_all has_no_effect ts
  | _ -> false

let rec decomp_bexp t =
  match t.desc with
  | BinOp((And|Or), t1, t2) -> decomp_bexp t1 @ decomp_bexp t2
  | Not t1 -> decomp_bexp t1
  | _ -> [t]

let var_of_term t =
  match t.desc with
  | Var x -> x
  | _ -> invalid_arg "var_of_term"

let int_of_term t =
  match t.desc with
  | Const (Int n) -> n
  | _ -> invalid_arg "int_of_term"

let bool_of_term t =
  match t.desc with
  | Const True -> true
  | Const False -> false
  | _ -> invalid_arg "bool_of_term"

let pair_of_term t =
  match t.desc with
  | Tuple [t1; t2] -> t1, t2
  | _ -> invalid_arg "pair_of_term"

let tuple_of_term t =
  match t.desc with
  | Tuple ts -> ts
  | _ -> invalid_arg "tuple_of_term"

let rec list_of_term t =
  match t.desc with
  | Nil -> []
  | Cons(t1,t2) -> t1 :: list_of_term t2
  | _ -> invalid_arg "list_of_term"


(* not capture-avoiding *)
let subst_tdata,subst_tdata_typ =
  let tr = make_trans2 () in
  let tr_typ (s,ty1) ty2 =
    match ty2 with
    | TData s' when s = s' -> ty1
    | _ -> tr.tr2_typ_rec (s,ty1) ty2
  in
  tr.tr2_typ <- tr_typ;
  (fun s ty' t -> tr.tr2_term (s,ty') t),
  (fun s ty' ty -> tr.tr2_typ (s,ty') ty)

(* not capture-avoiding *)
let subst_tdata_map,subst_tdata_typ_map =
  let tr = make_trans2 () in
  let tr_typ map ty =
    match ty with
    | TData s when List.mem_assoc s map -> List.assoc s map
    | _ -> tr.tr2_typ_rec map ty
  in
  tr.tr2_typ <- tr_typ;
  tr.tr2_term, tr.tr2_typ


let rec get_last_definition prefix env def t =
  match t.desc with
  | Local(Decl_let bindings, t2) ->
      get_last_definition prefix env (Some (env,bindings)) t2
  | Local(Decl_type decls, t2) ->
      let env' = List.map (fun (s,_) -> s, TData (prefix ^ s)) decls @ env in
      get_last_definition prefix env' def t2
  | Module decls -> unsupported "get_last_definition"
  | Fun _ -> []
  | _ ->
      match def with
      | None -> []
      | Some (env', [m, {desc=Module decls}]) ->
          List.fold_right make_local decls end_of_definitions
          |> get_last_definition (Id.name m ^ "." ^ prefix) env None
      | Some (env', bindings) -> List.map (Pair.map_fst @@ Id.map_typ (subst_tdata_typ_map env')) bindings
let get_last_definition t = get_last_definition "" [] None t

let rec get_body t =
  match t.desc with
  | Local(_, t2) -> get_body t2
  | _ -> t

let count_occurrence x t =
  List.length @@ List.filter (Id.same x) @@ get_fv ~eq:(fun _ _ -> false) t

let add_attr attr t = {t with attr=attr::t.attr}
let add_attrs attrs t = List.fold_right add_attr attrs t
let add_comment s t = add_attr (AComment s) t
let add_id id t = add_attr (AId id) t
let remove_attr attr t = {t with attr = List.filter_out ((=) attr) t.attr}
let replace_id id1 id2 t = add_id id2 @@ remove_attr (AId id1) t

let get_id t =
  try
    List.find_map (function AId n -> Some n | _ -> None) t.attr
  with Not_found -> invalid_arg "get_id"
let get_id_option t =
   Option.try_with (fun () -> get_id t) ((=) @@ Invalid_argument "get_id")


let get_id_map = make_col2 () (Fun.const2 ())
let get_id_map_term map t =
  get_id_map.col2_term_rec map t;
  Hashtbl.add map (get_id t) t
let get_id_map_typ map typ = ()
let () = get_id_map.col2_term <- get_id_map_term
let () = get_id_map.col2_typ <- get_id_map_typ
let get_id_map t =
  let map = Hashtbl.create 0 in
  get_id_map.col2_term map t;
  map



let rec decomp_prog t =
  match t.desc with
  | Local(Decl_let bindings, t') ->
      let defs,main = decomp_prog t' in
      bindings::defs, main
  | _ -> [], t

let from_fpat_const c =
  match c with
  | Fpat.Const.Unit -> unit_term
  | Fpat.Const.True -> true_term
  | Fpat.Const.False -> false_term
  | Fpat.Const.Int n -> make_int n
  | _ -> unsupported "Term_util.from_fpat_const"

let from_fpat_idnt x =
  Id.from_string (Fpat.Idnt.string_of x) typ_unknown

let decomp_binop t =
  match t with
  | Fpat.Term.Const c ->
      begin
      match c with
      | Fpat.Const.Lt _ -> Some make_lt
      | Fpat.Const.Gt _ -> Some make_gt
      | Fpat.Const.Leq _ -> Some make_leq
      | Fpat.Const.Geq _ -> Some make_geq
      | Fpat.Const.Eq _ -> Some make_eq
      | Fpat.Const.Add _ -> Some make_add
      | Fpat.Const.Sub _ -> Some make_sub
      | Fpat.Const.Mul _ -> Some make_mul
      | Fpat.Const.Div _ -> Some make_div
      | Fpat.Const.Neq _ -> Some (fun x y -> make_not @@ make_eq x y)
      | _ -> None
      end
  | _ -> None

let rec decomp_list t =
  match t.desc with
  | Nil -> Some []
  | Cons(t1, t2) -> Option.map (List.cons t1) @@ decomp_list t2
  | _ -> None
let is_list_literal t = None <> decomp_list t

let rec from_fpat_term = function
  | Fpat.Term.Const c -> from_fpat_const c
  | Fpat.Term.Var x -> make_var (from_fpat_idnt x)
  | Fpat.Term.App(Fpat.Term.App(f, t1), t2) when Option.is_some @@ decomp_binop f ->
      let make = Option.get @@ decomp_binop f in
      let t1' = from_fpat_term t1 in
      let t2' = from_fpat_term t2 in
      make t1' t2'
  | Fpat.Term.App(Fpat.Term.Const Fpat.Const.Not, t) -> make_not (from_fpat_term t)
  | t -> Fpat.Term.pr Format.std_formatter t; assert false

let from_fpat_formula t = from_fpat_term @@ Fpat.Formula.term_of t


let find_exn_typ = make_col [] (@)
let find_exn_typ_term t =
  match t.desc with
  | Raise t' ->
      Debug.printf "FOUND1: %a@." Print.typ t'.typ;
      [t'.typ]
  | TryWith(t', {typ=TFun(x, _)}) ->
      Debug.printf "FOUND2: %a@." Print.typ @@ Id.typ x;
      [Id.typ x]
  | _ -> find_exn_typ.col_term_rec t
let find_exn_typ_typ typ = []
let () = find_exn_typ.col_term <- find_exn_typ_term
let () = find_exn_typ.col_typ <- find_exn_typ_typ
let find_exn_typ t =
  match find_exn_typ.col_term t with
  | [] -> None
  | typ::_ -> Some typ



let col_typ_var = make_col [] (@)
let col_typ_var_typ typ =
  match typ with
  | TVar({contents=None} as r,_) -> [r]
  | _ -> col_typ_var.col_typ_rec typ
let () = col_typ_var.col_typ <- col_typ_var_typ
let col_typ_var t =
  List.unique ~eq:(==) @@ col_typ_var.col_term t



let col_id = make_col [] (@)
let col_id_term t =
  List.filter_map (function AId n -> Some n | _ -> None) t.attr @ col_id.col_term_rec t
let () = col_id.col_term <- col_id_term
let col_id t = List.unique @@ col_id.col_term t


let rec is_fail t =
  match t.desc with
  | Local(Decl_let [_, t], _) -> is_fail t
  | App({desc=Event("fail",_)}, [{desc=Const Unit}]) -> true
  | _ -> false


let col_app_args =
  let col = make_col2 [] (@) in
  let col_desc f desc =
    match desc with
    | App({desc=Var g}, ts) when Id.(f = g) -> [ts]
    | _ -> col.col2_desc_rec f desc
  in
  col.col2_desc <- col_desc;
  col.col2_term


let col_non_fixed_args =
  let col = make_col2 [] (@) in
  let col_desc (f,xs) desc =
    match desc with
    | App({desc=Var g}, ts) when Id.(f = g) ->
        let check (i,x) =
          match List.nth ts i with
          | {desc=Var y} when Id.(x = y) -> []
          | t -> x :: col.col2_term (f,xs) t
          | exception Invalid_argument _ -> [x]
        in
        List.flatten_map check xs
    | _ -> col.col2_desc_rec (f,xs) desc
  in
  col.col2_desc <- col_desc;
  fun f xs t ->
    let xs' = List.mapi Pair.pair xs in
    col.col2_term (f,xs') t

let find_fixed_args f xs t =
  let non_fixed = col_non_fixed_args f xs t in
  List.filter_out (Id.mem -$- non_fixed) xs



let trans_if =
  let tr = make_trans2 () in
  let trans_if_term trans t2 =
    match trans t2 with
    | None -> tr.tr2_term_rec trans t2
    | Some t2' -> t2'
  in
  tr.tr2_term <- trans_if_term;
  tr.tr2_term

let rename,rename_pat =
  let tr = make_trans2 () in
  let tr_var map x =
    match Id.assoc_option x map with
    | None -> x
    | Some y -> Id.set_typ y @@ Id.typ x
  in
  tr.tr2_var <- tr_var;
  tr.tr2_typ <- Fun.snd;
  tr.tr2_term, tr.tr2_pat

let get_max_var_id =
  let col = make_col (-1) max in
  let col_id_term t =
    match t.desc with
    | Var x -> Id.id x
    | _ -> col.col_term_rec t
  in
  col.col_term <- col_id_term;
  col.col_term

let effect_of_typ ty =
  match ty with
  | TAttr(TAEffect e::_, _) -> e
  | _ ->
      Format.eprintf "%a@." Print.typ ty;
      invalid_arg "effect_of_typ"

let effect_of t =
  match t.attr with
  | AEffect e::_ -> e
  | _ ->
      Format.eprintf "%a@." Print.term t;
      invalid_arg "effect_of"

let get_tdata =
  let col = make_col [] (@@@) in
  let col_typ ty =
    match ty with
    | TData s -> [s]
    | _ -> col.col_typ_rec ty
  in
  col.col_typ <- col_typ;
  col.col_typ

let has_pnondet =
  let col = make_col false (||) in
  let col_pat p =
    match p.pat_desc with
    | PNondet -> true
    | _ -> col.col_pat_rec p
  in
  col.col_pat <- col_pat;
  col.col_pat

let set_id_counter_to_max =
  Id.set_counter -| succ -| get_max_var_id

module Term = struct
  let unit = unit_term
  let tt = true_term
  let true_ = true_term
  let ff = false_term
  let false_ = false_term
  let fail = fail_unit_term
  let randi = randint_unit_term
  let randb = randbool_unit_term
  let rand = make_randvalue_unit
  let bot = make_bottom
  let eod = end_of_definitions
  let var = make_var
  let vars = List.map make_var
  let int = make_int
  let bool = make_bool
  let string = make_string
  let (@) = make_app
  let (@@) = make_app
  let let_ = make_let
  let lets = make_lets
  let fun_ = make_fun
  let not = make_not
  let (&&) = make_and
  let (||) = make_or
  let (+) = make_add
  let (-) = make_sub
  let ( * ) = make_mul
  let (/) = make_div
  let (~-) = make_neg
  let if_ = make_if
  let br = make_br
  let (=) = make_eq
  let (<>) = make_neq
  let (<) = make_lt
  let (>) = make_gt
  let (<=) = make_leq
  let (>=) = make_geq
  let (<|) t1 op = make_binop op t1 and (|>) mb t2 = mb t2
  let fst = make_fst
  let snd = make_snd
  let pair = make_pair
  let tuple = make_tuple
  let proj = make_proj
  let nil = make_nil
  let cons = make_cons
  let list = make_list
  let seq = make_seq
  let seqs = List.fold_right make_seq
  let ignore = make_ignore
  let assert_ = make_assert
  let assume = make_assume
  let none = make_none
  let some = make_some
  let match_ = make_match
  let length = make_length
  let (|->) = subst
end

module Pat = struct
  let __ = make_pany
  let const_ = make_pconst
  let unit = const_ unit_term
  let int = const_ -| make_int
  let bool = const_ -| make_bool
  let true_ = bool true
  let false_ = bool false
  let var = make_pvar
  let pair = make_ppair
  let tuple = make_ptuple
  let nil = make_pnil
  let nil2 = make_pnil2
  let cons = make_pcons
end
