
open Util
open Asttypes
open Typedtree
open Types
open Syntax
open Term_util
open Type


type declaration =
    Decl_let of rec_flag * (id * typed_term) list
  | Decl_type of (string * (typ list * type_kind)) list
  | Decl_exc of string * typ list


(*
let () = Compile.init_path ()
*)
let () = Config.load_path := Flag.ocaml_lib
let initial_env = Compile.initial_env ()



let prim_typs =
  ["unit", TUnit;
   "bool", TBool;
   "int", TInt;
   "Pervasives.format", TConstr("string", false);
(*
   "Pervasives.in_channel", TUnit
*)]

let conv_primitive_app t ts typ =
  match t.desc,ts with
      Var {Id.name="Pervasives.="}, [t1;t2] -> make_eq t1 t2
    | Var {Id.name="Pervasives.<>"}, [t1;t2] -> make_neq t1 t2
    | Var {Id.name="Pervasives.<"}, [t1;t2] -> make_lt t1 t2
    | Var {Id.name="Pervasives.>"}, [t1;t2] -> make_gt t1 t2
    | Var {Id.name="Pervasives.<="}, [t1;t2] -> make_leq t1 t2
    | Var {Id.name="Pervasives.>="}, [t1;t2] -> make_geq t1 t2
    | Var {Id.name="Pervasives.&&"}, [t1;t2] -> make_and t1 t2
    | Var {Id.name="Pervasives.||"}, [t1;t2] -> make_or t1 t2
    | Var {Id.name="Pervasives.+"}, [t1;t2] -> make_add t1 t2
    | Var {Id.name="Pervasives.-"}, [t1;t2] -> make_sub t1 t2
    | Var {Id.name="Pervasives.*"}, [t1;t2] -> make_mul t1 t2
    | Var {Id.name="Pervasives.~-"}, [t] -> make_neg t
    | Var {Id.name="Pervasives.not"}, [t] -> make_not t
    | Var {Id.name="Pervasives.fst"}, [t] -> make_fst t
    | Var {Id.name="Pervasives.snd"}, [t] -> make_snd t
    | Var {Id.name="Pervasives.raise"}, [t] -> {desc=Raise(t); typ=typ}
    | Var {Id.name="Random.bool"}, [{desc=Const Unit}] -> make_eq (make_app randint_term [unit_term]) (make_int 0)
    | Var {Id.name="Random.int"}, [{desc=Const (Int 0)}] -> make_app randint_term [unit_term]
    | Var {Id.name="Random.int"}, [t] ->
        let x = Id.new_var "ni" TInt in
          make_let [x, [], make_app randint_term [unit_term]]
            (make_if
               (make_and (make_leq (make_int 0) (make_var x)) (make_lt (make_var x) t))
               (make_var x)
               (make_loop TInt))
    | Var {Id.name="Pervasives.open_in"}, [{desc=Const(Int _)}] -> make_app (make_event "newr") [unit_term]
    | Var {Id.name="Pervasives.close_in"}, [{typ=TUnit}] -> make_app (make_event "close") [unit_term]
    | _ -> make_app t ts

let venv = ref []

let rec from_type_expr tenv typ =
  let typ' = Ctype.repr typ in
    match typ'.Types.desc with
        Tvar ->
          begin
            try
              List.assoc typ'.Types.id !venv
            with Not_found ->
              let x = TVar (ref None) in
                venv := (typ'.Types.id, x)::!venv;
                x
          end
      | Tarrow(_, typ1, typ2, _) ->
          let typ1' = from_type_expr tenv typ1 in
          let typ2' = from_type_expr tenv typ2 in
          let x = Id.new_var "x" typ1' in
            TFun(x, typ2')
      | Ttuple [] -> assert false
      | Ttuple (typ::typs) ->
          let aux typ_pair typ =
            let typ' = from_type_expr tenv typ in
              TPair(Id.new_var "x" typ_pair,typ')
          in
            List.fold_left aux (from_type_expr tenv typ) typs
      | Tconstr(path, _, _) when List.mem_assoc (Path.name path) prim_typs ->
          List.assoc (Path.name path) prim_typs
      | Tconstr(path, [type_expr], _) when Path.name path = "list" ->
          TList (from_type_expr tenv type_expr)
      | Tconstr(path, _, m) ->
          let b =
            try match (Env.find_type path tenv).type_kind with
                Type_abstract -> false
              | Type_variant _ -> true
              | Type_record _ -> true
            with Not_found -> false
          in
            TConstr(Path.name path, b)
      | Tobject _ -> unsupported "Tobject"
      | Tfield _ -> unsupported "Tfield"
      | Tnil -> unsupported "Tnil"
      | Tlink _ -> unsupported "Tlink"
      | Tsubst _ -> unsupported "Tsubst"
      | Tvariant _ -> unsupported "Tvariant"
      | Tunivar -> unsupported "Tunivar"
      | Tpoly _ -> unsupported "Tpoly"
      | Tpackage _ -> unsupported "Tpackage"


let from_rec_flag = function
    Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive
  | Asttypes.Default -> unsupported "rec_flag (Default)"

let from_mutable_flag = function
    Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable



let sign_to_letters s =
  let is_op s = List.mem s.[0] ['!';'$';'%';'&';'*';'+';'-';'.';'/';':';'<';'=';'>';'?';'@';'^';'|';'~'] in
  let map = function
      '!' -> "_bang"
    | '$' -> "_dollar"
    | '%' -> "_percent"
    | '&' -> "_ampersand"
    | '*' -> "_asterisk"
    | '+' -> "_plus"
    | '-' -> "_minus"
    | '.' -> "_dot"
    | '/' -> "_slash"
    | ':' -> "_colon"
    | '<' -> "_lessthan"
    | '=' -> "_equal"
    | '>' -> "_greaterthan"
    | '?' -> "_question"
    | '@' -> "_at"
    | '^' -> "_caret"
    | '|' -> "_bar"
    | '~' -> "_tilde"
    | c -> String.make 1 c
  in
  let rec trans acc s =
    if String.length s = 0
    then acc
    else trans (acc ^ map s.[0]) (String.sub s 1 (String.length s - 1))
  in
    if is_op s
    then trans "op" s
    else s

let from_ident_aux name binding_time typ =
  let name = sign_to_letters name in
  let name = if name.[0] = '_' then "x" ^ name else name in
    Id.make binding_time name typ

let from_ident x typ =
  from_ident_aux (Ident.name x) (Ident.binding_time x) typ

let from_ident_path path typ =
  from_ident_aux (Path.name path) (Path.binding_time path) typ


let get_constr_name desc typ env =
  let clean_copy ty =
    if ty.level = Btype.generic_level
    then ty
    else Subst.type_expr Subst.identity ty
  in
  let rec get_type_descr ty tenv =
    match (Ctype.repr ty).Types.desc with
        Tconstr (path,_,_) -> Env.find_type path tenv
      | _ -> assert false
  in
  let rec get_constr tag ty tenv =
    match get_type_descr ty tenv with
        {type_kind=Type_variant constr_list} ->
          Datarepr.find_constr_by_tag tag constr_list
      | {type_manifest = Some _} ->
          get_constr tag (Ctype.expand_head_once tenv (clean_copy ty)) tenv
      | _ -> assert false
  in
    match desc.cstr_tag with
        Cstr_exception path -> Path.name path
      | _ -> fst (get_constr desc.cstr_tag typ env)
let get_constr_name desc typ env =
  get_constr_name desc typ env

let get_label_name label env =
  let typ_decl =
    let path =
      match (Ctype.repr label.lbl_res).Types.desc with
          Tconstr(path,_,_) -> path
        | _ -> assert false
    in
      Env.find_type path env
  in
    match typ_decl.type_kind with
        Type_record(labels, _) ->
          let name,_,_ = List.nth labels label.lbl_pos in
            name
      | Type_variant _
      | Type_abstract -> assert false


let rec from_type_declaration tenv decl =
  let venv = List.map (fun typ -> typ.Types.id, TVar (ref None)) decl.type_params in
  let params = List.map snd venv in
  let kind =
    match decl.type_kind with
        Type_abstract -> KAbstract
      | Type_variant stypss ->
          KVariant(List.map (fun (s,typs) -> s,List.map (from_type_expr tenv) typs) stypss)
      | Type_record(sftyps,_) ->
          KRecord(List.map (fun (s,flag,typ) -> s,(from_mutable_flag flag,from_type_expr tenv typ)) sftyps)
  in
    params, kind


let rec add_type_env env typ =
  match (Ctype.repr typ).Types.desc with
      Tvar -> ()
    | Tarrow(_,typ1,typ2,_) -> add_type_env env typ1; add_type_env env typ2
    | Ttuple typs -> List.iter (add_type_env env) typs
    | Tconstr(path,typs,_) ->
        begin
          match (Env.find_type path env).type_kind with
              Type_abstract -> ()
            | Type_variant stypss ->
                let typ_name = Path.name path in
                let kind = Type_decl.TKVariant (List.map (fun (s,typs) -> s, List.map (from_type_expr env) typs) stypss) in
                  Type_decl.add_typ_decl typ_name kind
            | Type_record(fields,_) ->
                let typ_name = Path.name path in
                let kind = Type_decl.TKRecord(List.map (fun (s,f,typ) -> s,(from_mutable_flag f,from_type_expr env typ)) fields) in
                  Type_decl.add_typ_decl typ_name kind
        end
    | Tobject _ -> unsupported "Tobject"
    | Tfield _ -> unsupported "Tfield"
    | Tnil -> unsupported "Tnil"
    | Tlink _ -> unsupported "Tlink"
    | Tsubst _ ->  unsupported "Tsubst"
    | Tvariant _ -> unsupported "Tvariant"
    | Tunivar -> unsupported "Tunivar"
    | Tpoly _ -> unsupported "Tpoly"
    | Tpackage _ -> unsupported "Tpackage"

let add_exc_env cstr_desc env =
  let typ_name =
    match cstr_desc.cstr_res.Types.desc with
        Tconstr(path,_,_) -> Path.name path
      | _ -> assert false
  in
    if typ_name = "exn"
    then
      let name = get_constr_name cstr_desc cstr_desc.cstr_res env in
      let typs = List.map (from_type_expr env) cstr_desc.cstr_args in
        Type_decl.add_exc_decl name typs

let rec from_pattern {Typedtree.pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=env} =
  add_type_env env typ;
  let typ' = from_type_expr env typ in
  let desc =
    match desc with
        Tpat_any -> PAny
      | Tpat_var x -> PVar(from_ident x typ')
      | Tpat_alias(p,x) -> PAlias(from_pattern p, from_ident x typ')
      | Tpat_constant(Const_int n) -> PConst {desc=Const(Int n);typ=typ'}
      | Tpat_constant(Const_char c) -> PConst {desc=Const(Char c);typ=typ'}
      | Tpat_constant(Const_string s) -> PConst {desc=Const(String s);typ=typ'}
      | Tpat_constant(Const_float s) -> PConst {desc=Const(Float s);typ=typ'}
      | Tpat_constant(Const_int32 n) -> PConst {desc=Const(Int32 n);typ=typ'}
      | Tpat_constant(Const_int64 n) -> PConst {desc=Const(Int64 n);typ=typ'}
      | Tpat_constant(Const_nativeint n) -> PConst {desc=Const(Nativeint n);typ=typ'}
      | Tpat_tuple [] -> assert false
      | Tpat_tuple(p::ps) ->
          let aux p1 p2 =
            let p2' = from_pattern p2 in
              {pat_desc=PPair(p1,p2'); pat_typ=TPair(Id.new_var "x" p1.pat_typ, p2'.pat_typ)}
          in
            (List.fold_left aux (from_pattern p) ps).pat_desc
      | Tpat_construct(cstr_desc, []) when get_constr_name cstr_desc typ env = "()" -> PConst {desc=Const Unit;typ=typ'}
      | Tpat_construct(cstr_desc, []) when get_constr_name cstr_desc typ env = "[]" -> PNil
      | Tpat_construct(cstr_desc, [p1;p2]) when get_constr_name cstr_desc typ env = "::" ->
          PCons(from_pattern p1, from_pattern p2)
      | Tpat_construct(cstr_desc, ps) ->
          let name = get_constr_name cstr_desc typ env in
            add_exc_env cstr_desc env;
            (*
              let path = match (Ctype.repr typ).desc with Tconstr(path,_,_) -> path in
              let typ_name = Path.name path in
              let typ =
              if typ_name = "exn"
              then TVariant[name, List.map (from_type_expr env []) cstr_desc.cstr_args]
              else
              match from_type_declaration env (Env.find_type path env) with
              _, KVariant stypss -> TVariant stypss
              | _ -> assert false
              in
              add_type_env (KeyLabelResult name) (TConstr(typ_name,true));
              add_type_env (KeyLabelArg name) typ;
              add_type_env (KeyTypeEntity typ_name) typ;
            *)
            PConstruct(name, List.map from_pattern ps)
      | Tpat_variant _ -> unsupported "pattern match (variant)"
      | Tpat_record pats ->
          let aux1 (lbl,p) = lbl.lbl_pos, (get_label_name lbl env, from_mutable_flag lbl.lbl_mut, from_pattern p) in
            (*
              let typs =
              let labels = Array.to_list (fst (List.hd pats)).lbl_all in
              let aux lbl =
              let name = get_label_name lbl env in
              let flag = from_mutable_flag lbl.lbl_mut in
              name, (flag, from_type_expr env [] lbl.lbl_arg)
              in
              List.map aux labels
              in
              let name = match (Ctype.repr typ).desc with Tconstr(path,_,_) -> Path.name path in
              let aux2 (s,(_,typ)) =
              add_type_env (KeyLabelResult s) (TConstr(name,true));
              add_type_env (KeyLabelArg s) typ;
              in
              List.iter aux2 typs;
              add_type_env (KeyTypeEntity name) (TRecord(false, typs));
            *)
            PRecord (List.map aux1 pats)
      | Tpat_array _ -> unsupported "pattern match (array)"
      | Tpat_or(p1,p2,None) -> POr(from_pattern p1, from_pattern p2)
      | Tpat_or(_,_,Some _) -> unsupported "pattern match (or) where row = Some _"
      | Tpat_lazy _ -> unsupported "pattern match (lazy)"
  in
    {pat_desc=desc; pat_typ=typ'}


let from_value_kind = function
    Types.Val_reg -> Format.printf "Val_reg@."; assert false
  | Types.Val_prim prim_desc -> Id.new_var (prim_desc.Primitive.prim_name)
  | Types.Val_ivar _ -> Format.printf "Val_ivar@."; assert false
  | Types.Val_self _ -> Format.printf "Val_self@."; assert false
  | Types.Val_anc _ -> Format.printf "Val_anc@."; assert false
  | Types.Val_unbound -> Format.printf "Val_unbound@."; assert false

let from_constant = function
    Const_int n -> Int n
  | Const_char c -> Char c
  | Const_string s -> String s
  | Const_float x -> Float x
  | Const_int32 n -> Int32 n
  | Const_int64 n -> Int64 n
  | Const_nativeint n -> Nativeint n


let rec from_expression {exp_desc=exp_desc; exp_loc=_; exp_type=typ; exp_env=env} =
  add_type_env env typ;
  let typ' = from_type_expr env typ in
    match exp_desc with
        Texp_ident(path, _) ->
          make_var (from_ident_path path typ')
      | Texp_constant c ->
          {desc = Const (from_constant c); typ = typ'}
      | Texp_let(rec_flag, [p,e1], e2)
            when (function {pat_desc=PVar _} -> false | _ -> true) (from_pattern p) ->
          let p' = from_pattern p in
          let t1 = from_expression e1 in
          let t2 = from_expression e2 in
            make_single_match t1 p' t2
      | Texp_let(rec_flag, pats, e) ->
          let flag = from_rec_flag rec_flag in
          let aux (p,e) =
            let p' = from_pattern p in
            let e' = from_expression e in
              match p'.pat_desc with
                  PVar x -> x, [], e'
                | _ ->
                    if flag = Recursive
                    then raise (Fatal "Only variables are allowed as left-hand side of 'let rec'")
                    else unsupported "Only variables are allowed as left-hand side of 'let ... and ...'"
          in
          let bindings = List.map aux pats in
          let t = from_expression e in
            make_let_f flag bindings t
      | Texp_function([{Typedtree.pat_desc=Tpat_var x},e],Total) ->
          begin
            match e.exp_desc, from_type_expr env typ with
                Texp_when _,_ -> unsupported "???"
              | _,TFun({Id.typ=typ1},typ2) ->
                  let x' = from_ident x typ1 in
                    make_fun x' (from_expression e)
              | _ -> assert false
          end
      | Texp_function(pats,totality) ->
          let x,typ2 =
            match typ' with
                TFun(x,typ2) -> x,typ2
              | _ -> assert false
          in
          let aux (p,e) =
            match e.exp_desc with
                Texp_when(e1,e2) -> from_pattern p, from_expression e1, from_expression e2
              | _ -> from_pattern p, true_term, from_expression e
          in
          let tail =
            match totality with
                Total -> []
              | Partial ->
                  let p = {pat_desc=PVar(Id.new_var "u" (Id.typ x)); pat_typ=Id.typ x} in
                  let t =
                    make_let [Id.new_var "u" TUnit, [], make_app fail_term [unit_term]] (make_bottom typ2)
                  in
                    [p, true_term, t]
          in
            make_fun x {desc=Match({desc=Var x;typ=Id.typ x}, List.map aux pats@tail);typ=typ2}
      | Texp_apply(e, es) ->
          let t = from_expression e in
          let aux = function
              _, Optional -> unsupported "expression (optional)"
            | None, Required -> unsupported "???"
            | Some e, Required -> from_expression e
          in
          let ts = List.map aux es in
            conv_primitive_app t ts typ'
      | Texp_match(e,pats,tp) ->
          let t = from_expression e in
          let aux (p,e) =
            match e.exp_desc with
                Texp_when(e1,e2) -> from_pattern p, from_expression e1, from_expression e2
              | _ -> from_pattern p, true_term, from_expression e
          in
          let pats' = List.map aux pats in
          let pats'' =
            match tp with
                Total -> pats'
              | Partial ->
                  let p = {pat_desc=PVar(Id.new_var "u" t.typ); pat_typ=t.typ} in
                  let u = Id.new_var "u" TUnit in
                  let t = make_let [u, [], make_app fail_term [unit_term]] (make_bottom typ') in
                    pats'@[p, true_term, t]
          in
            {desc=Match(t, pats''); typ=typ'}
      | Texp_try(e,pats) ->
          let aux (p,e) =
            match e.exp_desc with
                Texp_when(e1,e2) -> from_pattern p, from_expression e1, from_expression e2
              | _ -> from_pattern p, true_term, from_expression e
          in
          let x = Id.new_var "e" !typ_excep in
          let pats' = List.map aux pats in
          let pats'' = pats' @ [make_pany !typ_excep, true_term, {desc=Raise(make_var x); typ=typ'}] in
            {desc=TryWith(from_expression e, make_fun x {desc=Match(make_var x, pats''); typ=typ'}); typ=typ'}
      | Texp_tuple(e::es) ->
          let t = from_expression e in
            List.fold_left (fun t e -> make_pair t (from_expression e)) t es
      | Texp_tuple _ -> assert false
      | Texp_construct(desc,es) ->
          let desc =
            match get_constr_name desc typ env, es with
                "()",[] -> Const Unit
              | "true",[] -> Const True
              | "false",[] -> Const False
              | "[]",[] -> Nil
              | "::",[e1;e2] -> Cons(from_expression e1, from_expression e2)
              | name,es ->
                  add_exc_env desc env;
                  Constr(name, List.map from_expression es)
          in
            {desc=desc; typ=typ'}
      | Texp_variant _ -> unsupported "expression (variant)"
      | Texp_record(fields,None) ->
          let fields' = List.sort ~cmp:(fun (lbl1,_) (lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos) fields in
          let aux (label,e) =
            get_label_name label env, (from_mutable_flag label.lbl_mut, from_expression e)
          in
          let fields'' = List.map aux fields' in
            {desc=Record fields''; typ=typ'}
      | Texp_record(fields, Some init) ->
          let labels = Array.to_list (fst (List.hd fields)).lbl_all in
          let r = Id.new_var "r" typ' in
          let fields' =
            let aux lbl =
              let name = get_label_name lbl env in
              let flag = from_mutable_flag lbl.lbl_mut in
                try
                  name, (flag, from_expression (List.assoc lbl fields))
                with Not_found ->
                  name, (flag, {desc=Proj(lbl.lbl_pos, name, flag,
                                          {desc=Var r; typ=Id.typ r});
                                typ=from_type_expr env lbl.lbl_arg})
            in
              List.map aux labels
          in
            make_let [r, [], from_expression init] {desc=Record fields';typ=typ'}
      | Texp_field(e,label) ->
          {desc=Proj(label.lbl_pos,
                     get_label_name label env,
                     from_mutable_flag label.lbl_mut,
                     from_expression e);
           typ=typ'}
      | Texp_setfield(e1,label,e2) ->
          {desc=SetField(None,
                         label.lbl_pos,
                         get_label_name label env,
                         from_mutable_flag label.lbl_mut,
                         from_expression e1,
                         from_expression e2);
           typ=typ'}
      | Texp_array _ -> unsupported "expression (array)"
      | Texp_ifthenelse(e1,e2,e3) ->
          let t1 = from_expression e1 in
          let t2 = from_expression e2 in
          let t3 =
            match e3 with
                None -> {desc=Const Unit; typ=TUnit}
              | Some e3 -> from_expression e3
          in
            make_if t1 t2 t3
      | Texp_sequence(e1,e2) -> make_seq (from_expression e1) (from_expression e2)
      | Texp_while _ -> unsupported "expression (while)"
      | Texp_for _ -> unsupported "expression (for)"
      | Texp_when _ -> unsupported "expression (when)"
      | Texp_send _
      | Texp_new _ -> unsupported "expression (class)"
      | Texp_instvar _ -> unsupported "expression (instvar)"
      | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
      | Texp_override _ -> unsupported "expression (override)"
      | Texp_letmodule _ -> unsupported "expression (module)"
      | Texp_assert e -> make_assert (from_expression e)
      | Texp_assertfalse -> make_seq (make_assert false_term) (make_bottom typ')
      | Texp_lazy e -> assert false
      | Texp_object _ -> unsupported "expression (class)"
      | Texp_pack _ -> unsupported "expression (pack)"




let from_exception_declaration env = List.map (from_type_expr env)


let from_top_level_phrase (env,defs) = function
    Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  | Parsetree.Ptop_def struc ->
      let struc,_,env' = Typemod.type_structure env struc Location.none in
      let aux2 = function
          Tstr_eval e ->
            let t = from_expression e in
              [Decl_let(Nonrecursive, [Id.new_var "u" t.typ, t])]
        | Tstr_value(rec_flag,pats) ->
            let flag = from_rec_flag rec_flag in
            let aux (p,e) =
              let p' = from_pattern p in
              let e' = from_expression e in
                match p'.pat_desc with
                    PVar x -> x, e'
                  | _ ->
                      if flag = Recursive
                      then raise (Fatal "Only variables are allowed as left-hand side of 'let rec'")
                      else unsupported "Only variables are allowed as left-hand side of 'let'"
            in
              [Decl_let(flag, List.map aux pats)]
        | Tstr_primitive _ -> unsupported "external"
        | Tstr_type decls -> []
        | Tstr_exception(x,exc_decl) -> []
        | Tstr_exn_rebind _ -> unsupported "exception rebind"
        | Tstr_module _
        | Tstr_recmodule _
        | Tstr_modtype _ -> unsupported "module"
        | Tstr_open _ -> unsupported "open"
        | Tstr_class _
        | Tstr_cltype _ -> unsupported "class"
        | Tstr_include _ -> unsupported "include"
      in
        env', rev_map_flatten aux2 struc @@@ defs


let from_use_file ast =
  let _,defs = List.fold_left from_top_level_phrase (initial_env,[]) ast in
  let aux t = function
      Decl_let(flag, defs) ->
        let defs' = List.map (fun (f,t1) -> f, [], t1) defs in
          make_let_f flag defs' t
    | Decl_type _ -> t
    | Decl_exc _ -> t
  in
  let t = List.fold_left aux {desc=Const Unit;typ=TUnit} defs in
    Trans.merge_let_fun t
