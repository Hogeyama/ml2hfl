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


let () = Compmisc.init_path false


let prim_typs =
  ["unit", TUnit;
   "bool", TBool;
   "int", TInt;
   "Pervasives.format", TData("string", false);
(*
   "Pervasives.in_channel", TUnit
*)]

let conv_primitive_app t ts typ =
  match t.desc,ts with
  | Var {Id.name="List.length"}, [t1] -> make_length t1
  | Var {Id.name="Pervasives.@@"}, [t1;t2] -> make_app t1 [t2]
  | Var {Id.name="Pervasives.="}, [t1;t2] -> make_eq t1 t2
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
  | Var {Id.name="Pervasives.raise"}, [t] -> {desc=Raise(t); typ=typ; attr=[]}
  | Var {Id.name="Pervasives.ref"}, [t] -> make_ref t
  | Var {Id.name="Pervasives.read_int"}, [{desc=Const Unit}] ->
      let attr = if !Flag.mode = Flag.NonTermination then AAbst_under::randint_term.attr else randint_term.attr in
      make_app {randint_term with attr} [unit_term]
  | Var {Id.name="Pervasives.!"}, [t] -> make_deref t
  | Var {Id.name="Pervasives.:="}, [t1;t2] -> make_setref t1 t2
  | Var {Id.name="Random.bool"}, [{desc=Const Unit}] -> make_eq (make_app randint_term [unit_term]) (make_int 0)
  | Var {Id.name="Random.int"}, [{desc=Const (Int 0)}] -> randint_unit_term
  | Var {Id.name="Random.int"}, [t] ->
      let x = Id.new_var ~name:"n" TInt in
      make_let [x, [], randint_unit_term] @@
        make_assume
          (make_and (make_leq (make_int 0) (make_var x)) (make_lt (make_var x) t))
          (make_var x)
  | Var {Id.name="Pervasives.open_in"}, [{desc=Const(Int _)}] -> make_app (make_event "newr") [unit_term]
  | Var {Id.name="Pervasives.close_in"}, [{typ=TUnit}] -> make_app (make_event "close") [unit_term]
  | _ -> make_app t ts

let venv = ref []

let rec from_type_expr tenv typ =
  let typ' = Ctype.repr typ in
  match typ'.Types.desc with
  | Tvar _ ->
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
      let x = Id.new_var typ1' in
      TFun(x, typ2')
  | Ttuple typs ->
      TTuple (List.map (Id.new_var -| from_type_expr tenv) typs)
  | Tconstr(path, _, _) when List.mem_assoc (Path.name path) prim_typs ->
      List.assoc (Path.name path) prim_typs
  | Tconstr(path, [type_expr], _) when Path.name path = "list" ->
      TList (from_type_expr tenv type_expr)
  | Tconstr(path, [type_expr], _) when Path.name path = "Pervasives.ref" ->
      TRef (from_type_expr tenv type_expr)
  | Tconstr(path, [type_expr], _) when Path.name path = "option" ->
      TOption (from_type_expr tenv type_expr)
  | Tconstr(path, [type_expr], _) when Path.name path = "array" ->
      TRef (make_tpair TInt @@ TFun(Id.new_var TInt, from_type_expr tenv type_expr))
  | Tconstr(path, typs, m) ->
      let b =
        try
          match (Env.find_type path tenv).type_kind with
          | Type_abstract -> false
          | Type_variant _ -> true
          | Type_record _ -> true
          | Type_open -> unsupported "Type_open"
        with Not_found -> false
      in
      let s = Path.name path in
      TData(s, b)
  | Tobject _ -> unsupported "Tobject"
  | Tfield _ -> unsupported "Tfield"
  | Tnil -> unsupported "Tnil"
  | Tlink _ -> unsupported "Tlink"
  | Tsubst _ -> unsupported "Tsubst"
  | Tvariant _ -> unsupported "Tvariant"
  | Tunivar _ -> unsupported "Tunivar"
  | Tpoly _ -> unsupported "Tpoly"
  | Tpackage _ -> unsupported "Tpackage"


let from_rec_flag = function
  | Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive

let from_mutable_flag = function
  | Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable



let sign_to_letters s =
  let is_op s = String.contains "!$%&*+-./:<=>?@^|~" s.[0] in
  let map = function
    | '!' -> "_bang"
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
  if is_op s
  then "op" ^ String.join "" @@ List.map map @@ String.explode s
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
  desc.cstr_name

let get_label_name label env =
  label.lbl_name

let from_type_kind tenv type_kind =
  match type_kind with
  | Type_abstract -> KAbstract
  | Type_variant decls ->
      let aux {cd_id;cd_args} =
        Ident.name cd_id, List.map (from_type_expr tenv) cd_args
      in
      KVariant(List.map aux decls)
  | Type_record(decls,_) ->
      let aux {ld_id;ld_mutable;ld_type} =
        Ident.name ld_id, (from_mutable_flag ld_mutable, from_type_expr tenv ld_type)
      in
      KRecord (List.map aux decls)
  | Type_open -> unsupported "Type_open"

let rec from_type_declaration tenv decl =
  let venv = List.map (fun typ -> typ.Types.id, TVar (ref None)) decl.type_params in
  let params = List.map snd venv in
  let kind = from_type_kind tenv decl.type_kind in
  params, kind


let rec add_type_env env typ =
  match (Ctype.repr typ).Types.desc with
  | Tvar _ -> ()
  | Tarrow(_,typ1,typ2,_) -> add_type_env env typ1; add_type_env env typ2
  | Ttuple typs -> List.iter (add_type_env env) typs
  | Tconstr(path,typs,_) when Path.name path = "list" -> List.iter (add_type_env env) typs
  | Tconstr(path,typs,_) when Path.name path = "Pervasives.ref" -> List.iter (add_type_env env) typs
  | Tconstr(path,typs,_) when Path.name path = "option" -> List.iter (add_type_env env) typs
  | Tconstr(path,typs,_) ->
      let {type_kind} = Env.find_type path env in
      if type_kind <> Type_abstract then
        let typ_name = Path.name path in
        Type_decl.add_typ_decl typ_name @@ Type_decl.from_type_kind @@ from_type_kind env type_kind
  | Tobject _ -> unsupported "Tobject"
  | Tfield _ -> unsupported "Tfield"
  | Tnil -> unsupported "Tnil"
  | Tlink _ -> unsupported "Tlink"
  | Tsubst _ ->  unsupported "Tsubst"
  | Tvariant _ -> unsupported "Tvariant"
  | Tunivar _ -> unsupported "Tunivar"
  | Tpoly _ -> unsupported "Tpoly"
  | Tpackage _ -> unsupported "Tpackage"

let add_exc_env cstr_desc env =
  let typ_name =
    match cstr_desc.cstr_res.Types.desc with
    | Tconstr(path,_,_) -> Path.name path
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
    | Tpat_any -> PAny
    | Tpat_var(x,_) -> PVar(from_ident x typ')
    | Tpat_alias({Typedtree.pat_desc=Tpat_any},x,_) -> PVar (from_ident x typ')
    | Tpat_alias(p,x,_) -> PAlias(from_pattern p, from_ident x typ')
    | Tpat_constant(Const_int n) -> PConst {desc=Const(Int n);typ=typ'; attr=[]}
    | Tpat_constant(Const_char c) -> PConst {desc=Const(Char c);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,None)) -> PConst {desc=Const(String s);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,Some _)) -> unsupported "Const_string Some"
    | Tpat_constant(Const_float s) -> PConst {desc=Const(Float s);typ=typ'; attr=[]}
    | Tpat_constant(Const_int32 n) -> PConst {desc=Const(Int32 n);typ=typ'; attr=[]}
    | Tpat_constant(Const_int64 n) -> PConst {desc=Const(Int64 n);typ=typ'; attr=[]}
    | Tpat_constant(Const_nativeint n) -> PConst {desc=Const(Nativeint n);typ=typ'; attr=[]}
    | Tpat_tuple ps -> PTuple (List.map from_pattern ps)
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "None" -> PNone
    | Tpat_construct(_, cstr_desc, [p]) when get_constr_name cstr_desc typ env = "Some" -> PSome (from_pattern p)
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "()" -> PConst unit_term
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "[]" -> PNil
    | Tpat_construct(_, cstr_desc, [p1;p2]) when get_constr_name cstr_desc typ env = "::" ->
        PCons(from_pattern p1, from_pattern p2)
    | Tpat_construct(_, cstr_desc, ps) ->
        let name = get_constr_name cstr_desc typ env in
        add_exc_env cstr_desc env;
        PConstruct(name, List.map from_pattern ps)
    | Tpat_variant _ -> unsupported "pattern match (variant)"
    | Tpat_record(pats,_) ->
        let aux1 (_,lbl,p) = get_label_name lbl env, from_pattern p in
        PRecord (List.map aux1 pats)
    | Tpat_array _ -> unsupported "pattern match (array)"
    | Tpat_or(p1,p2,None) -> POr(from_pattern p1, from_pattern p2)
    | Tpat_or(_,_,Some _) -> unsupported "pattern match (or) where row = Some _"
    | Tpat_lazy _ -> unsupported "pattern match (lazy)"
  in
  {pat_desc=desc; pat_typ=typ'}



let from_value_kind = function
  | Types.Val_reg -> Format.printf "Val_reg@."; assert false
  | Types.Val_prim prim_desc -> Id.new_var (prim_desc.Primitive.prim_name)
  | Types.Val_ivar _ -> Format.printf "Val_ivar@."; assert false
  | Types.Val_self _ -> Format.printf "Val_self@."; assert false
  | Types.Val_anc _ -> Format.printf "Val_anc@."; assert false
  | Types.Val_unbound -> Format.printf "Val_unbound@."; assert false

let from_constant = function
  | Const_int n -> Int n
  | Const_char c -> Char c
  | Const_string(s, None) -> String s
  | Const_string(s, Some _) -> unsupported "Const_string Some"
  | Const_float x -> Float x
  | Const_int32 n -> Int32 n
  | Const_int64 n -> Int64 n
  | Const_nativeint n -> Nativeint n


let rec from_expression {exp_desc=exp_desc; exp_loc=_; exp_type=typ; exp_env=env} =
  add_type_env env typ;
  let typ' = from_type_expr env typ in
  match exp_desc with
  | Texp_ident(path, _, _) ->
      make_var @@ from_ident_path path typ'
  | Texp_constant c ->
      {desc = Const (from_constant c); typ = typ'; attr=[]}
  | Texp_let(rec_flag, [{vb_pat;vb_expr}], e2)
       when (function {pat_desc=PVar _} -> false | _ -> true) (from_pattern vb_pat) ->
      let p' = from_pattern vb_pat in
      let t1 = from_expression vb_expr in
      let t2 = from_expression e2 in
      make_single_match t1 p' t2
  | Texp_let(rec_flag, pats, e) ->
      let flag = from_rec_flag rec_flag in
      let aux {vb_pat;vb_expr} =
        let p' = from_pattern vb_pat in
        let e' = from_expression vb_expr in
        match p'.pat_desc with
        | PVar x -> x, [], e'
        | _ ->
            if flag = Recursive
            then unsupported "Only variables are allowed as left-hand side of 'let rec'"
            else unsupported "Only variables are allowed as left-hand side of 'let ... and ...'"
      in
      let bindings = List.map aux pats in
      let t = from_expression e in
      make_let_f flag bindings t
  | Texp_function(_,[case],Total)
       when (function ({pat_desc=PVar _},t,_) -> t = true_term | _ -> false) @@ from_case case ->
      begin
        match from_case case with
        | ({pat_desc=PVar x}, _, e) -> make_fun x e
        | _ -> assert false
      end
  | Texp_function(_,pats,totality) ->
      let x,typ2 =
        match typ' with
        | TFun(x,typ2) ->
            let x' = (* for readable variable names *)
              match (from_pattern (List.hd pats).c_lhs).pat_desc with
              | PTuple ps ->
                  let xs = List.map (function {pat_desc=PVar x} -> Some x | _ -> None) ps in
                  if List.for_all Option.is_some xs
                  then
                    xs
                    |> List.map (Id.name -| Option.get)
                    |> String.join "__"
                    |> Id.set_name x
                  else x
              | _ -> x
            in
            x', typ2
        | _ -> assert false
      in
      let pats' = List.map from_case pats in
      let pats'' =
        match totality with
        | Total -> pats'
        | Partial -> (make_pvar (Id.new_var_id x), true_term, make_fail typ2)::pats'
      in
      begin
        match pats'' with
        | [{pat_desc=PAny},{desc=Const True},t'] -> make_fun x t'
        | [{pat_desc=PVar y},{desc=Const True},t'] -> make_fun x @@ subst_var y x t'
        | [{pat_desc=PConst{desc=Const Unit}},{desc=Const True},t'] -> make_fun x t'
        | _ -> make_fun x @@ make_match (make_var x) pats''
      end
  | Texp_apply(e, es) ->
      let t = from_expression e in
      let aux = function
        | _, _, Optional -> unsupported "expression (optional)"
        | _, None, Required -> unsupported "???"
        | _, Some e, Required -> from_expression e
      in
      let ts = List.map aux es in
      conv_primitive_app t ts typ'
  | Texp_match(e,pats,[],tp) ->
      let t = from_expression e in
      let pats' = List.map from_case pats in
      let pats'' =
        match tp with
        | Total -> pats'
        | Partial -> pats'@[make_pvar (Id.new_var t.typ), true_term, make_fail typ']
      in
      make_match t pats''
  | Texp_match(e,pats,_,tp) -> unsupported "Texp_match (exception)"
  | Texp_try(e,pats) ->
      let x = Id.new_var ~name:"e" !typ_excep in
      let pats' = List.map from_case pats in
      let pats'' = pats' @ [make_pany !typ_excep, true_term, {desc=Raise(make_var x); typ=typ'; attr=[]}] in
      {desc=TryWith(from_expression e, make_fun x (make_match (make_var x) pats'')); typ=typ'; attr=[]}
  | Texp_tuple es ->
      {desc=Tuple(List.map from_expression es); typ=typ'; attr=[]}
  | Texp_construct(_,desc,es) ->
      let desc =
        match get_constr_name desc typ env, es with
        | "()",[] -> Const Unit
        | "true",[] -> Const True
        | "false",[] -> Const False
        | "[]",[] -> Nil
        | "::",[e1;e2] -> Cons(from_expression e1, from_expression e2)
        | "None",[] -> TNone
        | "Some",[e] -> TSome (from_expression e)
        | name,es ->
            add_exc_env desc env;
            Constr(name, List.map from_expression es)
      in
      {desc=desc; typ=typ'; attr=[]}
  | Texp_variant _ -> unsupported "expression (variant)"
  | Texp_record(fields,None) ->
      let fields' = List.sort ~cmp:(fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos) fields in
      let aux (_,label,e) = get_label_name label env, from_expression e in
      let fields'' = List.map aux fields' in
      {desc=Record fields''; typ=typ'; attr=[]}
  | Texp_record(fields, Some init) ->
      let labels = Array.to_list (Triple.snd @@ List.hd fields).lbl_all in
      let r = Id.new_var ~name:"r" typ' in
      let fields' =
        let aux lbl =
          let name = get_label_name lbl env in
          try
            let _,_,e = List.find (fun (_,lbl',_) -> lbl = lbl') fields in
            name, from_expression e
          with Not_found ->
            name, {desc=Field(name, make_var r); typ=from_type_expr env lbl.lbl_arg; attr=[]}
        in
        List.map aux labels
      in
      make_let [r, [], from_expression init] {desc=Record fields';typ=typ'; attr=[]}
  | Texp_field(e,_,label) ->
      let t = from_expression e in
      {desc=Field(get_label_name label env, t); typ=typ'; attr=make_attr[t]}
  | Texp_setfield(e1,_,label,e2) ->
      {desc=SetField(get_label_name label env, from_expression e1, from_expression e2); typ=typ'; attr=[]}
  | Texp_array es ->
      let typ'' =
        match (Ctype.full_expand env @@ Ctype.repr typ).Types.desc with
        | Tconstr(path, [type_expr], _) when Path.name path = "array" ->
            from_type_expr env type_expr
        | _ -> assert false
      in
      let ts = List.map from_expression es in
      let n = List.length ts in
      let len = make_int n in
      let i = Id.new_var ~name:"i" TInt in
      let ti = make_var i in
      let t = List.fold_right (fun j t -> t) (List.init n Fun.id) (make_bottom typ'') in
      let t' = make_seq (make_assert @@ make_and (make_leq (make_int 0) ti) (make_lt ti len)) t in
      make_ref @@ make_pair len @@ make_fun i t'
  | Texp_ifthenelse(e1,e2,e3) ->
      let t1 = from_expression e1 in
      let t2 = from_expression e2 in
      let t3 =
        match e3 with
          None -> {desc=Const Unit; typ=TUnit; attr=[]}
        | Some e3 -> from_expression e3
      in
      make_if t1 t2 t3
  | Texp_sequence(e1,e2) -> make_seq (from_expression e1) (from_expression e2)
  | Texp_while(e1,e2) ->
      let t1 = from_expression e1 in
      let t2 = from_expression e2 in
      let x = Id.new_var ~name:"u" TUnit in
      let f = Id.new_var ~name:"while" (TFun(Id.new_var ~name:"u" TUnit, t2.typ)) in
      let t2' = make_if t1 (make_seq t2 @@ make_app (make_var f) [unit_term]) unit_term in
      make_letrec [f, [x], t2'] @@ make_app (make_var f) [unit_term]
  | Texp_for(x, _, e1, e2, dir, e3) ->
      let t1 = from_expression e1 in
      let t2 = from_expression e2 in
      let t3 = from_expression e3 in
      let x' = from_ident x TInt in
      let f = Id.new_var ~name:"for" (TFun(Id.new_var ~name:"i" TInt, t3.typ)) in
      let init = Id.new_var ~name:"init" TInt in
      let last = Id.new_var ~name:"last" TInt in
      let t31 =
        match dir with
        | Upto -> make_leq (make_var x') (make_var last)
        | Downto -> make_geq (make_var x') (make_var last)
      in
      let x'' =
        match dir with
        | Upto -> make_add (make_var x') (make_int 1)
        | Downto -> make_sub (make_var x') (make_int 1)
      in
      let t32 = make_seq t3 @@ make_app (make_var f) [x''] in
      let t3' = make_if t31 t32 unit_term in
      make_letrec [f, [x'], t3'] @@ make_lets [init,[],t1; last,[],t2] @@ make_app (make_var f) [make_var init]
  | Texp_send _
  | Texp_new _ -> unsupported "expression (class)"
  | Texp_instvar _ -> unsupported "expression (instvar)"
  | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
  | Texp_override _ -> unsupported "expression (override)"
  | Texp_letmodule _ -> unsupported "expression (module)"
  | Texp_assert e ->
      let e' = from_expression e in
      if e' = false_term
      then make_seq (make_assert false_term) (make_bottom typ')
      else make_assert e'
  | Texp_lazy e -> assert false
  | Texp_object _ -> unsupported "expression (class)"
  | Texp_pack _ -> unsupported "expression (pack)"

and from_case {c_lhs;c_guard;c_rhs} =
  from_pattern c_lhs,
  Option.map_default from_expression true_term c_guard,
  from_expression c_rhs


let from_exception_declaration env = List.map (from_type_expr env)


let from_top_level_phrase (env,defs) = function
  | Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  | Parsetree.Ptop_def struc ->
      let struc',_,env' = Typemod.type_structure env struc Location.none in
      let aux2 str_item =
        match str_item.str_desc with
        | Tstr_eval(e,_) ->
            let t = from_expression e in
            [Decl_let(Nonrecursive, [Id.new_var ~name:"u" t.typ, t])]
        | Tstr_value(rec_flag,pats) ->
            let flag = from_rec_flag rec_flag in
            let aux {vb_pat;vb_expr} =
              let p = from_pattern vb_pat in
              let e = from_expression vb_expr in
              match p.pat_desc, flag with
              | PVar x, _ -> x, e
              | _, Recursive -> fatal "Only variables are allowed as left-hand side of 'let rec'"
              | _, Nonrecursive -> unsupported "Only variables are allowed as left-hand side of 'let'"
            in
            [Decl_let(flag, List.map aux pats)]
        | Tstr_primitive _ -> []
        | Tstr_type _ -> []
        | Tstr_typext _ -> unsupported "typext"
        | Tstr_exception _ -> []
        | Tstr_module _
        | Tstr_recmodule _
        | Tstr_modtype _ -> unsupported "module"
        | Tstr_open _ -> []
        | Tstr_class _
        | Tstr_class_type _ -> unsupported "class"
        | Tstr_include _ -> unsupported "include"
        | Tstr_attribute _ -> unsupported "attribute"
      in
      env', List.rev_map_flatten aux2 struc'.str_items @@@ defs


let from_use_file ast =
  let env = Compmisc.initial_env () in
  let _,defs = List.fold_left from_top_level_phrase (env,[]) ast in
  let aux t = function
    | Decl_let(flag, defs) ->
        let defs' = List.map (fun (f,t1) -> f, [], t1) defs in
        make_let_f flag defs' t
    | Decl_type _ -> t
    | Decl_exc _ -> t
  in
  let t = List.fold_left aux unit_term defs in
  Trans.merge_let_fun t
