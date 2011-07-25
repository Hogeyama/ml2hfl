
open Utilities
open Asttypes
open Typedtree
open Types
open Syntax
open Type


type declaration =
    Decl_let of Flag.rec_flag * (id * typed_term) list
  | Decl_type of (string * (typ list * type_kind)) list
  | Decl_exc of string * typ list


exception Unsupported of string

let unsupported s = raise (Unsupported s)


let () = Config.load_path := Flag.ocaml_lib @ !Config.load_path
let initial_env = Compile.initial_env ()


let rec from_type_expr tenv venv typ =
  let typ' = Ctype.repr typ in
    match typ'.Types.desc with
        Tvar ->
          begin
            try
              venv, List.assoc typ'.Types.id venv 
            with Not_found ->
              let x = TVar (ref None) in
                (typ'.Types.id, x)::venv, x
          end
      | Tarrow(_, typ1, typ2, _) ->
          let venv1,typ1' = from_type_expr tenv venv typ1 in
          let venv2,typ2' = from_type_expr tenv venv1 typ2 in
          let x = Id.new_var "x" typ1' in
            venv2, TFun(x, typ2')
      | Ttuple typs ->
          let aux (c,env,typs) typ =
            let env',typ' = from_type_expr tenv venv typ in
              c+1, env', typs @ [string_of_int c,(Flag.Immutable,typ')]
          in
          let _,env',typs = List.fold_left aux (0,venv,[]) typs in
            env', TRecord(true,typs)
      | Tconstr(path, [], _) when Path.name path = "unit" -> venv, TUnit
      | Tconstr(path, [], _) when Path.name path = "bool" -> venv, TBool
      | Tconstr(path, [], _) when Path.name path = "int" -> venv, TInt []
      | Tconstr(path, [], _) when Path.name path = "string" -> venv, TInt []
      | Tconstr(path, [type_expr], _) when Path.name path = "list" ->
          let env',typ' = from_type_expr tenv venv type_expr in
            env', TList(typ', [])
      | Tconstr(path, _, m) ->
          let b =
            try match (Env.find_type path tenv).type_kind with
                Type_abstract -> false
              | Type_variant _ -> true
              | Type_record _ -> true
            with Not_found -> true
          in
            venv, TConstr(Path.name path, b)
      | Tobject _ -> unsupported "Tobject"
      | Tfield _ -> unsupported "Tfield"
      | Tnil _ -> unsupported "Tnil"
      | Tlink _ -> unsupported "Tlink"
      | Tsubst _ -> unsupported "Tsubst"
      | Tvariant _ -> unsupported "Tvariant"
      | Tunivar _ -> unsupported "Tunivar"
      | Tpoly _ -> unsupported "Tpoly"
let from_type_expr tenv venv typ = snd (from_type_expr tenv venv typ)


let from_rec_flag = function
    Asttypes.Nonrecursive -> Flag.Nonrecursive
  | Asttypes.Recursive -> Flag.Recursive
  | Asttypes.Default -> unsupported "rec_flag (Default)"

let from_mutable_flag = function
    Asttypes.Mutable -> Flag.Mutable
  | Asttypes.Immutable -> Flag.Immutable


let from_ident x typ = Id.make (Ident.binding_time x) (Ident.name x) typ


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
  let venv = [] in
  let kind =
    match decl.type_kind with
        Type_abstract -> KAbstract
      | Type_variant stypss ->
          KVariant(List.map (fun (s,typs) -> s,List.map (from_type_expr tenv venv) typs) stypss)
      | Type_record(sftyps,_) ->
          KRecord(List.map (fun (s,flag,typ) -> s,(from_mutable_flag flag,from_type_expr tenv venv typ)) sftyps)
  in
    params, kind

(*
let rec add_type_env env typ =
  match (Ctype.repr typ).Types.desc with
      Tvar -> ()
    | Tarrow(_,typ1,typ2,_) -> add_type_env env typ1; add_type_env env typ2
    | Ttuple typs -> List.iter (add_type_env env) typs
    | Tconstr(path,_,_) when Typing.in_env (Typing.KeyTypeEntity (Path.name path)) -> ()
    | Tconstr(path,typs,_) ->
        begin
          match (Env.find_type path env).type_kind with
              Type_abstract -> ()
            | Type_variant stypss ->
                let typ_name = Path.name path in
                let typ = TVariant (List.map (fun (s,typs) -> s, List.map (from_type_expr env []) typs) stypss) in
                let aux (s,typs) =
                  List.iter (add_type_env env) typs;
                  Typing.add_type_env (Typing.KeyLabelResult s) (TConstr(typ_name,true))
                in
                  Typing.add_type_env (Typing.KeyTypeEntity typ_name) typ;
                  List.iter aux stypss
            | Type_record(fields,_) ->
                let typ_name = Path.name path in
                let typ = TRecord(false, List.map (fun (s,f,typ) -> s,(from_mutable_flag f,from_type_expr env [] typ)) fields) in
                let aux (s,_,typ) =
                  add_type_env env typ;
                  Typing.add_type_env (Typing.KeyLabelResult s) (TConstr(typ_name,true))
                in
                  Typing.add_type_env (Typing.KeyTypeEntity typ_name) typ;
                  List.iter aux fields
        end
    | Tobject _ -> unsupported "Tobject"
    | Tfield _ -> unsupported "Tfield"
    | Tnil -> unsupported "Tnil"
    | Tlink _ -> unsupported "Tlink"
    | Tsubst _ ->  unsupported "Tsubst"
    | Tvariant _ -> unsupported "Tvariant"
    | Tunivar -> ()
    | Tpoly _ -> unsupported "Tpoly"
    | Tpackage _ -> unsupported "Tpackage"

let add_exc_env cstr_desc env =
  let typ_name =
    match cstr_desc.cstr_res.desc with
        Tconstr(path,_,_) -> Path.name path
      | _ -> assert false
  in
    if typ_name = "exn"
    then
      let name = get_constr_name cstr_desc cstr_desc.cstr_res env in
        Typing.add_type_env (Typing.KeyLabelResult name) (TConstr("exn",true));
        Typing.add_exc_env name (List.map (from_type_expr env []) cstr_desc.cstr_args)
*)
let add_type_env env typ = ()
let add_exc_env cstr_desc env = ()

let rec from_pattern {Typedtree.pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=env} =
  add_type_env env typ;
  let typ' = from_type_expr env [] typ in
  let desc =
    match desc with
        Tpat_any -> PVar(Id.new_var "u" typ')
      | Tpat_var x -> PVar(from_ident x typ')
      | Tpat_alias _ -> unsupported "pattern match (alias)"
      | Tpat_constant(Const_int n) -> PConst {desc=Int n;typ=typ'}
      | Tpat_constant(Const_char c) -> unsupported "pattern match (char constant)"
      | Tpat_constant(Const_string s) -> unsupported "pattern match (string constant)"
      | Tpat_constant(Const_float x) -> unsupported "pattern match (float constant)"
      | Tpat_constant(Const_int32 n) -> unsupported "pattern match (int32 constant)"
      | Tpat_constant(Const_int64 n) -> unsupported "pattern match (int64 constant)"
      | Tpat_constant(Const_nativeint n) -> unsupported "pattern match (nativeint constant)"
      | Tpat_tuple ps ->
          let _,pats = List.fold_left (fun (c,pats) p -> c+1, pats @ [c, (string_of_int c, Flag.Immutable, from_pattern p)]) (0,[]) ps in
            PRecord(true,pats)
      | Tpat_construct(cstr_desc, []) when get_constr_name cstr_desc typ env = "()" -> PConst {desc=Unit;typ=typ'}
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
            PRecord(false, List.map aux1 pats)
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
  | Types.Val_unbound _ -> Format.printf "Val_unbound@."; assert false

let from_constant = function
    Const_int n -> Int n
  | Const_char _ -> unsupported "constant (char)"
  | Const_string s -> Scanf.sscanf (String.sub (Digest.to_hex (Digest.string s)) 0 6) "%x" (fun x -> Int x)
  | Const_float _ -> unsupported "constant (float)"
  | Const_int32 _ -> unsupported "constant (int32)"
  | Const_int64 _ -> unsupported "constant (int64)"
  | Const_nativeint _ -> unsupported "constant (nativeint)"


let rec get_bindings pat t =
  match pat.pat_desc, t.desc with
      PVar x, _ -> [x, t]
    | PRecord(b,pats), Record(b',fields) ->
        let aux (i,(_,_,p)) =
          let _,(_,t) = List.nth fields i in
            get_bindings p t
        in
          List.flatten (List.map aux pats)
    | PRecord(b,pats), Var x ->
        let n = if b then Some (List.length pats) else None in
        let aux (i,(s,f,p)) = get_bindings p {desc=Proj(n,i,s,f,{desc=Var x;typ=Id.typ x});typ=p.pat_typ} in
          List.flatten (List.map aux pats)
    | PRecord(b,pats), _ ->
        let x = Id.new_var "x" t.typ in
          (x,t) :: get_bindings {pat_desc=PRecord(b,pats);pat_typ=pat.pat_typ} {desc=Var x;typ=Id.typ x}
    | _ -> assert false

let rec from_expression x = match x with {exp_desc=exp_desc; exp_loc=_; exp_type=typ; exp_env=env} ->
  add_type_env env typ;
  let typ' = from_type_expr env [] typ in
  let desc =
    match exp_desc with
        Texp_ident(path, _) -> Var (Id.make (Path.binding_time path) (Path.name path) typ')
      | Texp_constant c -> from_constant c
      | Texp_let(rec_flag, [p,e1], e2) ->
        let flag = from_rec_flag rec_flag in
        let p' = from_pattern p in
        let t1 = from_expression e1 in
        let t2 = from_expression e2 in
        let bindings = get_bindings p' t1 in
        (List.fold_right (fun (x,t') t -> {desc=Let(flag,x,[],t',t);typ=typ'}) bindings t2).desc
      | Texp_let _ -> unsupported "Texp_let"
      (*
        | Texp_let(rec_flag, pats, e) ->
        let flag = from_rec_flag rec_flag in
        let bindings = List.map (fun (p,e) -> var_of_pattern p, [], from_expression e) pats in
        let t = from_expression e in
        Let(flag, bindings, t)
      *)
      | Texp_function([{Typedtree.pat_desc=Tpat_var x},e],Total) ->
        begin
          match e.exp_desc, from_type_expr env [] typ with
              Texp_when _,_ -> unsupported "???"
            | _,TFun({Id.typ=typ1},typ2) ->
              let x' = from_ident x typ1 in
              let f = Id.new_var "f" (TFun(x',typ2)) in
              Let(Flag.Nonrecursive, f, [x'], from_expression e, {desc=Var f;typ=Id.typ f})
            | _ -> assert false
        end
      | Texp_function(pes,Total) ->
        begin
          match typ' with
              TFun(x,typ2) ->
                let aux (p,e) = 
                  match e.exp_desc with
                      Texp_when(e1,e2) -> from_pattern p, Some (from_expression e1), from_expression e2
                    | _ -> from_pattern p, None, from_expression e
                in
                let f = Id.new_var "f" (TFun(x,typ2)) in
                Let(Flag.Nonrecursive, f, [x], {desc=Match_({desc=Var x;typ=Id.typ x}, List.map aux pes);typ=typ2}, {desc=Var f;typ=typ'})
            | _ -> assert false
        end
      | Texp_apply(e, es) ->
        let t = from_expression e in
        let aux = function
        _, Optional -> unsupported "expression (optional)"
          | None, Required -> unsupported "???"
          | Some e, Required -> from_expression e
        in
        let ts = List.map aux es in
        begin
          match t.desc,ts with
              Var {Id.name="Pervasives.="}, [t1;t2] -> BinOp(Eq, t1, t2)
            | Var {Id.name="Pervasives.<>"}, [t1;t2] -> Not{desc=BinOp(Eq, t1, t2);typ=TBool}
            | Var {Id.name="Pervasives.<"}, [t1;t2] -> BinOp(Lt, t1, t2)
            | Var {Id.name="Pervasives.>"}, [t1;t2] -> BinOp(Gt, t1, t2)
            | Var {Id.name="Pervasives.<="}, [t1;t2] -> BinOp(Leq, t1, t2)
            | Var {Id.name="Pervasives.>="}, [t1;t2] -> BinOp(Geq, t1, t2)
            | Var {Id.name="Pervasives.&&"}, [t1;t2] -> BinOp(And, t1, t2)
            | Var {Id.name="Pervasives.||"}, [t1;t2] -> BinOp(Or, t1, t2)
            | Var {Id.name="Pervasives.+"}, [t1;t2] -> BinOp(Add, t1, t2)
            | Var {Id.name="Pervasives.-"}, [t1;t2] -> BinOp(Sub, t1, t2)
            | Var {Id.name="Pervasives.*"}, [t1;t2] -> BinOp(Mult, t1, t2)
            | Var {Id.name="Pervasives.~-"}, [t] -> BinOp(Sub, {desc=Int 0;typ=TInt[]}, t)
            | Var {Id.name="Pervasives.not"}, [t] -> Not(t)
            | _ -> App(t, ts)
        end
      | Texp_match(e,pes,Total) ->
        let t = from_expression e in
        let aux (p,e) = 
          match e.exp_desc with
              Texp_when(e1,e2) -> from_pattern p, Some (from_expression e1), from_expression e2
            | _ -> from_pattern p, None, from_expression e
        in
        Match_(t, List.map aux pes)
      | Texp_match _ -> unsupported "expression (match)"
      | Texp_try(e,pes) ->
        let aux (p,e) = 
          match e.exp_desc with
              Texp_when(e1,e2) -> from_pattern p, Some (from_expression e1), from_expression e2
            | _ -> from_pattern p, None, from_expression e
        in
        TryWith(from_expression e, List.map aux pes)
      | Texp_tuple es ->
        let aux (c,fields) e = c+1, fields @ [string_of_int c, (Flag.Immutable, from_expression e)] in
        let _,fields = List.fold_left aux (0,[]) es in
        Record(true, fields)
      | Texp_construct(desc,es) ->
        begin
          match get_constr_name desc typ env, es with
              "()",[] -> Unit
            | "true",[] -> True
            | "false",[] -> False
            | "[]",[] -> Nil
            | "::",[e1;e2] -> Cons(from_expression e1, from_expression e2)
            | name,es ->
              add_exc_env desc env;
              Constr(name, List.map from_expression es)
        end
      | Texp_variant _ -> unsupported "expression (variant)"
      | Texp_record(fields,None) ->
        let fields' = List.sort (fun (lbl1,_) (lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos) fields in
        let fields'' = List.map (fun (label,e) -> get_label_name label env, (from_mutable_flag label.lbl_mut, from_expression e)) fields' in
        Record(false, fields'')
      | Texp_record(fields,Some init) ->
        let labels = Array.to_list (fst (List.hd fields)).lbl_all in
        let r = Id.new_var "r" typ' in
        let fields' =
          let aux lbl =
            let name = get_label_name lbl env in
            let flag = from_mutable_flag lbl.lbl_mut in
            try
              name, (flag, from_expression (List.assoc lbl fields))
            with Not_found -> name, (flag, {desc=Proj(None, lbl.lbl_pos, name, flag, {desc=Var r;typ=Id.typ r});typ=from_type_expr env [] lbl.lbl_arg})
          in
          List.map aux labels
        in
        Let(Flag.Nonrecursive,r,[],from_expression init,{desc=Record(false,fields');typ=typ'})
      | Texp_field(e,label) ->
        Proj(None, label.lbl_pos, get_label_name label env, from_mutable_flag label.lbl_mut, from_expression e)
      | Texp_setfield(e1,label,e2) ->
        SetField(None, label.lbl_pos, get_label_name label env, from_mutable_flag label.lbl_mut, from_expression e1, from_expression e2)
      | Texp_array _ -> unsupported "expression (array)"
      | Texp_ifthenelse(e1,e2,e3) ->
        let t1 = from_expression e1 in
        let t2 = from_expression e2 in
        let t3 =
          match e3 with
              None -> {desc=Unit; typ=TUnit}
            | Some e3 -> from_expression e3
        in
        If(t1, t2, t3)
      | Texp_sequence(e1,e2) ->
        let t1 = from_expression e1 in
        let t2 = from_expression e2 in
        let u = Id.new_var "u" t1.typ in
        Let(Flag.Nonrecursive, u, [], t1, t2)
      | Texp_while _ -> unsupported "expression (while)"
      | Texp_for _ -> unsupported "expression (for)"
      | Texp_when _ -> unsupported "expression (when)"
      | Texp_send _
      | Texp_new _ -> unsupported "expression (class)"
      | Texp_instvar _ -> unsupported "expression (instvar)"
      | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
      | Texp_override _ -> unsupported "expression (override)"
      | Texp_letmodule _ -> unsupported "expression (module)"
      | Texp_assert e ->
          let u = Id.new_var "u" TEvent in
            If(from_expression e, unit_term, {desc=Let(Flag.Nonrecursive, u, [], {desc=Fail;typ=TEvent}, unit_term); typ=TUnit})
      | Texp_assertfalse _ -> assert false;
        let x = Id.new_var "x" TUnit in
        App({desc=Fail;typ=TFun(x,TUnit)}, [{desc=Unit;typ=TUnit}])
      | Texp_lazy e -> assert false(*
                                     let u = Id.new_var "u" TUnit in
                                     Fun(u, from_expression e);
                                     from_expression e*)
      | Texp_object _ -> unsupported "expression (class)"
  in
  {desc=desc; typ=typ'}


let from_exception_declaration env = List.map (from_type_expr env [])


let from_top_level_phrase (env,defs) = function
    Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  | Parsetree.Ptop_def struc ->
      let aux2 = function
          Tstr_eval e ->
            let t = from_expression e in
              [Decl_let(Flag.Nonrecursive, [Id.new_var "u" t.typ, t])]
        | Tstr_value(rec_flag,defs) ->
            let flag = from_rec_flag rec_flag in
            let defs' = List.map (fun (p,t) -> from_pattern p, from_expression t) defs in
              [Decl_let(flag, List.flatten (List.map (fun (p,t) -> get_bindings p t) defs'))]
        | Tstr_primitive _ -> unsupported "external"
        | Tstr_type decls -> [](*Decl_type(List.map (fun (t,decl) -> Id.name (from_ident t ()), from_type_declaration env decl) decls)*)
        | Tstr_exception(x,exc_decl) -> [](*Decl_exc((from_ident x).name, from_exception_declaration env exc_decl)*)
        | Tstr_exn_rebind _ -> unsupported "exception rebind"
        | Tstr_module _
        | Tstr_recmodule _
        | Tstr_modtype _ -> unsupported "module"
        | Tstr_open _ -> unsupported "open"
        | Tstr_class _
        | Tstr_cltype _ -> unsupported "class"
        | Tstr_include _ -> unsupported "include"
      in
      let struc,_,env' = Typemod.type_structure env struc Location.none in
        env', rev_map_flatten aux2 struc @@ defs


let from_use_file ast =
  let _,defs = List.fold_left from_top_level_phrase (initial_env,[]) ast in
  let aux t = function
      Decl_let(Flag.Nonrecursive, defs) ->
        List.fold_right (fun (f,t1) t2 -> {desc=Let(Flag.Nonrecursive, f, [], t1, t2); typ=t2.typ}) defs t
    | Decl_let(Flag.Recursive, defs) ->
        let fs = List.map fst defs in
        let fs' = List.map Id.new_var_id fs in
        let aux f =
          let rec aux = function
              [] -> assert false
            | g::fs when g=f -> fs
            | _::fs -> aux fs
          in
            aux fs
        in
        let map1 = List.map2 (fun f f' ->  f, {desc=Var f'; typ=Id.typ f'}) fs fs' in
        let map2 = List.map2 (fun f f' ->  f', app2app {desc=Var f;typ=Id.typ f} (List.map (fun f -> {desc=Var f;typ=Id.typ f}) (aux f))) fs fs' in
        let sbst t = subst_term map2 (subst_term map1 t) in
        let defs' = List.map (fun (f,t) -> f, List.fold_right (fun f t -> {desc=Fun(f,t);typ=TFun(f,t.typ)}) (aux f) (sbst t)) defs in
          List.fold_right (fun (f,t1) t2 -> {desc=Let(Flag.Recursive, f, [], t1, t2);typ=t2.typ}) defs' (sbst t)
    | Decl_type(typs) -> t(*Type_decl(typs,t)*)
    | Decl_exc(exc,typs) -> t(*Exception(exc,typs,t)*)
  in
  let t = List.fold_left aux {desc=Unit;typ=TUnit} defs in
  let t = merge_let_fun t in
    set_target t


