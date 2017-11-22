# 2 "parser_wrapper_4.03.ml"
open Util
open Asttypes
open Typedtree
open Types
open Syntax
open Term_util
open Type


module Debug = Debug.Make(struct let check = make_debug_check "Parser_wrapper" end)

type declaration =
  | Decl_let of rec_flag * (id * term) list
  | Decl_type of (string * (typ list * type_kind)) list
  | Decl_exc of string * typ list


let () = Compmisc.init_path false


let exc_init : (string * typ list) list = []
(*
  ["Assert_failure", [];
   "Not_found", []]
 *)
let exc_env = ref exc_init
let init_exc_env () = exc_env := exc_init
let add_exc_env name typs =
  if List.mem_assoc name !exc_env then
    if not @@ List.eq ~eq:Type.same_shape (List.assoc name !exc_env) typs then
      unsupported "Same name exception"
    else
      ()
  else
    exc_env := (name,typs) :: !exc_env
let exc_typ () =
  Type(["exn", TVariant !exc_env], "exn")


let prim_typs =
  ["unit", TUnit;
   "bool", TBool;
   "int", TInt;
   "Pervasives.format", TData "string";
   "CamlinternalFormatBasics.fmt", TData "string";
   "exn", TData "exn";
(*
   "Pervasives.in_channel", TUnit
*)]


let venv = ref []

let from_mutable_flag = function
  | Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable


let prim_typ_cons =
  ["list", TList;
   "Pervasives.ref", TRef;
   "option", TOption;
   "array", TArray]

let rec from_type_expr rec_env tenv typ =
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
      let typ1' = from_type_expr rec_env tenv typ1 in
      let typ2' = from_type_expr rec_env tenv typ2 in
      let x = Id.new_var typ1' in
      TFun(x, typ2')
  | Ttuple typs ->
      make_ttuple (List.map (from_type_expr rec_env tenv) typs)
  | Tconstr(path, _, _) when List.mem_assoc (Path.name path) prim_typs ->
      List.assoc (Path.name path) prim_typs
  | Tconstr(path, typs, _) when List.mem_assoc (Path.name path) prim_typ_cons ->
      TApp(List.assoc (Path.name path) prim_typ_cons, List.map (from_type_expr rec_env tenv) typs)
  | Tconstr(path, typs, _) ->
      let name = Path.name path in
      if List.mem name rec_env then
        TData name
      else
        let typ'' =
          try
            match (Env.find_type path tenv).type_kind with
            | Type_abstract ->
                begin
                  try
                    let typs,typ',n = Env.find_type_expansion path tenv in
                    if typs <> [] then unsupported "Tconstr";
                    from_type_expr rec_env tenv typ'
                  with Not_found -> TData name
                end
            | Type_variant decls ->
                let aux {cd_id;cd_args} =
                  match cd_args with
                  | Cstr_tuple args -> Ident.name cd_id, List.map (from_type_expr (name::rec_env) tenv) args
                  | Cstr_record _ -> unsupported "Cstr_record"
                in
                TVariant (List.map aux decls)
            | Type_record(decls,_) ->
                let aux {ld_id;ld_mutable;ld_type} =
                  Ident.name ld_id, (from_mutable_flag ld_mutable, from_type_expr (name::rec_env) tenv ld_type)
                in
                TRecord (List.map aux decls)
            | Type_open -> unsupported "Type_open"
          with Not_found -> assert false
        in
        if typ'' <> TData name && data_occurs name typ'' then
          Type([name, typ''], name)
        else
          typ''
  | Tobject _ -> unsupported "Tobject"
  | Tfield _ -> unsupported "Tfield"
  | Tnil -> unsupported "Tnil"
  | Tlink _ -> unsupported "Tlink"
  | Tsubst _ -> unsupported "Tsubst"
  | Tvariant _ -> unsupported "Tvariant"
  | Tunivar _ -> unsupported "Tunivar"
  | Tpoly _ -> unsupported "Tpoly"
  | Tpackage _ -> unsupported "Tpackage"
let from_type_expr tenv typ = from_type_expr [] tenv typ

let from_rec_flag = function
  | Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive



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

let from_ident_aux name binding_time attr typ =
  let name = sign_to_letters name in
  let name = if name.[0] = '_' then "u" ^ name else name in
  Id.make binding_time name attr typ

let from_ident x typ =
  from_ident_aux (Ident.name x) (Ident.binding_time x) [] typ

let from_ident_path id_env path typ =
  let name = Path.name path in
  try
    Id.set_typ (List.find (Id.name |- (=) name) id_env) typ
  with Not_found ->
    let binding_time,attr =
      if Char.is_uppercase name.[0] then
        0, [Id.External]
      else
        Path.binding_time path, []
    in
    from_ident_aux name binding_time attr typ


let get_constr_name desc typ env =
  match desc.cstr_tag with
  | Cstr_constant _ -> desc.cstr_name
  | Cstr_block _ -> desc.cstr_name
  | Cstr_extension(path, b) -> Path.name path

let get_label_name label env =
  label.lbl_name


let add_exc_env_from_constr cstr_desc env =
  match cstr_desc.cstr_res.Types.desc with
  | Tconstr(path,_,_) ->
      if Path.name path = "exn" then
        let name = get_constr_name cstr_desc cstr_desc.cstr_res env in
        let typs = List.map (from_type_expr env) cstr_desc.cstr_args in
        add_exc_env name typs
  | _ -> assert false

let rec from_pattern {pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=env} =
(*
  add_type_env env typ;
*)
  let typ' = from_type_expr env typ in
  let desc =
    match desc with
    | Tpat_any -> PAny
    | Tpat_var(x,_) -> PVar(from_ident x typ')
    | Tpat_alias({pat_desc=Tpat_any},x,_) -> PVar (from_ident x typ')
    | Tpat_alias(p,x,_) -> PAlias(from_pattern p, from_ident x typ')
    | Tpat_constant(Const_int n) -> PConst {desc=Const(Int n);typ=typ'; attr=[]}
    | Tpat_constant(Const_char c) -> PConst {desc=Const(Char c);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,None)) -> PConst {desc=Const(String s);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,Some _)) -> unsupported "Const_string Some"
    | Tpat_constant(Const_float s) -> PConst {desc=Const(Float (float_of_string s));typ=typ'; attr=[]}
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
        add_exc_env_from_constr cstr_desc env;
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
  | Var {Id.name="Pervasives./"}, [t1;t2] ->
      let t2' =
        let make_check t = make_seq (make_assert (make_neq t @@ make_int 0)) t in
        if has_no_effect t2 then
          make_check t2
        else
          let x = Id.new_var TInt in
          make_let [x,t2] @@ make_check @@ make_var x
      in
      make_div t1 t2'
  | Var {Id.name="Pervasives.~-"}, [t] -> make_neg t
  | Var {Id.name="Pervasives.not"}, [t] -> make_not t
  | Var {Id.name="Pervasives.fst"}, [t] -> make_fst t
  | Var {Id.name="Pervasives.snd"}, [t] -> make_snd t
  | Var {Id.name="Pervasives.raise"}, [t] -> make_raise t typ
  | Var {Id.name="Pervasives.ref"}, [t] -> make_ref t
  | Var {Id.name="Pervasives.read_int"}, [{desc=Const Unit}] ->
      let attr =
        if Flag.Method.(!mode = NonTermination || !mode = FairNonTermination) then
          AAbst_under::randint_term.attr
        else
          randint_term.attr in
      make_app {randint_term with attr} [unit_term]
  | Var {Id.name="Pervasives.!"}, [t] -> make_deref t
  | Var {Id.name="Pervasives.:="}, [t1;t2] -> make_setref t1 t2
  | Var {Id.name="Random.bool"}, [{desc=Const Unit}] -> make_eq (make_app randint_term [unit_term]) (make_int 0)
  | Var {Id.name="Random.int"}, [{desc=Const (Int 0)}] -> randint_unit_term
  | Var {Id.name="Random.int"}, [t] ->
      let x = Id.new_var ~name:"n" TInt in
      make_let [x, randint_unit_term] @@
        make_assume
          (make_and (make_leq (make_int 0) (make_var x)) (make_lt (make_var x) t))
          (make_var x)
  | Var {Id.name="Pervasives.open_in"}, [{desc=Const(Int _)}] -> make_event_unit "newr"
  | Var {Id.name="Pervasives.close_in"}, [{typ=TUnit}] -> make_event_unit "close"
  | Var {Id.name="Pervasives.invalid_arg"}, [{desc=Const(String s)}] ->
      let c = "Invalid_argument" ^ "." ^ s in
      add_exc_env c [];
      make_raise (make_construct c [] !!exc_typ) typ
  | Var {Id.name="Pervasives.failwith"}, [{desc=Const(String s)}] ->
      let c = "Failure" ^ "." ^ s in
      add_exc_env c [];
      make_raise (make_construct c [] !!exc_typ) typ
  | Var {Id.name="Pervasives.invalid_arg"}, [_] ->
      unsupported "Pervasives.invalid_arg with non-constant"
  | Var {Id.name="event"}, [{desc=Const(String s)}] -> make_event_unit s
  | _ -> make_app t ts



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
  | Const_float s -> Float (float_of_string s)
  | Const_int32 n -> Int32 n
  | Const_int64 n -> Int64 n
  | Const_nativeint n -> Nativeint n


let is_var_case case =
  match case with
  | {c_lhs={pat_desc=Tpat_var _}; c_guard=None} -> true
  | {c_lhs={pat_desc=Tpat_alias({pat_desc=Tpat_any},_,_)}; c_guard=None} -> true
  | _ -> false

let rec from_expression id_env {exp_desc; exp_loc=_; exp_type=typ; exp_env=env} =
  let typ' = from_type_expr env typ in
  match exp_desc with
  | Texp_ident(path, _, _) ->
      make_var @@ from_ident_path id_env path typ'
  | Texp_constant c ->
      {desc = Const (from_constant c); typ = typ'; attr=[]}
  | Texp_let(rec_flag, [{vb_pat;vb_expr}], e2)
       when (function Tpat_var _ -> false | _ -> true) vb_pat.pat_desc ->
      let p' = from_pattern vb_pat in
      let t1 = from_expression id_env vb_expr in
      let t2 = from_expression id_env e2 in
      make_single_match t1 p' t2
  | Texp_let(rec_flag, pats, e) ->
      let flag = from_rec_flag rec_flag in
      let aux {vb_pat;vb_expr} =
        let p' = from_pattern vb_pat in
        let e' = from_expression id_env vb_expr in
        match p'.pat_desc with
        | PVar x -> x, e'
        | _ ->
            if flag = Recursive
            then unsupported "Only variables are allowed as left-hand side of 'let rec'"
            else unsupported "Only variables are allowed as left-hand side of 'let ... and ...'"
      in
      let bindings = List.map aux pats in
      let t = from_expression id_env e in
      make_let bindings t
  | Texp_function(_,[case],Total) when is_var_case case ->
      begin
        match from_case id_env case with
        | {pat_desc=PVar x}, _, e -> make_fun x e
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
      let pats' = List.map (from_case id_env) pats in
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
      let t = from_expression id_env e in
      let aux = function
        | _, None -> unsupported "expression (optional)"
        | _, Some e -> from_expression id_env e
      in
      let ts = List.map aux es in
      conv_primitive_app t ts typ'
  | Texp_match(e,pats,[],tp) ->
      let t = from_expression id_env e in
      let pats' = List.map (from_case id_env) pats in
      let pats'' =
        match tp with
        | Total -> pats'
        | Partial -> pats'@[make_pvar (Id.new_var t.typ), true_term, make_fail typ']
      in
      make_match t pats''
  | Texp_match(e,pats,_,tp) -> unsupported "Texp_match (exception)"
  | Texp_try(e,pats) ->
      let typ_excep = !!exc_typ in
      let x = Id.new_var ~name:"e" typ_excep in
      let pats' = List.map (from_case id_env) pats in
      let pats'' = pats' @ [make_pany typ_excep, true_term, {desc=Raise(make_var x); typ=typ'; attr=[]}] in
      {desc=TryWith(from_expression id_env e, make_fun x (make_match (make_var x) pats'')); typ=typ'; attr=[]}
  | Texp_tuple es ->
      {desc=Tuple(List.map (from_expression id_env) es); typ=typ'; attr=[]}
  | Texp_construct(_,desc,es) ->
      let desc =
        match get_constr_name desc typ env, es with
        | "()",[] -> Const Unit
        | "true",[] -> Const True
        | "false",[] -> Const False
        | "[]",[] -> Nil
        | "::",[e1;e2] -> Cons(from_expression id_env e1, from_expression id_env e2)
        | "None",[] -> TNone
        | "Some",[e] -> TSome (from_expression id_env e)
        | "Format",_ -> Const (String "%some format%")
        | name,es ->
            add_exc_env_from_constr desc env;
            Debug.printf "CONSTR: %s@." name;
            Debug.printf "   typ: %a@." Printtyp.type_expr typ;
            Constr(name, List.map (from_expression id_env) es)
      in
      {desc=desc; typ=typ'; attr=[]}
  | Texp_variant _ -> unsupported "expression (variant)"
  | Texp_record(fields, None) ->
      let fields' = List.sort (fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos) fields in
      let aux (_,label,e) = get_label_name label env, from_expression id_env e in
      let fields'' = List.map aux fields' in
      {desc=Record fields''; typ=typ'; attr=[]}
  | Texp_record(fields, Some init) ->
      let labels = Array.to_list (Triple.snd @@ List.hd fields).lbl_all in
      let r = Id.new_var ~name:"r" typ' in
      let fields' =
        let aux lbl =
          let name = get_label_name lbl env in
          try
            let _,_,e = List.find (fun (_,lbl',_) -> lbl.lbl_name = lbl'.lbl_name) fields in
            name, from_expression id_env e
          with Not_found ->
            name, {desc=Field(make_var r,name); typ=from_type_expr env lbl.lbl_arg; attr=[]}
        in
        List.map aux labels
      in
      make_let [r, from_expression id_env init] {desc=Record fields';typ=typ'; attr=[]}
  | Texp_field(e,_,label) ->
      let t = from_expression id_env e in
      {desc=Field(t, get_label_name label env); typ=typ'; attr=make_attr[t]}
  | Texp_setfield(e1,_,label,e2) ->
      {desc=SetField(from_expression id_env e1, get_label_name label env, from_expression id_env e2); typ=typ'; attr=[]}
  | Texp_array es ->
      let typ'' = array_typ typ' in
      let ts = List.map (from_expression id_env) es in
      let array_of_list = make_var @@ Id.new_var ~name:"Array.of_list" ~attr:[Id.External] @@ make_tfun (make_tlist typ'') typ' in
      make_app array_of_list [List.fold_right make_cons ts (make_nil typ'')]
  | Texp_ifthenelse(e1,e2,e3) ->
      let t1 = from_expression id_env e1 in
      let t2 = from_expression id_env e2 in
      let t3 =
        match e3 with
        | None -> unit_term
        | Some e3 -> from_expression id_env e3
      in
      make_if t1 t2 t3
  | Texp_sequence(e1,e2) -> make_seq (from_expression id_env e1) (from_expression id_env e2)
  | Texp_while(e1,e2) ->
      let t1 = from_expression id_env e1 in
      let t2 = from_expression id_env e2 in
      let x = Id.new_var TUnit in
      let f = Id.new_var ~name:"while" @@ make_tfun TUnit t2.typ in
      let t2' = make_if t1 (make_seq t2 @@ make_app (make_var f) [unit_term]) unit_term in
      make_let [f, make_fun x t2'] @@ make_app (make_var f) [unit_term]
  | Texp_for(x, _, e1, e2, dir, e3) ->
      let t1 = from_expression id_env e1 in
      let t2 = from_expression id_env e2 in
      let t3 = from_expression id_env e3 in
      let x' = from_ident x TInt in
      if Type.can_unify t3.typ TUnit then
        Type.unify t3.typ TUnit
      else
        unsupported "The body of a for-expression must have type unit";
      let f = Id.new_var ~name:"for" (TFun(Id.new_var ~name:"i" TInt, TUnit)) in
      let init = Id.new_var ~name:"init" TInt in
      let last = Id.new_var ~name:"last" TInt in
      let t31 =
        match dir with
        | Upto -> make_leq (make_var x') (make_var last)
        | Downto -> make_geq (make_var x') (make_var last)
      in
      let t32 =
        let x'' =
          match dir with
          | Upto -> make_add (make_var x') (make_int 1)
          | Downto -> make_sub (make_var x') (make_int 1)
        in
        make_seq t3 @@ make_app (make_var f) [x'']
      in
      assert (Flag.Debug.check_typ => Type.can_unify t31.typ TBool);
      let t3' = make_if t31 t32 unit_term in
      make_lets [init,t1; last,t2] @@ make_let [f, make_fun x' t3'] @@ make_app (make_var f) [make_var init]
  | Texp_send _
  | Texp_new _ -> unsupported "expression (class)"
  | Texp_instvar _ -> unsupported "expression (instvar)"
  | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
  | Texp_override _ -> unsupported "expression (override)"
  | Texp_letmodule _ -> unsupported "expression (module)"
  | Texp_assert e ->
      let t = from_expression id_env e in
      if t.desc = Const False
      then make_fail typ'
      else make_assert t
  | Texp_lazy e -> assert false
  | Texp_object _ -> unsupported "expression (class)"
  | Texp_pack _ -> unsupported "expression (pack)"
  | Texp_unreachable -> unsupported "Texp_unreachable"
  | Texp_extension_constructor _ -> unsupported "Texp_extension_constructor"

and from_case id_env {c_lhs;c_guard;c_rhs} =
  from_pattern c_lhs,
  Option.map_default (from_expression id_env) true_term c_guard,
  from_expression id_env c_rhs


let from_exception_declaration env = List.map (from_type_expr env)


let rec from_module_binding id_env mb =
  match mb.mb_expr.mod_desc with
  | Tmod_structure struc ->
      let id_env', decls = from_structure id_env struc in
      let prefix = Ident.name mb.mb_id ^ "." in
      let map =
        let aux = function
          | Decl_let(_, defs) -> List.map fst defs
          | _ -> []
        in
        let fs = List.flatten_map aux decls in
        List.map (fun f -> Format.printf "id_env: %a@." Id.print f ;f, Id.add_name_before prefix f) fs
      in
      let decls' =
        let rename decl =
          match decl with
          | Decl_let(flag, defs) ->
              let aux (f,t) =
                Id.assoc f map, List.fold_right (Fun.uncurry subst_var) map t
              in
              Decl_let(flag, List.map aux defs)
          | _ -> decl
        in
        List.map rename decls
      in
      let id_env'' = List.map snd map @ id_env' in
      id_env'', decls'
  | _ -> unsupported "module"

and from_str_item id_env str_item =
  match str_item.str_desc with
  | Tstr_eval(e,_) ->
      let t = from_expression id_env e in
      id_env, [Decl_let(Nonrecursive, [Id.new_var ~name:"u" t.typ, t])]
  | Tstr_value(rec_flag,pats) ->
      let flag = from_rec_flag rec_flag in
      let aux {vb_pat;vb_expr} =
        let p = from_pattern vb_pat in
        let e = from_expression id_env vb_expr in
        match p.pat_desc, flag with
        | PVar x, _ -> x, e
        | PConst {desc=Const Unit}, _ -> Id.new_var ~name:"u" TUnit, e
        | PAny, _ -> new_var_of_term e, e
        | _, Recursive -> fatal "Only variables are allowed as left-hand side of 'let rec'"
        | _, Nonrecursive -> unsupported "Only variables are allowed as left-hand side of 'let'"
      in
      id_env, [Decl_let(flag, List.map aux pats)]
  | Tstr_primitive _ -> id_env, []
  | Tstr_type _ -> id_env, []
  | Tstr_typext _ -> unsupported "typext"
  | Tstr_exception _ -> id_env, []
  | Tstr_module mb ->
      from_module_binding id_env mb
  | Tstr_recmodule _
  | Tstr_modtype _ ->
      unsupported "module"
  | Tstr_open _ -> id_env, []
  | Tstr_class _
  | Tstr_class_type _ -> unsupported "class"
  | Tstr_include _ -> id_env, []
  | Tstr_attribute _ -> id_env, []

and from_structure id_env struc =
  let aux (id_env,decls) str_item =
    let id_env',decls' = from_str_item id_env str_item in
    id_env', decls' @@@ decls
  in
  List.fold_left aux (id_env,[]) struc.str_items

let from_top_level_phrase (tenv,id_env,decls) = function
  | Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  | Parsetree.Ptop_def struc ->
      let struc',_,tenv' = Typemod.type_structure tenv struc Location.none in
      let id_env',decls' = from_structure id_env struc' in
      tenv', id_env', decls' @@@ decls

let from_use_file ast =
  let env = Compmisc.initial_env () in
  init_exc_env ();
  let aux t = function
    | Decl_let(_, defs) -> make_let defs t
    | Decl_type _ -> t
    | Decl_exc _ -> t
  in
  ast
  |> List.fold_left from_top_level_phrase (env,[],[])
  |> Triple.trd
  |> List.fold_left aux unit_term
  |> subst_data_type_term "exn" !!exc_typ
  |> Trans.rename_bound_module
  |> Trans.split_let
  |@> Debug.printf "before alpha: %a@." Print.term'
  |> Trans.alpha_rename ~whole:true
  |@> Debug.printf "after alpha: %a@." Print.term'
  |@> Debug.printf "after alpha: %a@." Print.term
  |@> Id.set_counter -| succ -| Term_util.get_max_var_id
