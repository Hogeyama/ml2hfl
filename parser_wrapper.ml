
open Util
open Asttypes
open Typedtree
open Types
open Syntax


type declaration =
    Decl_let of rec_flag * (ident * ident list * t) list
  | Decl_type of (string * type_kind) list


exception Unsupported of string

let unsupported s = raise (Unsupported s)


let () = Config.load_path := Flag.ocaml_lib
let initial_env = Compile.initial_env ()


let rec from_type_expr typ = from_type_desc typ.desc

and from_type_desc = function
    Tvar -> TUnknown(*unsupported "Tvar"*)
  | Tarrow(_, typ1, typ2, _) -> let x = new_var' "x" in TFun((x,from_type_expr typ1), from_type_expr typ2)
  | Ttuple _ -> unsupported "Ttuple"
  | Tconstr(path, [], _) when Path.name path = "unit" -> TUnit
  | Tconstr(path, [], _) when Path.name path = "bool" -> TBool
  | Tconstr(path, [], _) when Path.name path = "int" -> TInt []
  | Tconstr(path, [type_expr], _) when Path.name path = "list" -> TList(from_type_expr type_expr, [])
  | Tconstr(path, [], _) -> TAbs (Path.name path)
  | Tconstr(path, typs, _) -> unsupported ("Tconstr " ^ Path.name path)(*TConstr(Path.name path, List.map from_type_expr typs)*)
  | Tobject _ -> unsupported "Tobject"
  | Tfield _ -> unsupported "Tfield"
  | Tnil _ -> unsupported "Tnil"
  | Tlink typ -> from_type_expr typ
  | Tsubst _ -> unsupported "Tsubst"
  | Tvariant _ -> unsupported "Tvariant"
  | Tunivar _ -> unsupported "Tunivar"
  | Tpoly _ -> unsupported "Tpoly"


let from_rec_flag = function
    Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive
  | Asttypes.Default -> unsupported "rec_flag (Default)"

let from_ident x = {id=Ident.binding_time x; name=Ident.name x; typ=TUnknown}

let rec from_path = function
    Path.Pident x -> from_ident x
  | Path.Pdot (path,str,_) when (from_path path).name="Pervasives" -> {id=(-1); name=str; typ=TUnknown}
  | Path.Pdot (_,str,_) -> unsupported "Path.Pdot"
  | Path.Papply _ -> unsupported "Path.Papply"


let get_constr_name tag ty tenv =
  match (Ctype.repr ty).desc with
      Tconstr(path,_,_) ->
        begin
          match Env.find_type path tenv with
            | {type_kind=Type_variant constr_list} ->
                fst (Datarepr.find_constr_by_tag tag constr_list)
            | _ -> assert false
        end
    | _ -> assert false

let rec from_pattern {pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=env} =
  match desc with
      Tpat_any -> PVar (new_var' "u")
    | Tpat_var x -> PVar (from_ident x)
    | Tpat_alias _ -> unsupported "pattern match (alias)"
    | Tpat_constant(Const_int n) -> PConst (Int n)
    | Tpat_constant(Const_char c) -> unsupported "pattern match (char constant)"
    | Tpat_constant(Const_string s) -> unsupported "pattern match (string constant)"
    | Tpat_constant(Const_float x) -> unsupported "pattern match (float constant)"
    | Tpat_constant(Const_int32 n) -> unsupported "pattern match (int32 constant)"
    | Tpat_constant(Const_int64 n) -> unsupported "pattern match (int64 constant)"
    | Tpat_constant(Const_nativeint n) -> unsupported "pattern match (nativeint constant)"
    | Tpat_tuple ps -> PTuple (List.map from_pattern ps)
    | Tpat_construct(cstr_desc, ps) ->
        let name = get_constr_name cstr_desc.cstr_tag typ env in
          PConstruct(name, List.map from_pattern ps)
    | Tpat_variant _ -> unsupported "pattern match (variant)"
    | Tpat_record _ -> unsupported "pattern match (record)"
    | Tpat_array _ -> unsupported "pattern match (array)"
    | Tpat_or(p1,p2,None) -> POr(from_pattern p1, from_pattern p2)
    | Tpat_or(_,_,Some _) -> unsupported "pattern match (or) where row = Some _"
    | Tpat_lazy _ -> unsupported "pattern match (lazy)"


let from_value_kind = function
    Types.Val_reg -> Format.printf "Val_reg@."; assert false
  | Types.Val_prim prim_desc -> new_var' (prim_desc.Primitive.prim_name)
  | Types.Val_ivar _ -> Format.printf "Val_ivar@."; assert false
  | Types.Val_self _ -> Format.printf "Val_self@."; assert false
  | Types.Val_anc _ -> Format.printf "Val_anc@."; assert false
  | Types.Val_unbound _ -> Format.printf "Val_unbound@."; assert false

let from_constant = function
    Const_int n -> Int n
  | Const_char _ -> unsupported "constant (char)"
  | Const_string _ -> Int 0
  | Const_float _ -> unsupported "constant (float)"
  | Const_int32 _ -> unsupported "constant (int32)"
  | Const_int64 _ -> unsupported "constant (int64)"
  | Const_nativeint _ -> unsupported "constant (nativeint)"
      


let prim_of_var = function
    Var x when x.name = "read" -> Event "read"
  | Var x when x.name = "open_in" -> Event "newr"
  | Var x when x.name = "close_in" -> Event "close"
  | t -> t

let var_of_pattern pat =
  match from_pattern pat with
      PVar x -> x
    | _ -> assert false

let rec from_expression {exp_desc=exp_desc; exp_loc=_; exp_type=typ; exp_env=env} =
  match exp_desc with
      Texp_ident(path, {Types.val_type=_; Types.val_kind=_}) ->
        prim_of_var (Var (from_path path))
    | Texp_constant c -> from_constant c
    | Texp_let(rec_flag, [p,e1], e2) ->
        let flag = from_rec_flag rec_flag in
          Let(flag, var_of_pattern p, [], from_expression e1, from_expression e2)
    | Texp_let _ -> unsupported "Texp_let"
        (*
          | Texp_let(rec_flag, pats, e) ->
          let flag = from_rec_flag rec_flag in
          let bindings = List.map (fun (p,e) -> var_of_pattern p, [], from_expression e) pats in
          let t = from_expression e in
          Let(flag, bindings, t)
        *)
    | Texp_function([p,e], _) ->
        let x = var_of_pattern p in
        let t = from_expression e in
          Fun(x, t)
    | Texp_function _ -> unsupported "pattern match"
    | Texp_apply(e, es) ->
        let t = from_expression e in
        let aux = function
            _, Optional -> unsupported "expression (optional)"
          | None, Required -> unsupported "???"
          | Some e, Required -> from_expression e
        in
        let ts = List.map aux es in
          begin
            match t,ts with
                Var {name="="}, [t1;t2] -> BinOp(Eq, t1, t2)
              | Var {name="<>"}, [t1;t2] -> Not(BinOp(Eq, t1, t2))
              | Var {name="<"}, [t1;t2] -> BinOp(Lt, t1, t2)
              | Var {name=">"}, [t1;t2] -> BinOp(Gt, t1, t2)
              | Var {name="<="}, [t1;t2] -> BinOp(Leq, t1, t2)
              | Var {name=">="}, [t1;t2] -> BinOp(Geq, t1, t2)
              | Var {name="&&"}, [t1;t2] -> BinOp(And, t1, t2)
              | Var {name="||"}, [t1;t2] -> BinOp(Or, t1, t2)
              | Var {name="+"}, [t1;t2] -> BinOp(Add, t1, t2)
              | Var {name="-"}, [t1;t2] -> BinOp(Sub, t1, t2)
              | Var {name="*"}, [t1;t2] -> BinOp(Mult, t1, t2)
              | Var {name="not"}, [t] -> Not(t)
              | _ -> App(t, ts)
          end
    | Texp_match(e,pes,Total) ->
        let t = from_expression e in
        let tes = List.map (fun (p,e) -> from_pattern p, from_expression e) pes in
          Match_(t,tes)
            (*
              | Texp_match(e1,[pat1,e2;pat2,e3],Total) ->
              begin
              match from_type_expr pat1.pat_type with
              TList _ ->
              begin
              match pat1.pat_desc, pat2.pat_desc with
              Tpat_construct(desc1, []), Tpat_construct(desc2, [pat21;pat22]) ->
              let t1 = from_expression e1 in
              let x = var_of_pattern pat21 in
              let y = var_of_pattern pat22 in
              let t2 = from_expression e2 in
              let t3 = from_expression e3 in
              Match (t1, t2, x, y, t3)
              | _ -> unsupported "expression (match)"
              end
              | _ -> unsupported "expression (match)"
              end
            *)
    | Texp_match _ -> unsupported "expression (match)"
    | Texp_try _ -> unsupported "expression (try)"
    | Texp_tuple _ -> unsupported "expression (tuple)"
    | Texp_construct(desc,es) ->
        begin
          match get_constr_name desc.cstr_tag typ env, es with
              "()",[] -> Unit
            | "true",[] -> True
            | "false",[] -> False
            | "[]",[] -> Nil
            | "::",[e1;e2] -> Cons(from_expression e1, from_expression e2)
            | s,es -> Constr(s, List.map from_expression es)
        end
    | Texp_variant _ -> unsupported "expression (variant)"
    | Texp_record _
    | Texp_field _
    | Texp_setfield _ -> unsupported "expression (record)"
    | Texp_array _ -> unsupported "expression (array)"
    | Texp_ifthenelse(e1,e2,e3) ->
        let t1 = from_expression e1 in
        let t2 = from_expression e2 in
        let t3 = match e3 with None -> Unit | Some e3 -> from_expression e3 in
          If(t1, t2, t3)
    | Texp_sequence(e1,e2) ->
        let u = new_var' "u" in
        let t1 = from_expression e1 in
        let t2 = from_expression e2 in
          Let(Nonrecursive, u, [], t1, t2)
    | Texp_while _ -> unsupported "expression (while)"
    | Texp_for _ -> unsupported "expression (for)"
    | Texp_when _ -> unsupported "expression (when)"
    | Texp_send _
    | Texp_new _ -> unsupported "expression (class)"
    | Texp_instvar _ -> unsupported "expression (instvar)"
    | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
    | Texp_override _ -> unsupported "expression (override)"
    | Texp_letmodule _ -> unsupported "expression (module)"
    | Texp_assert e -> App(Var ({id=(-1); name="assert"; typ=TUnknown}), [from_expression e])
    | Texp_assertfalse _ -> App(Fail, [Unit])
    | Texp_lazy _ -> unsupported "expression (lazy)"
    | Texp_object _ -> unsupported "expression (class)"


(*
let print_type_kind fm = function
    Type_abstract -> Format.print_string "Type_abstract"
  | Type_variant sts ->
      Format.print_string "Type_variant: ";
      List.iter (fun (s,ts) -> Format.printf "| %s of "; List.iter (Format.printf "%a;" Printtyp.type_expr) ts) sts
  | Type_record(sfts,_) -> Format.print_string "Type_record"

let print_type_declaration fm decl =
  Format.printf "type_declaration:@." ;
  List.iter (Format.printf "  type_params: %a@." Printtyp.type_expr) decl.type_params;
  Format.printf "  type_arity: %d@." decl.type_arity;
  Format.printf "  type_kind: %d@." decl.type_arity
*)

let from_mutable_flag = function
    Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable

let rec from_type_declaration decl = from_type_kind decl.type_kind
and from_type_kind = function
    Type_abstract -> unsupported "Type_abstract"
  | Type_variant stypss ->
      Variant(List.map (fun (s,typs) -> s,List.map from_type_expr typs) stypss)
  | Type_record(sftyps,_) ->
      Record(List.map (fun (s,flag,typ) -> s,from_mutable_flag flag,from_type_expr typ) sftyps)

let from_use_file ast =
  let from_top_level_phrase (env,defs) = function
      Parsetree.Ptop_def struc ->
        let aux2 = function
            Tstr_eval e -> Decl_let(Nonrecursive, [new_var' "u", [], from_expression e])
          | Tstr_value(rec_flag,defs) ->
              let flag = from_rec_flag rec_flag in
              let aux (p,e) = var_of_pattern p, [], from_expression e in
                Decl_let(flag, List.map aux defs)
          | Tstr_primitive _ -> unsupported "external"
          | Tstr_type decls -> Decl_type(List.map (fun (t,decl) -> (from_ident t).name, from_type_declaration decl) decls)
          | Tstr_exception _
          | Tstr_exn_rebind _ -> unsupported "exception"
          | Tstr_module _
          | Tstr_recmodule _
          | Tstr_modtype _ -> unsupported "module"
          | Tstr_open _ -> unsupported "open"
          | Tstr_class _
          | Tstr_cltype _ -> unsupported "class"
          | Tstr_include _ -> unsupported "include"
        in
        let struc,_,env' = Typemod.type_structure env struc Location.none in
          env', List.rev_map aux2 struc @@ defs
    | Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  in
  let _,defs = List.fold_left from_top_level_phrase (initial_env,[]) ast in
  let aux t = function
      Decl_let(flag, defs) when flag=Nonrecursive || List.length defs = 1 ->
        List.fold_right (fun (f, xs, t1) t2 -> Let(flag, f, xs, t1, t2)) defs t
    | Decl_type(typs) -> Type_decl(typs,t)
    | _ -> unsupported "Tstr_value"
  in
    fun2let (List.fold_left aux Unit defs)


