
open Util
open Asttypes
open Typedtree
open Types
open Syntax


exception Unsupported of string

let unsupported s = raise (Unsupported s)


let () = Config.load_path := Flag.ocaml_lib
let initial_env = Compile.initial_env ()


let rec from_type_expr typ = from_type_desc typ.desc

and from_type_desc = function
    Tvar -> TUnknown(*unsupported "Tvar"*)
  | Tarrow(_, typ1, typ2, _) -> let x = new_var' "x" in TFun((x,from_type_expr typ1), from_type_expr typ2)
  | Ttuple _ -> unsupported "Ttuple"
  | Tconstr(path, _, _) when Path.name path = "unit" -> TUnit
  | Tconstr(path, _, _) when Path.name path = "bool" -> TBool
  | Tconstr(path, [type_expr], _) when Path.name path = "list" -> TList (from_type_expr type_expr)
  | Tconstr _ -> unsupported "Tconstr"
  | Tobject _ -> unsupported "Tobject"
  | Tfield _ -> unsupported "Tfield"
  | Tnil _ -> unsupported "Tnil"
  | Tlink _ -> unsupported "Tlink"
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

let from_pattern {pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=_} =
  match desc with
      Tpat_any -> new_var' "u"
    | Tpat_var x -> from_ident x
    | Tpat_alias _ -> unsupported "pattern match (alias)"
    | Tpat_constant _ -> unsupported "pattern match (constant)"
    | Tpat_tuple _ -> unsupported "pattern match (tuple)"
    | Tpat_construct _ -> unsupported "pattern match (constructor)"
    | Tpat_variant _ -> unsupported "pattern match (variant)"
    | Tpat_record _ -> unsupported "pattern match (record)"
    | Tpat_array _ -> unsupported "pattern match (array)"
    | Tpat_or _ -> unsupported "pattern match (or)"
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
  | Const_string _ -> unsupported "constant (string)"
  | Const_float _ -> unsupported "constant (float)"
  | Const_int32 _ -> unsupported "constant (int32)"
  | Const_int64 _ -> unsupported "constant (int64)"
  | Const_nativeint _ -> unsupported "constant (nativeint)"


let rec app2binop = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun(x, t) -> Fun(x, app2binop t)
  | App(t, ts) ->
      begin
        match t,ts with
            Var {id=_; name="="; typ=_}, [t1;t2] -> BinOp(Eq, app2binop t1, app2binop t2)
          | Var {id=_; name="<"; typ=_}, [t1;t2] -> BinOp(Lt, app2binop t1, app2binop t2)
          | Var {id=_; name=">"; typ=_}, [t1;t2] -> BinOp(Gt, app2binop t1, app2binop t2)
          | Var {id=_; name="<="; typ=_}, [t1;t2] -> BinOp(Leq, app2binop t1, app2binop t2)
          | Var {id=_; name=">="; typ=_}, [t1;t2] -> BinOp(Geq, app2binop t1, app2binop t2)
          | Var {id=_; name="&&"; typ=_}, [t1;t2] -> BinOp(And, app2binop t1, app2binop t2)
          | Var {id=_; name="||"; typ=_}, [t1;t2] -> BinOp(Or, app2binop t1, app2binop t2)
          | Var {id=_; name="+"; typ=_}, [t1;t2] -> BinOp(Add, app2binop t1, app2binop t2)
          | Var {id=_; name="-"; typ=_}, [t1;t2] -> BinOp(Sub, app2binop t1, app2binop t2)
          | Var {id=_; name="*"; typ=_}, [t1;t2] -> BinOp(Mult, app2binop t1, app2binop t2)
          | _ -> App(app2binop t, List.map app2binop ts)
      end
  | If(t1, t2, t3) -> If(app2binop t1, app2binop t2, app2binop t3)
  | Branch(t1, t2) -> Branch(app2binop t1, app2binop t2)
  | Let(flag, f, xs, t1, t2) -> Let(flag, f, xs, app2binop t1, app2binop t2)
  | BinOp(binop, t1, t2) -> BinOp(binop, app2binop t1, app2binop t2)
  | Not t -> Not (app2binop t)
  | Fail -> Fail
  | Label(b,t) -> Label(b, app2binop t)
  | Event s -> Event s
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(app2binop t1, app2binop t2)


let rec from_expression {exp_desc=exp_desc; exp_loc=_; exp_type=_; exp_env=_} =
  app2binop (from_expression_desc exp_desc)

and from_expression_desc = function
    Texp_ident(path, {Types.val_type=_; Types.val_kind=_}) ->
      Var (from_path path)
  | Texp_constant c -> from_constant c
  | Texp_let(rec_flag, [p,e1], e2) ->
      let flag = from_rec_flag rec_flag in
        Let(flag, from_pattern p, [], from_expression e1, from_expression e2)
  | Texp_let _ -> unsupported "Texp_let"
      (*
        | Texp_let(rec_flag, pats, e) ->
        let flag = from_rec_flag rec_flag in
        let bindings = List.map (fun (p,e) -> from_pattern p, [], from_expression e) pats in
        let t = from_expression e in
        Let(flag, bindings, t)
      *)
  | Texp_function([p,e], _) ->
      let x = from_pattern p in
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
        App(t, ts)
  | Texp_match _ -> unsupported "expression (match)"
  | Texp_try _ -> unsupported "expression (try)"
  | Texp_tuple _ -> unsupported "expression (tuple)"
  | Texp_construct(desc,es) ->
      begin
        match from_type_expr desc.cstr_res with
            TUnit -> Unit
          | TBool when desc.cstr_tag = Cstr_constant 0 -> False
          | TBool when desc.cstr_tag = Cstr_constant 1 -> True
          | TList _ when desc.cstr_tag = Cstr_constant 0 -> Nil
          | TList _ when desc.cstr_tag = Cstr_block 0 ->
              match es with
                  [e1;e2] -> Cons(from_expression e1, from_expression e2)
                | _ -> assert false
          | _ -> unsupported ("expression (constructor)")
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




let from_use_file ast =
  let from_top_level_phrase (env,defs) = function
      Parsetree.Ptop_def struc ->
        let aux2 = function
            Tstr_eval e -> Nonrecursive, [new_var' "u", [], from_expression e]
          | Tstr_value(rec_flag,defs) ->
              let flag = from_rec_flag rec_flag in
              let aux (p,e) = from_pattern p, [], from_expression e in
                flag, List.map aux defs
          | Tstr_primitive _ -> unsupported "external"
          | Tstr_type _ -> unsupported "type"
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
  let aux t2 = function
      flag, [f, xs, t1] -> Let(flag, f, xs, t1, t2)
    | _ -> unsupported "Tstr_value"
  in
    fun2let (List.fold_left aux Unit defs)



