# 2 "parser_wrapper_4.03.ml"
open Util
open Asttypes
open Typedtree
open Types
open Syntax
open Term_util
open Type

module Debug = Debug.Make(struct let check = Flag.Debug.make_check "Parser_wrapper" end)

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
let exc_typ () = TVariant(false,!exc_env)


let prim_typs =
  ["unit", Ty.unit;
   "bool", Ty.bool;
   "int", Ty.int;
   "Pervasives.format", Ty.prim "string";
   "Pervasives.format4", Ty.prim "string";
   "Pervasives.format6", Ty.prim "string";
   "CamlinternalFormatBasics.fmt", Ty.prim "string";
   "CamlinternalFormatBasics.format6", Ty.prim "string";
   "exn", TData "exn"]
    @ List.map (fun s -> s, TBase (TPrim s)) prim_base_types


let venv = ref []

let from_mutable_flag = function
  | Asttypes.Mutable -> Mutable
  | Asttypes.Immutable -> Immutable


let prim_typ_constr =
  ["list", TList;
   "Pervasives.ref", TRef;
   "option", TOption;
   "array", TArray;
   "Lazy.t", TLazy]

let is_prim_constr constrs =
  match constrs with
  | ["[]", []; "::", ty::_] -> Some (Ty.list ty)
  | _ -> None

(* for Types.type_declaration *)
(* TODO: merge with one for Typeedtree.type_declaration *)
let rec from_type_declaration env tdata decl =
  let decls,ty =
    match decl.type_kind with
    | Type_abstract ->
        begin
          match decl.type_manifest with
          | None -> unsupported "Ttype_abstract"
          | Some ty -> from_type_expr env tdata ty
        end
    | Type_variant constrs ->
        let aux {cd_id;cd_args} (decls,constrs) =
          match cd_args with
          | Cstr_tuple args ->
              let s = Ident.name cd_id in
              let decls',tys = from_type_exprs env tdata args in
              decls'@decls, (s,tys)::constrs
          | Cstr_record _ -> unsupported "Cstr_record"
        in
        let decls,constrs = List.fold_right aux constrs ([],[]) in
        let ty' = Option.default (TVariant(false, constrs)) @@ is_prim_constr constrs in
        decls, ty'
    | Type_record(fields, _) ->
        let aux {ld_id;ld_mutable;ld_type} (decls,fields) =
          let s = Ident.name ld_id in
          let flag = from_mutable_flag ld_mutable in
          let decls', ty = from_type_expr env tdata ld_type in
          decls'@decls, (s, (flag, ty))::fields
        in
        let decls,fields = List.fold_right aux fields ([],[]) in
        decls, TRecord fields
    | Type_open -> [], TVar (ref None, None)
    | exception Not_found -> assert false
  in
  decls, ty

and from_type_exprs env tdata tys =
  let aux ty (decls,tys) =
    let decls',ty' = from_type_expr env tdata ty in
    decls'@decls, ty'::tys
  in
  List.fold_right aux tys ([],[])

and from_type_expr env tdata typ =
  let typ = Ctype.correct_levels typ in
  let typ' = (if false then Ctype.repr else Ctype.full_expand env) typ in
  Debug.printf "ty1: %a@." Printtyp.type_expr typ;
  Debug.printf "ty2: %a@." Printtyp.type_expr typ';
  Debug.printf "ty3: %a@.@." Printtyp.raw_type_expr typ';
  let decls1,tdata =
    try
      let p,_,ty = Ctype.extract_concrete_typedecl env typ' in
      let s = Path.name p in
      let tdata' = s :: tdata in
      let decls' =
        if List.mem_assoc s prim_typs || List.mem_assoc s prim_typ_constr || List.mem s tdata then
          []
        else
          let decls,ty = from_type_declaration env tdata' ty in
          (s,ty)::decls
      in
      decls', tdata'
    with Not_found -> [], tdata
  in
  let decls2,ty =
    match typ'.Types.desc with
    | Tvar _ ->
        begin
          try
            [], List.assoc typ'.Types.id !venv
          with Not_found ->
            let x = TVar(ref None, Some (List.length !venv)) in
            venv := (typ'.Types.id, x)::!venv;
            [], x
        end
    | Tarrow(_, typ1, typ2, _) ->
        let decls1,typ1' = from_type_expr env tdata typ1 in
        let decls2,typ2' = from_type_expr env tdata typ2 in
        let x = Id.new_var typ1' in
        decls1@decls2, TFun(x, typ2')
    | Ttuple typs ->
        let decls,tys' = from_type_exprs env tdata typs in
        decls, make_ttuple tys'
    | Tconstr(path, _, _) when List.mem_assoc (Path.name path) prim_typs ->
        [], List.assoc (Path.name path) prim_typs
    | Tconstr(path, typs, _) when List.mem_assoc (Path.name path) prim_typ_constr ->
        let decls,tys = from_type_exprs env tdata typs in
        decls, TApp(List.assoc (Path.name path) prim_typ_constr, tys)
    | Tconstr(path, typs, _) ->
        let s = Path.name path in
        let typ' = Ctype.expand_head env typ in
        let tdata' = s :: tdata in
        let decls' =
          if List.mem s tdata then
            []
          else
            let decls,ty = from_type_expr env tdata' typ' in
            (s,ty)::decls
        in
        decls', TData s
    | Tobject _ -> unsupported "Tobject"
    | Tfield _ -> unsupported "Tfield"
    | Tnil -> unsupported "Tnil"
    | Tlink _ -> unsupported "Tlink"
    | Tsubst _ -> unsupported "Tsubst"
    | Tvariant row_desc ->
        let from_row_desc row_desc =
          let tr (s,field) =
            match field with
            | Rpresent None -> [], (s,[])
            | Rpresent (Some type_expr) ->
                let decls',ty = from_type_expr env tdata type_expr in
                let tys =
                  match ty with
                  | TTuple xs -> List.map Id.typ xs
                  | ty -> [ty]
                in
                decls', (s,tys)
            | Reither(_,type_exprs,_,_) ->
                let decls,tys = from_type_exprs env tdata type_exprs in
                [], (s, tys)
            | Rabsent -> assert false
          in
          let declss,constrs1 = List.split_map tr row_desc.row_fields in
          let decls,constrs2 =
            if row_desc.row_more.desc = Tnil then
              [], []
            else
              match from_type_expr env tdata row_desc.row_more with
              | _, TVar _ -> [], []
              | decls, TVariant(_,constrs) -> decls, constrs
              | _ -> assert false
          in
          List.flatten declss, constrs1 @ constrs2
        in
        let decls,constrs = from_row_desc row_desc in
        decls, TVariant(true, constrs)
    | Tunivar _ -> unsupported "Tunivar"
    | Tpoly(typ,[]) -> from_type_expr env tdata typ
    | Tpoly _ -> unsupported "Tpoly"
    | Tpackage _ -> unsupported "Tpackage"
  in
  decls1@decls2, ty

let from_type_expr env ty = from_type_expr env [] ty
let from_type_exprs env ty = from_type_exprs env [] ty

(* for Typedtree.type_declaration *)
let rec from_type_declaration env decl =
  Debug.printf "decl: %a@." (Printtyp.type_declaration (Ident.create "t")) decl.typ_type;
  let open Typedtree in
  let decls,ty =
    match decl.typ_kind with
    | Ttype_abstract ->
        begin
          match decl.typ_manifest with
          | None -> unsupported "Ttype_abstract"
          | Some ty -> from_type_expr env ty.ctyp_type
        end
    | Ttype_variant constrs ->
        let aux {cd_id;cd_args} (decls,constrs) =
          match cd_args with
          | Cstr_tuple args ->
              let s = Ident.name cd_id in
              let decls',tys = from_type_exprs env @@ List.map (fun arg -> arg.ctyp_type) args in
              decls'@decls, (s,tys)::constrs
          | Cstr_record _ -> unsupported "Cstr_record"
        in
        let decls,constrs = List.fold_right aux constrs ([],[]) in
        decls, TVariant(false, constrs)
    | Ttype_record fields ->
        let aux {ld_id;ld_mutable;ld_type} (decls,fields) =
          let s = Ident.name ld_id in
          let flag = from_mutable_flag ld_mutable in
          let decls', ty = from_type_expr env ld_type.ctyp_type in
          decls'@decls, (s, (flag, ty))::fields
        in
        let decls,fields = List.fold_right aux fields ([],[]) in
        decls, TRecord fields
    | Ttype_open -> unsupported "Ttype_open"
    | exception Not_found -> assert false
  in
  decls, (decl.typ_name.txt, ty)

let from_rec_flag = function
  | Asttypes.Nonrecursive -> Nonrecursive
  | Asttypes.Recursive -> Recursive

let wrap_letters_with_sign s =
  if String.contains "!$%&*+-./:<=>?@^|~" s.[0] then
    "(" ^ s ^ ")"
  else
    s

let from_ident_aux name binding_time attr typ =
(*
  let name = wrap_letters_with_sign name in
 *)
(*
  let name,binding_time =
    match String.split_on_char '_' name with
    | [] -> assert false
    | [_] -> name, 0
    | ss ->
        try
          let names,id = List.decomp_snoc ss in
          String.join "_" names, int_of_string id
        with Failure _ | Invalid_argument _ -> name, 0
  in
 *)
  let name = if name.[0] = '_' then "u" ^ name else name in
  let name =
    if name.[0] = '*' && name.[String.length name - 1] = '*' then
      "__" ^ String.sub name 1 (String.length name - 2)
    else
      name
  in
  let id = if false then binding_time else 0 in
  Id.make id name attr typ

let from_ident x typ =
  from_ident_aux (Ident.name x) (Ident.binding_time x) [] typ

let rec string_of_path x =
  match x with
  | Path.Pident s -> Ident.name s
  | Path.Pdot(y, s, _) -> string_of_path y ^ "." ^ s
  | Path.Papply(y, z) -> Format.sprintf "%s(%s)" (string_of_path y) (string_of_path z)

let from_ident_path id_env path typ =
  let name = string_of_path path in
  try
    Id.set_typ (List.find (Id.name |- (=) name) id_env) typ
  with Not_found ->
    let binding_time = Path.binding_time path in
    let attr = [] in
    from_ident_aux name binding_time attr typ

let get_constr_name desc typ env =
  match desc.cstr_tag with
  | Cstr_constant _ -> desc.cstr_name
  | Cstr_block _ -> desc.cstr_name
  | Cstr_extension(path, _) -> Path.name path

let get_label_name label env =
  label.lbl_name

let add_exc_env_from_constr cstr_desc env =
  match cstr_desc.cstr_res.Types.desc with
  | Tconstr(path,_,_) ->
      if Path.name path = "exn" then
        let name = get_constr_name cstr_desc cstr_desc.cstr_res env in
        let _,typs = from_type_exprs env cstr_desc.cstr_args in
        add_exc_env name typs
  | _ -> assert false


let rec from_patterns ps =
  let aux p (decls,ps) =
    let decls',p' = from_pattern p in
    decls'@decls, p'::ps
  in
  List.fold_right aux ps ([],[])

and from_pattern {pat_desc=desc; pat_loc=_; pat_type=typ; pat_env=env} =
  let decls1,typ' = from_type_expr env typ in
  let decls2,desc =
    match desc with
    | Tpat_any -> [], PAny
    | Tpat_var(x,_) -> [], PVar(from_ident x typ')
    | Tpat_alias({pat_desc=Tpat_any},x,_) -> [], PVar (from_ident x typ')
    | Tpat_alias(p,x,_) ->
        let decls,p' = from_pattern p in
        decls, PAlias(p', from_ident x typ')
    | Tpat_constant(Const_int n) -> [], PConst {desc=Const(Int n);typ=typ'; attr=[]}
    | Tpat_constant(Const_char c) -> [], PConst {desc=Const(Char c);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,None)) -> [], PConst {desc=Const(String s);typ=typ'; attr=[]}
    | Tpat_constant(Const_string(s,Some _)) -> [], PConst {desc=Const(String s);typ=typ'; attr=[]}
    | Tpat_constant(Const_float s) -> [], PConst {desc=Const(Float (float_of_string s));typ=typ'; attr=[]}
    | Tpat_constant(Const_int32 n) -> [], PConst {desc=Const(Int32 n);typ=typ'; attr=[]}
    | Tpat_constant(Const_int64 n) -> [], PConst {desc=Const(Int64 n);typ=typ'; attr=[]}
    | Tpat_constant(Const_nativeint n) -> [], PConst {desc=Const(Nativeint n);typ=typ'; attr=[]}
    | Tpat_tuple ps ->
        let decls,ps' = from_patterns ps in
        decls, PTuple ps'
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "None" -> [], PNone
    | Tpat_construct(_, cstr_desc, [p]) when get_constr_name cstr_desc typ env = "Some" ->
        let decls,p' = from_pattern p in
        decls, PSome p'
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "()" -> [], PConst unit_term
    | Tpat_construct(_, cstr_desc, []) when get_constr_name cstr_desc typ env = "[]" -> [], PNil
    | Tpat_construct(_, cstr_desc, [p1;p2]) when get_constr_name cstr_desc typ env = "::" ->
        let decls1,p1' = from_pattern p1 in
        let decls2,p2' = from_pattern p2 in
        decls1@decls2, PCons(p1', p2')
    | Tpat_construct(_, cstr_desc, ps) ->
        add_exc_env_from_constr cstr_desc env;
        let name = get_constr_name cstr_desc typ env in
        let decls,ps' = from_patterns ps in
        decls, PConstr(name, ps')
    | Tpat_variant(name, pat, _) ->
        let decls, ps =
          match pat with
          | None -> [], []
          | Some pat' ->
              let decls,p = from_pattern pat' in
              let ps =
                match p with
                | {pat_desc=PTuple ps} -> ps
                | pat -> [pat]
              in
              decls, ps
        in
        decls, PConstr(name, ps)
    | Tpat_record(ps,_) ->
        let aux (_,lbl,p) (decls,ps) =
          let s = get_label_name lbl env in
          let decls',p' = from_pattern p in
          decls'@decls, (s,p')::ps
        in
        let decls,ps = List.fold_right aux ps ([],[]) in
        decls, PRecord ps
    | Tpat_array _ -> unsupported "pattern match (array)"
    | Tpat_or(p1,p2,None) ->
        let decls1,p1' = from_pattern p1 in
        let decls2,p2' = from_pattern p2 in
        decls1@decls2, POr(p1', p2')
    | Tpat_or(_,_,Some _) -> unsupported "pattern match (or) where row = Some _"
    | Tpat_lazy _ -> unsupported "pattern match (lazy)"
  in
  decls1@decls2, {pat_desc=desc; pat_typ=typ'}



let conv_primitive_app t ts typ =
  match t.desc,ts with
  | Var {Id.name="List.length"}, [t1] -> make_length t1
  | Var {Id.name="Pervasives.@@"}, [t1;t2] -> Term.(t1 @ [t2])
  | Var {Id.name="Pervasives.="}, [t1;t2] -> Term.(t1 = t2)
  | Var {Id.name="Pervasives.<>"}, [t1;t2] -> Term.(t1 <> t2)
  | Var {Id.name="Pervasives.<"}, [t1;t2] -> Term.(t1 < t2)
  | Var {Id.name="Pervasives.>"}, [t1;t2] -> Term.(t1 > t2)
  | Var {Id.name="Pervasives.<="}, [t1;t2] -> Term.(t1 <= t2)
  | Var {Id.name="Pervasives.>="}, [t1;t2] -> Term.(t1 >= t2)
  | Var {Id.name="Pervasives.&&"}, [t1;t2] -> Term.(t1 && t2)
  | Var {Id.name="Pervasives.||"}, [t1;t2] -> Term.(t1 || t2)
  | Var {Id.name="Pervasives.+"}, [t1;t2] -> Term.(t1 + t2)
  | Var {Id.name="Pervasives.-"}, [t1;t2] -> Term.(t1 - t2)
  | Var {Id.name="Pervasives.*"}, [t1;t2] -> Term.(t1 * t2)
  | Var {Id.name="Pervasives./"}, [t1;t2] ->
      let t2' =
        if !Flag.Method.check_div_operand then
          let make_check t = make_seq (make_assert (make_neq t @@ make_int 0)) t in
          if has_no_effect t2 then
            make_check t2
          else
            let x = Id.new_var Ty.int in
            make_let [x,t2] @@ make_check @@ make_var x
        else
          t2
      in
      if !Flag.Method.abst_div then
        (Flag.add_use_abst "abst_div";
         Term.(seq t1 (seq t2' randi)))
      else
        Term.(t1 / t2')
  | Var {Id.name="Pervasives.~-"}, [t] -> Term.(~- t)
  | Var {Id.name="Pervasives.not"}, [t] -> Term.(not t)
  | Var {Id.name="Pervasives.fst"}, [t] -> Term.(fst t)
  | Var {Id.name="Pervasives.snd"}, [t] -> Term.(snd t)
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
  | Var {Id.name="Random.bool"}, [{desc=Const Unit}] -> randbool_unit_term
  | Var {Id.name="Random.int"}, [{desc=Const (Int 0)}] -> randint_unit_term
  | Var {Id.name="Random.int"}, [t] ->
      let x = Id.new_var ~name:"n" Ty.int in
      Term.(let_ [x, randi] (assume (int 0 <= var x && var x < t) (var x)))
  | Var {Id.name="Pervasives.open_in"}, [{desc=Const(Int _)}] -> make_event_unit "newr"
  | Var {Id.name="Pervasives.close_in"}, [{typ=TBase TUnit}] -> make_event_unit "close"
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
  | Types.Val_reg -> Format.eprintf "Val_reg@."; assert false
  | Types.Val_prim prim_desc -> Id.new_var (prim_desc.Primitive.prim_name)
  | Types.Val_ivar _ -> Format.eprintf "Val_ivar@."; assert false
  | Types.Val_self _ -> Format.eprintf "Val_self@."; assert false
  | Types.Val_anc _ -> Format.eprintf "Val_anc@."; assert false
  | Types.Val_unbound -> Format.eprintf "Val_unbound@."; assert false

let from_constant = function
  | Const_int n -> Int n
  | Const_char c -> Char c
  | Const_string(s, None) -> String s
  | Const_string(s, Some _) -> String s
  | Const_float s -> Float (float_of_string s)
  | Const_int32 n -> Int32 n
  | Const_int64 n -> Int64 n
  | Const_nativeint n -> Nativeint n


let is_var_case case =
  match case with
  | {c_lhs={pat_desc=Tpat_var _}; c_guard=None} -> true
  | {c_lhs={pat_desc=Tpat_alias({pat_desc=Tpat_any},_,_)}; c_guard=None} -> true
  | _ -> false

let rec from_expression id_env {exp_desc; exp_loc; exp_type=typ; exp_env=env} : (string * typ) list * term =
  let decls1,typ' = from_type_expr env typ in
  let decls2,t =
    match exp_desc with
    | Texp_ident(path, _, ty_desc) ->
        let decls,ty' = from_type_expr env ty_desc.val_type in
        let decls' =
          match typ' with
          | TData s -> (s, ty')::decls
          | _ -> decls
        in
        decls', make_var @@ from_ident_path id_env path typ'
    | Texp_constant c ->
        [], {desc = Const (from_constant c); typ = typ'; attr=const_attr}
    | Texp_let(rec_flag, [{vb_pat;vb_expr}], e2)
         when (function Tpat_var _ -> false | _ -> true) vb_pat.pat_desc ->
        let decls0,p' = from_pattern vb_pat in
        let decls1,t1 = from_expression id_env vb_expr in
        let decls2,t2 = from_expression id_env e2 in
        decls0 @ decls1 @ decls2,
        make_single_match t1 p' t2
    | Texp_let(Asttypes.Recursive, pats, e) ->
        let declss1,ps = List.split_map (fun {vb_pat} -> from_pattern vb_pat) pats in
        let id_env' = List.fold_right (fun p env -> get_bv_pat p @ env) ps id_env in
        let aux p {vb_expr} =
          match p.pat_desc with
          | PVar x ->
              let decls,t = from_expression id_env' vb_expr in
              decls, (x, t)
          | _ -> unsupported "Only variables are allowed as left-hand side of 'let rec ... and ...'"
        in
        let declss2,bindings = List.split @@ List.map2 aux ps pats in
        let decls2,t = from_expression id_env e in
        List.flatten declss1 @ List.flatten declss2 @ decls2,
        make_let bindings t
    | Texp_let(Asttypes.Nonrecursive, pats, e) ->
        let decls1,(ps,ts) =
          let aux {vb_pat;vb_expr} =
            let decls1,pat = from_pattern vb_pat in
            let decls2,t = from_expression id_env vb_expr in
            decls1@decls2, (pat,t)
          in
          let declss,psts = List.split_map aux pats in
          List.flatten declss, List.split psts
        in
        let p =
          match ps with
          | [p] -> p
          | _ -> make_ptuple ps
        in
        let t1 =
          match ts with
          | [t] -> t
          | _ -> make_tuple ts
        in
        let decls2,t2 =
          let id_env' = get_bv_pat p @ id_env in
          from_expression id_env' e
        in
        decls1 @ decls2,
        make_match t1 [p, true_term, t2]
    | Texp_function(_,[case],Total) when is_var_case case ->
        begin
          match from_case id_env case with
          | decls, ({pat_desc=PVar x}, _, e) -> decls, make_fun x e
          | _ -> assert false
        end
    | Texp_function(_,pats,totality) ->
        let decls2,x,typ2 =
          match typ' with
          | TFun(x,typ2) ->
              let decls,x' = (* for readable variable names *)
                let decls,p = from_pattern (List.hd pats).c_lhs in
                let x' =
                  match p with
                  | {pat_desc=PTuple ps} ->
                      let xs = List.map (function {pat_desc=PVar x} -> Some x | _ -> None) ps in
                      if List.for_all Option.is_some xs then
                        try
                          xs
                          |> List.map (Id.name -| Option.get)
                          |> String.join "__"
                          |> Id.set_name x
                        with Option.No_value -> assert false
                      else
                        x
                  | _ -> x
                in
                decls, x'
              in
              decls, x', typ2
          | _ -> assert false
        in
        let declss,pats' = List.split_map (from_case id_env) pats in
        let pats'' =
          match totality with
          | Total -> pats'
          | Partial -> (make_pvar (Id.new_var_id x), true_term, make_fail typ2)::pats'
        in
        let t =
          match pats'' with
          | [{pat_desc=PAny},{desc=Const True},t'] -> make_fun x t'
          | [{pat_desc=PVar y},{desc=Const True},t'] -> make_fun x @@ subst_var y x t'
          | [{pat_desc=PConst{desc=Const Unit}},{desc=Const True},t'] -> make_fun x t'
          | _ -> make_fun x @@ make_match (make_var x) pats''
        in
        decls2 @ List.flatten declss, t
    | Texp_apply(e, es) ->
        let decls2,t = from_expression id_env e in
        let aux i (a,b) =
          match a,b with
          | _, None -> unsupported "expression (optional)"
          | Optional _, Some e ->
              (* I don't know why, but the type environment of e is not appropriate for this context *)
              from_expression id_env {e with exp_env=env}
          | _, Some e -> from_expression id_env e
        in
        let declss,ts = List.split @@ List.mapi aux es in
        decls2 @ List.flatten declss,
        conv_primitive_app t ts typ'
    | Texp_match(e,pats,[],tp) ->
        let decls2,t = from_expression id_env e in
        let declss,pats' = List.split_map (from_case id_env) pats in
        let pats'' =
          match tp with
          | Total -> pats'
          | Partial -> pats' @ [make_pvar (Id.new_var t.typ), true_term, make_fail typ']
        in
        decls2 @ List.flatten declss,
        make_match t pats''
    | Texp_match(e,pats,_,tp) -> unsupported "Texp_match (exception)"
    | Texp_try(e,pats) ->
        let typ_excep = !!exc_typ in
        let x = Id.new_var ~name:"e" typ_excep in
        let declss,pats' = List.split_map (from_case id_env) pats in
        let pats'' = pats' @ [make_pany typ_excep, true_term, {desc=Raise(make_var x); typ=typ'; attr=[]}] in
        let decls2,t = from_expression id_env e in
        decls2 @ List.flatten declss,
        {desc=TryWith(t, Term.(fun_ x (match_ (var x) pats''))); typ=typ'; attr=[]}
    | Texp_tuple es ->
        let declss,ts = List.split_map (from_expression id_env) es in
        decls1 @ List.flatten declss,
        {desc=Tuple ts; typ=typ'; attr=[]}
    | Texp_construct(_,desc,es) ->
        let declss,ts = List.split_map (from_expression id_env) es in
        let desc =
          match get_constr_name desc typ env, ts with
          | "()",[] -> Const Unit
          | "true",[] -> Const True
          | "false",[] -> Const False
          | "[]",[] -> Nil
          | "::",[t1;t2] -> Cons(t1, t2)
          | "None",[] -> TNone
          | "Some",[t] -> TSome t
          | "Format",_ -> Const (String "%some format%")
          | name,es ->
              add_exc_env_from_constr desc env;
              Debug.printf "CONSTR: %s@." name;
              Debug.printf "   typ: %a@." Printtyp.type_expr typ;
              Constr(name, ts)
        in
        List.flatten declss,
        {desc; typ=typ'; attr=[]}
    | Texp_variant(name, expr) ->
        let decls2,ts =
          match expr with
          | None -> [], []
          | Some expr' ->
              let decls,t = from_expression id_env expr' in
              let ts =
                match t with
                | {desc=Tuple ts} -> ts
                | _ -> [t]
              in
              decls, ts
        in
        let desc = Constr(name, ts) in
        decls2, {desc; typ=typ'; attr=[]}
    | Texp_record(fields, None) ->
        let fields' = List.sort (fun (_,lbl1,_) (_,lbl2,_) -> compare lbl1.lbl_pos lbl2.lbl_pos) fields in
        let aux (_,label,e) =
          let decls,t = from_expression id_env e in
          decls, (get_label_name label env, t)
        in
        let declss,fields'' = List.split_map aux fields' in
        List.flatten declss,
        {desc=Record fields''; typ=typ'; attr=[]}
    | Texp_record(fields, Some init) ->
        let labels = Array.to_list (Triple.snd @@ List.hd fields).lbl_all in
        let r = Id.new_var ~name:"r" typ' in
        let declss,fields' =
          let aux lbl =
            let name = get_label_name lbl env in
            try
              let _,_,e = List.find (fun (_,lbl',_) -> lbl.lbl_name = lbl'.lbl_name) fields in
              let decls,t = from_expression id_env e in
              decls, (name, t)
            with Not_found ->
              let decls,typ = from_type_expr env lbl.lbl_arg in
              decls, (name, {desc=Field(make_var r,name); typ; attr=[]})
          in
          List.split_map aux labels
        in
        let decls2,t = from_expression id_env init in
        decls2 @ List.flatten declss,
        make_let [r, t] {desc=Record fields';typ=typ'; attr=[]}
    | Texp_field(e,_,label) ->
        let decls2,t = from_expression id_env e in
        decls2,
        {desc=Field(t, get_label_name label env); typ=typ'; attr=make_attr[t]}
    | Texp_setfield(e1,_,label,e2) ->
        let decls2,t1 = from_expression id_env e1 in
        let decls3,t2 = from_expression id_env e2 in
        decls2 @ decls3,
        {desc=SetField(t1, get_label_name label env, t2); typ=typ'; attr=[]}
    | Texp_array es ->
        let typ'' = array_typ typ' in
        let declss,ts = List.split_map (from_expression id_env) es in
        let array_of_list = make_var @@ Id.new_var ~name:"Array.of_list" ~attr:[Id.External] @@ make_tfun (make_tlist typ'') typ' in
        List.flatten declss,
        make_app array_of_list [List.fold_right make_cons ts (make_nil typ'')]
    | Texp_ifthenelse(e1,e2,e3) ->
        let decls2,t1 = from_expression id_env e1 in
        let decls3,t2 = from_expression id_env e2 in
        let decls4,t3 =
          match e3 with
          | None -> [], unit_term
          | Some e3 -> from_expression id_env e3
        in
        decls2 @ decls3 @ decls4,
        make_if t1 t2 t3
    | Texp_sequence(e1,e2) ->
        let decls2,t1 = from_expression id_env e1 in
        let decls3,t2 = from_expression id_env e2 in
        decls2 @ decls3,
        make_seq t1 t2
    | Texp_while(e1,e2) ->
        let decls2,t1 = from_expression id_env e1 in
        let decls3,t2 = from_expression id_env e2 in
        let x = Id.new_var Ty.unit in
        let f = Id.new_var ~name:"while" Ty.(fun_ unit unit) in
        let t2' = Term.(if_ t1 (seq t2 (var f @ [unit])) unit) in
        decls2 @ decls3,
        Term.(let_ [f, fun_ x t2'] (var f @ [unit]))
    | Texp_for(x, _, e1, e2, dir, e3) ->
        let decls2,t1 = from_expression id_env e1 in
        let decls3,t2 = from_expression id_env e2 in
        let decls4,t3 = from_expression id_env e3 in
        let x' = from_ident x Ty.int in
        if Type.can_unify t3.typ Ty.unit then
          Type.unify t3.typ Ty.unit
        else
          unsupported "The body of a for-expression must have type unit";
        let f = Id.new_var ~name:"for" Ty.(TFun(Id.new_var ~name:"i" int, unit)) in
        let init = Id.new_var ~name:"init" Ty.int in
        let last = Id.new_var ~name:"last" Ty.int in
        let t =
          if !Flag.Method.abst_for_loop then
            let op =
              match dir with
              | Upto -> Term.(<=)
              | Downto -> Term.(>=)
            in
            Term.(if_ (op (var init) (var last)) (let_ [x', randi] (assume (var init <= var x' && var x' <= var last) t3)) unit)
          else
            let t31 =
              match dir with
              | Upto -> Term.(var x' <= var last)
              | Downto -> Term.(var x' >= var last)
            in
            let t32 =
              let x'' =
                match dir with
                | Upto -> Term.(var x' + int 1)
                | Downto -> Term.(var x' - int 1)
              in
              Term.(seq t3 (var f @ [x'']))
            in
            Term.(let_ [f, fun_ x' (if_ t31 t32 unit)] (var f @ [var init]))
        in
        decls2 @ decls3 @ decls4, Term.(lets [init,t1; last,t2] t)
    | Texp_send _
    | Texp_new _ -> unsupported "expression (class)"
    | Texp_instvar _ -> unsupported "expression (instvar)"
    | Texp_setinstvar _ -> unsupported "expression (setinstvar)"
    | Texp_override _ -> unsupported "expression (override)"
    | Texp_letmodule _ -> unsupported "expression (module)"
    | Texp_assert e ->
        let decls,t = from_expression id_env e in
        let t' =
          if t.desc = Const False
          then make_fail typ'
          else make_assert t
        in
        decls, t'
    | Texp_lazy e -> assert false
    | Texp_object _ -> unsupported "expression (class)"
    | Texp_pack _ -> unsupported "expression (pack)"
    | Texp_unreachable -> unsupported "Texp_unreachable"
    | Texp_extension_constructor _ -> unsupported "Texp_extension_constructor"
  in
  decls1@decls2, t

and from_case id_env {c_lhs;c_guard;c_rhs} =
  let decls,p = from_pattern c_lhs in
  let decls1,cond =
    match c_guard with
    | None -> [], true_term
    | Some e -> from_expression id_env e
  in
  let decls2,t =
    let id_env' = get_bv_pat p @ id_env in
    from_expression id_env' c_rhs
  in
  decls1@decls2, (p, cond, t)


let from_exception_declaration decls = List.map from_type_expr decls


let rec from_module_binding id_env tenv mb
        : id list * (Asttypes.rec_flag * Syntax.declaration) list =
  let id_env',mdl = from_module_expr id_env mb.mb_expr in
  let m = from_ident mb.mb_id mdl.typ in
  id_env', [Nonrecursive, Decl_let [m,mdl]]

and from_module_type mty =
  match mty with
  | Mty_ident path -> TData (Path.name path)
  | Mty_signature sgn -> TModule []
  | Mty_functor(id, None, mty2) -> unsupported "Mty_functor"
  | Mty_functor(id, Some mty1, mty2) -> TFun(from_ident id @@ from_module_type mty1, from_module_type mty2)
  | Mty_alias path -> TData (Path.name path)

and from_module_expr id_env mb_expr =
  match mb_expr.mod_desc with
  | Tmod_structure struc ->
      let id_env', decls = from_structure id_env struc in
      let mdl = make_module @@ List.map snd decls in
      let id_env'' =
        let map =
          let aux (_,decl) =
            match decl with
            | Decl_let defs -> List.map fst defs
            | Decl_type _ -> []
          in
          List.flatten_map aux decls
        in
        map @ id_env'
      in
      id_env'', mdl
  | Tmod_ident(path,loc) ->
      let ty = from_module_type mb_expr.mod_type in
      id_env, make_var @@ Id.make 0 (Path.name path) [] ty
  | Tmod_functor(id, loc, mty, expr) ->
      let ty =
        match mty with
        | None -> unsupported "Tmod_functor"
        | Some {mty_desc;mty_type;mty_env;mty_loc;mty_attributes} ->
            match mty_desc with
            | Tmty_ident(path,_) -> TData (Path.name path)
            | _ -> assert false
      in
      let m = from_ident id ty in
      let id_env',mdl = from_module_expr id_env expr in
      id_env', make_fun m mdl
  | Tmod_apply(e1,e2,_) ->
      let id_env',t1 = from_module_expr id_env e1 in
      let id_env'',t2 = from_module_expr id_env e2 in
      id_env'', make_app t1 [t2]
  | Tmod_constraint(expr, _, _, _) -> from_module_expr id_env expr
  | Tmod_unpack _ -> unsupported "Tmod_unpack"

and from_str_item id_env str_item
    : id list * (Asttypes.rec_flag * Syntax.declaration) list =
  let tenv = str_item.str_env in
  let make_tdecl decls =
    let decls' =
      decls
      |> List.filter_out (fun (s,ty) -> TData s = ty)
      |> List.sort_unique (Compare.on fst)
    in
    [Nonrecursive, Decl_type decls']
  in
  match str_item.str_desc with
  | Tstr_eval(e,_) ->
      let decls,t = from_expression id_env e in
      id_env, make_tdecl decls @ [Nonrecursive, Decl_let[Id.new_var ~name:"u" t.typ, t]]
  | Tstr_value(Asttypes.Recursive,pats) ->
      let aux {vb_pat;vb_expr} =
        let decls1,p = from_pattern vb_pat in
        let decls2,e = from_expression id_env vb_expr in
        match p.pat_desc with
        | PVar x -> decls1@decls2, (x, e)
        | _ -> fatal "Only variables are allowed as left-hand side of 'let rec'"
      in
      let declss,pats' = List.split_map aux pats in
      id_env, make_tdecl (List.flatten declss) @ [Recursive, Decl_let pats']
  | Tstr_value(Asttypes.Nonrecursive,pats) ->
      let aux {vb_pat;vb_expr} =
        let decls1,p = from_pattern vb_pat in
        let decls2,t = from_expression id_env vb_expr in
        let x,t =
          match p.pat_desc with
          | PVar x -> x, t
          | PConst {desc=Const Unit} -> Id.new_var ~name:"u" Ty.unit, t
          | PAny -> new_var_of_term t, t
          | _ -> unsupported "Only variables are allowed as left-hand side of 'let'"
        in
        make_tdecl (decls1@decls2) @ [Nonrecursive, Decl_let [x,t]]
      in
      id_env, List.flatten_map aux pats
  | Tstr_primitive _ -> id_env, []
  | Tstr_type(rec_flag,decls) ->
      let flag = from_rec_flag rec_flag in
      if flag = Nonrecursive then unsupported "Non recursive type declaration";
      let declss1,decls2 = List.split_map (from_type_declaration tenv) decls in
      let decls1 = List.flatten declss1 in
      id_env, make_tdecl decls1 @ [flag, Decl_type decls2]
  | Tstr_typext _ -> unsupported "typext"
  | Tstr_exception _ -> id_env, []
  | Tstr_module mb -> from_module_binding id_env tenv mb
  | Tstr_recmodule _
  | Tstr_modtype _ -> id_env, []
  | Tstr_open _ -> id_env, []
  | Tstr_class _
  | Tstr_class_type _ -> unsupported "class"
  | Tstr_include _ -> id_env, []
  | Tstr_attribute _ -> id_env, []

and from_structure id_env struc
    : id list * (Asttypes.rec_flag * Syntax.declaration) list =
  Debug.printf "struc: @[%a@." Printtyped.implementation struc;
  let aux (id_env,decls) str_item =
    let id_env',decls' = from_str_item id_env str_item in
    id_env', decls @ decls'
  in
  List.fold_left aux (id_env,[]) struc.str_items

let from_top_level_phrase (tenv,id_env,decls) ptop =
  match ptop with
  | Parsetree.Ptop_dir _ -> unsupported "toplevel_directive"
  | Parsetree.Ptop_def struc ->
      let struc',_,tenv' = Typemod.type_structure tenv struc Location.none in
      let id_env',decls' = from_structure id_env struc' in
      tenv', id_env', decls @ decls'

let make_local' (flag,decl) t =
  match flag,decl with
  | Nonrecursive, Decl_let defs ->
      let map =
        let fv = get_fv @@ make_tuple @@ List.map snd defs in
        defs
        |> List.map fst
        |> List.filter @@ Id.mem -$- fv
        |> List.map @@ Pair.add_right Id.new_var_id
      in
      let aux (x,t1) t2 =
        let x',t2' =
          match Id.assoc_option x map with
          | Some x' -> x', subst_var x x' t2
          | None -> x, t2
        in
        make_let [x',t1] t2'
      in
      List.fold_right aux defs t
  | _ -> make_local decl t


let from_use_file ast =
  let env = Compmisc.initial_env () in
  init_exc_env ();
  ast
  |> List.fold_left from_top_level_phrase (env,[],[])
  |> Triple.trd
  |> List.fold_right make_local' -$- end_of_definitions
  |> subst_tdata "exn" !!exc_typ
