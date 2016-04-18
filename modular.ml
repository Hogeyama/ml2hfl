open Util
open Syntax
open Term_util
open Type

let debug () = List.mem "Modular" !Flag.debug_module

let remove_ext_def = make_trans2 ()

let remove_ext_def_desc ext desc =
  match desc with
  | Let(flag, bindings, t) ->
      let bindings' = List.filter_out (fun (f,_,_) -> Id.mem f ext) bindings in
      let t' = remove_ext_def.tr2_term ext t in
      if bindings' = [] then t'.desc else Let(flag, bindings', t')
  | _ -> remove_ext_def.tr2_desc_rec ext desc

let () = remove_ext_def.tr2_desc <- remove_ext_def_desc
let remove_ext_def = remove_ext_def.tr2_term

let divide spec t ref_env =
  if !!debug then Format.printf "PROGRAM: %a@." Print.term t;
  if !!debug then Format.printf "ORIG: %a@." (List.print Print.id) @@ get_top_funs t;
  let ext = List.map fst ref_env in
  let t_main = remove_ext_def ext t in
  if !!debug then Format.printf "MAIN: %a@." (List.print Print.id) @@ get_top_funs t_main;
  let make_spec f =
    let ref_env,ext_ref_env = List.partition (Id.same f -| fst) ref_env in
    let aux (_,typ) =
      if not @@ Type.same_shape (Id.typ f) (Ref_type.to_simple typ) then
        begin
          Format.printf "VAR: %a@." Id.print f;
          Format.printf "  Prog: %a@." Print.typ @@ Id.typ f;
          Format.printf "  Spec: %a@." Ref_type.print typ;
          fatal @@ Format.sprintf "Type of %s in the specification is wrong?" @@ Id.name f
        end
    in
    List.iter aux ref_env;
    let spec' = {spec with Spec.ref_env; Spec.ext_ref_env = ext_ref_env @ spec.Spec.ext_ref_env} in
    if !!debug then Format.printf "SUB[%a]: %a@." Print.id f Spec.print spec';
    spec'
  in
  let targets = List.map (fun f -> Id.to_string f, make_spec f, t) ext in
  if !!debug then Format.printf "MAIN: %a@." Print.term t_main;
  ("MAIN", make_spec (Id.new_var ~name:"MAIN" TUnit), t_main)::targets


let main orig spec parsed =
  let verify (s,spec,t) =
    if !!debug then Format.printf "Start verification of %s:@.%a@." s Spec.print spec;
    s, Main_loop.run orig [] ~spec t
  in
  Spec.get_ref_env spec parsed
  |@(not !Flag.only_result)&> Spec.print_ref_env Format.std_formatter
  |> divide spec parsed
  |> List.map verify
  |@> Format.printf "RESULT: %a@." (List.print @@ Pair.print Format.pp_print_string Format.pp_print_bool)
  |> List.for_all snd





(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)




type constr =
  | Exp of typed_term
  | And of constr * constr
  | Imply of constr * constr
  | Sub of type_template * type_template
  | Pred of fun_id
and type_template =
  | Var of fun_id
  | Arg of int * type_template
  | App of type_template * typed_term list
  | Singleton of typed_term
  | Base of pred_var option
  | Fun of id * type_template * type_template
  | Inter of type_template list
and fun_id = id * Comp_tree.id_kind
and pred_var = int
and horn_clause = typed_term list * typed_term
and horn_clauses = horn_clause list

let pred_var = Id.make (-1) "v" TInt
let pred_var_term = make_var pred_var

let rec print_constr fm = function
  | Exp t -> Format.fprintf fm "%a" Print.term t
  | And(c1, c2) -> Format.fprintf fm "(@[%a@ &&@ %a@])" print_constr c1 print_constr c2
  | Imply(c1, c2) -> Format.fprintf fm "(@[%a@ =>@ %a@])" print_constr c1 print_constr c2
  | Sub(typ1, typ2) -> Format.fprintf fm "(@[%a@ <:@ %a@])" print_template typ1 print_template typ2
  | Pred x -> Format.fprintf fm "P[%a]" print_fun_id x
and print_fun_id fm (f, kind) =
  match kind with
  | Comp_tree.Local -> Format.fprintf fm "%a" Id.print f
(*
  | Some (tid, None, None) -> Format.fprintf fm "%a:%a" Id.print f print_tid tid
 *)
  | Comp_tree.NewThread(tid, n) -> Format.fprintf fm "%a:%a*%d" Id.print f print_tid tid n
  | Comp_tree.NthThread(tid, i) -> Format.fprintf fm "%a:%a-%d" Id.print f print_tid tid i
and print_tid = Format.pp_print_int

and print_template fm = function
  | Var f -> print_fun_id fm f
  | Arg(i, typ) -> Format.fprintf fm "#%d(%a)" i print_template typ
  | App(typ, []) -> print_template fm typ
  | App(typ, ts) -> Format.fprintf fm "%a%a" print_template typ (List.print Print.term) ts
  | Singleton t -> Format.fprintf fm "{%a}" Print.term t
  | Base None -> Format.fprintf fm "unit"
  | Base (Some p) -> Format.fprintf fm "P%d" p
  | Fun(x, tmp1, tmp2) when is_fun_typ @@ Id.typ x -> Format.fprintf fm "(@[<hov 2>%a ->@ %a@])" print_template tmp1 print_template tmp2
  | Fun(x, tmp1, tmp2) -> Format.fprintf fm "(@[<hov 2>%a:%a ->@ %a@])" Id.print x print_template tmp1 print_template tmp2
  | Inter [] -> Format.fprintf fm "T"
  | Inter tmps -> Format.fprintf fm "@[%a@]" (print_list print_template " /\\@ ") tmps

let print_horn_clause fm (pre,constr) =
  let pr_aux fm t =
    Format.fprintf fm "@[%s@]" @@ String.replace_chars (function '_' -> "" | c -> String.of_char c) @@ Format.asprintf "%a" Print.term t
  in
  let pr fm t =
    match t.desc with
    | Var p when String.starts_with (Id.name p) "P" -> Format.fprintf fm "%a()" pr_aux t
    | App(p, ts) -> Format.fprintf fm "@[%a(%a)@]" pr_aux p (print_list pr_aux ",") ts
    | _ -> pr_aux fm t
  in
  if constr = false_term
  then Format.fprintf fm "@[?- %a.@]" (print_list pr " &&@ ") pre
  else Format.fprintf fm "@[%a :- %a.@]" pr constr (print_list pr " &&@ ") pre
let print_horn_clauses fm hcs =
  Format.fprintf fm "@[%a@]" (print_list print_horn_clause "@\n") hcs

let _And c1 c2 =
  if c1 = Exp true_term then
    c2
  else if c2 = Exp true_term then
    c1
  else
    And(c1, c2)
let _Ands constrs = List.fold_right _And constrs (Exp true_term)
let _App typ map = if map=[] then typ else App(typ, map)
let rec _Imply cs c =
  if c = Exp true_term then
    Exp true_term
  else
    match cs with
    | [] -> c
    | c1::cs' ->
        if c1 = Exp true_term then
          _Imply cs' c
        else Imply(c1, _Imply cs' c)
let _Inter tmps =
  match tmps with
  | [tmp] -> tmp
  | _ -> Inter tmps

let rec add_bind f map typ =
  match typ with
  | Var(g, _) -> if Id.same f g then _App typ map else typ
  | Arg(i, typ) -> Arg(i, add_bind f map typ)
  | App(typ1, map1) -> _App (add_bind f map typ1) map1
  | Singleton _ -> typ
  | _ -> assert false
let add_bind f map typ =
  let map' = List.filter_out (is_fun_typ -| Id.typ) map in
  add_bind f (List.map make_var map') typ

let typ_of t =
  match t.desc with
  | Syntax.Var f -> Var (f, Comp_tree.Local)
  | _ -> Singleton t

let rec apply f = function
  | Exp t -> Exp t
  | And(c1,c2) -> And(apply f c1, apply f c2)
  | Imply(c1, c2) -> Imply(apply f c1, apply f c2)
  | Sub(typ1, typ2) -> Sub(f typ1, f typ2)
  | Pred x -> Pred x

let rec get_fv_typ = function
  | Var(f, Comp_tree.Local) -> [f]
  | Var(f, Comp_tree.NewThread _) -> []
  | Var(f, Comp_tree.NthThread _) -> [f]
  | Arg(_,  typ) -> get_fv_typ typ
  | App(typ, _) -> get_fv_typ typ
  | Singleton t -> Syntax.get_fv t
  | Base _ -> []
  | Fun(x,typ1,typ2) -> List.filter_out (Id.same x) @@ (get_fv_typ typ1 @ get_fv_typ typ2)
  | Inter typs -> List.flatten_map get_fv_typ typs
let rec get_fv = function
  | Exp t -> Syntax.get_fv t
  | And(c1, c2) -> get_fv c1 @ get_fv c2
  | Imply _ -> assert false
  | Sub(typ1,typ2) -> get_fv_typ typ1 @ get_fv_typ typ2
  | Pred(x, _) -> [x]

let filter_assumption env assumption =
  let check constr =
    let fv = get_fv constr in
    List.Set.subset fv env
  in
  List.filter check assumption

let rec generate_constraints templates tid_env var_env assumption (Rose_tree.Node(label, children)) =
  let dbg = true in
  let open Comp_tree in
  let r =
  let constr var_env' assumption' = List.flatten_map (generate_constraints templates tid_env var_env' assumption') children in
  match label with
  | Term {desc=If _} -> constr var_env assumption
  | Term {desc=App(t1, ts)} ->
      let f = Option.get @@ decomp_var t1 in
      let tid = get_id_option t1 in
      let fun_id =
        let kind =
          match tid with
          | None -> Local
          | Some tid ->
              match List.assoc_option tid tid_env with
              | None -> NewThread(tid, List.length children)
              | Some i -> NthThread(tid, i)
        in
        f, kind
      in
      let constr1 =
        let _,_,typs =
          let aux (i,map,typs) t =
            i+1,
            (if is_fun_typ t.typ then map else map@[t]),
            typs@[App(Arg(i, Var fun_id), map)]
          in
          List.fold_left aux (0,[],[]) ts
        in
        List.map2 (fun typ t -> _Imply assumption @@ Sub(typ_of t, typ)) typs ts
      in
      let constr2 =
        if Option.for_all (List.mem_assoc -$- tid_env) tid then
          constr var_env assumption
        else
          let tid = Option.get tid in
          let var_env' =
            match fun_id with
            | _, NthThread _ -> f::var_env
            | _ -> var_env
          in
          List.flatten @@ List.mapi (fun i child -> generate_constraints templates ((tid,i)::tid_env) var_env assumption child) children
      in
      constr1 @ constr2
  | Term _ -> assert false
  | Bind(None, var_env', map) ->
      assert (List.for_all (is_fun_typ -| Id.typ -| Triple.fst) map);
      let assumption' = filter_assumption var_env' assumption in
      let var_env'' = List.map Triple.fst map @ var_env' in
      constr var_env'' assumption'
  | Bind(Some t, var_env', map) ->
      let fun_id =
        let f = Option.get @@ decomp_var t in
        let tid = get_id_option t in
        let kind =
          match tid with
          | None -> Local
          | Some tid ->
              match List.assoc_option tid tid_env with
              | None -> NewThread(tid, List.length children)
              | Some i -> NthThread(tid, i)
        in
        f, kind
      in
      let add typ = List.fold_left (fun typ (x,env,_) -> add_bind x env typ) typ map in
      let aux (i,acc) (x,_,v) =
        let acc' =
          if is_fun_typ @@ Id.typ x
          then acc
          else Pred (x, Local) :: acc
        in
        i+1, acc'
      in
      let var_env'' = List.map Triple.fst map @ var_env' in
      let _,assumption' = List.fold_left aux (0,assumption) map in
      let assumption'' = filter_assumption var_env'' assumption' in
      if dbg then Format.printf "  label: %a@." print_label label;
      if dbg then Format.printf "  var_env'': %a@." (List.print Id.print) var_env'';
      if dbg then Format.printf "  assumption': %a@." (List.print print_constr) assumption';
      if dbg then Format.printf "  assumption'': %a@.@." (List.print print_constr) assumption'';
      List.map (apply add) @@ constr var_env'' assumption''
  | Assume t ->
      constr var_env (Exp t::assumption)
  | Fail ->
      assert (children = []);
      [_Imply assumption @@ Exp false_term]
in
if dbg then Format.printf "label: %a@." print_label label;
if dbg then Format.printf "assumption: %a@." (List.print print_constr) assumption;
if dbg then Format.printf "var_env: %a@." (List.print Id.print) var_env;
if dbg then Format.printf "r: %a@.@." (List.print print_constr) r;
r
let generate_constraints templates var_env comp_tree = generate_constraints templates [] var_env [] comp_tree


let merge_template ts = List.flatten ts
let new_pred cnt = Base (Some (Counter.gen cnt))
let rec make_template fun_env cnt (Rose_tree.Node(label, children)) =
  let dbg = false in
  let r=
  let mk_templates env = merge_template @@ List.map (make_template env cnt) children in
  let open Comp_tree in
  match label with
  | Term {desc=If _} -> mk_templates fun_env
  | Term {desc=App({desc=Var f} as t1, ts)} when not @@ List.mem (f, get_id_option t1) fun_env ->
      let f = Option.get @@ decomp_var t1 in
      let fun_env' = (f, get_id_option t1)::fun_env in
      let templates = mk_templates fun_env' in
      if dbg then Format.printf "APP: %a@." print_label label;
      if dbg then Format.printf "  TEMPLATE: %a@." (List.print @@ Pair.print Id.print print_template) templates;
      let aux (Rose_tree.Node(label', children')) =
        match label', children' with
        | Bind(_,_,map), [child]  ->
            let aux' t (x,_,_) tmp2 =
              let tmp1 =
                if is_fun_typ @@ Id.typ x then
                  try
                    Id.assoc x templates
                  with Not_found -> Inter []
                else
                  new_pred cnt
              in
              Fun(new_var_of_term t, tmp1, tmp2)
            in
            let r = List.fold_right2 aux' ts map (Base None) in
            if dbg then Format.printf "  map: %a@." (List.print Id.print) @@ List.map Triple.fst map;
            if dbg then Format.printf "  typ: %a@." print_template r;
            r
        | _ ->
            if dbg then Format.printf "%a@." print_label label;
            assert false
      in
      (f, _Inter @@ List.map aux children)::templates
  | Term {desc=App _} -> mk_templates fun_env
  | Term _ -> assert false
  | Bind(_,_,map) ->
      let map' = List.filter_out (is_fun_typ -| Id.typ -| Triple.fst) map in
      let templates = List.map (fun (x,_,_) -> x, new_pred cnt) map' in
      templates @ mk_templates fun_env
  | Assume _ -> mk_templates fun_env
  | Fail -> []
  in
  if dbg then Format.printf "  %a@." Comp_tree.print_label label;
  if dbg then Format.printf "  LEN: %d@." (List.length r); r
let make_template xs t =
  let cnt = Counter.create() in
  List.map (fun x -> x, new_pred cnt) xs @ make_template [] cnt t


let rec decomp_tfun typ =
  match typ with
  | Var _ -> assert false
  | Fun(x,typ1,typ2) -> Pair.map_fst (List.cons (x,typ1)) @@ decomp_tfun typ2
  | Inter [typ'] -> decomp_tfun typ'
  | Inter _ ->
      Format.printf "decomp_tfun: %a@." print_template typ;
      assert false
  | _ -> [], typ

let decomp_inter = function
  | Inter typs -> typs
  | typ -> [typ]

let rec apply_typ typ ts =
  match typ with
  | Var _ -> App(typ, ts)
  | Arg _ -> assert false
  | App(typ',ts') -> App(typ', ts'@ts)
  | Singleton _ -> typ
  | Base _ -> App(typ, ts)
  | Fun(x, typ1, typ2) -> Fun(x, apply_typ typ1 ts, apply_typ typ2 ts)
  | Inter typs -> Inter (List.map (apply_typ -$- ts) typs)

let rec decomp_base tmp =
  match tmp with
  | Base p -> Some (p,[])
  | App(tmp, ts) -> Option.map (Pair.map_snd @@ (-$-) (@) ts) @@ decomp_base tmp
  | Fun _ -> None
  | Inter _ -> None
  | Var _
  | Arg _
  | Singleton _ ->
      Format.printf "%a@." print_template tmp;
      assert false
let is_base = Option.is_some -| decomp_base

let rec decomp_var tmp =
  match tmp with
  | Var(f, kind) -> Some ((f, kind), [])
  | App(tmp, ts) -> Option.map (Pair.map_snd @@ (-$-) (@) ts) @@ decomp_var tmp
  | Base p -> None
  | Fun _ -> None
  | Inter _ -> None
  | Arg _
  | Singleton _ ->
      Format.printf "%a@." print_template tmp;
      assert false
let is_var = Option.is_some -| decomp_var

let rec inline_template_typ templates typ =
  let aux_rec = inline_template_typ templates in
  match typ with
  | Var(x, _) ->( try Id.assoc x templates with Not_found -> Format.printf "%a: %a@." Id.print x Print.typ (Id.typ x); assert false)
  | Arg(i, typ') when is_var typ' ->
      let (f,kind),ts = Option.get @@ decomp_var typ' in
      let typ'' =
        let aux j = snd @@ List.nth (fst @@ decomp_tfun @@ List.nth (decomp_inter @@ Id.assoc f templates) j) i in
        match kind with
        | Comp_tree.Local ->
            snd @@ List.nth (fst @@ decomp_tfun @@ Id.assoc f templates) i
        | Comp_tree.NewThread(tid, n) ->
            _Inter @@ List.init n aux
        | Comp_tree.NthThread(tid, j) ->
            aux j
      in
      apply_typ typ'' ts
  | Arg _ ->
      Format.printf "%a@." print_template typ;
      assert false
  | App(typ1, ts) -> apply_typ (aux_rec typ1) ts
  | Singleton t -> Singleton t
  | _ -> assert false
let rec inline_template templates constr =
  let aux_rec = inline_template templates in
  let aux_typ = inline_template_typ templates in
  if false then Format.printf "%a@.@." print_constr constr;
  match constr with
  | Exp t -> Exp t
  | And(c1, c2) -> And(aux_rec c1, aux_rec c2)
  | Imply(t, c) -> Imply(t, aux_rec c)
  | Sub(typ1, typ2) -> Sub(aux_typ typ1, aux_typ typ2)

let rec subst_template x t tmp =
  let sbst = subst_template x t in
  match tmp with
  | Var _ -> tmp
  | Arg(i, tmp') -> Arg(i, sbst tmp')
  | App(tmp', ts) -> App(sbst tmp', List.map (Term_util.subst x t) ts)
  | Singleton t' -> Singleton (Term_util.subst x t t')
  | Base _ -> tmp
  | Fun(x, tmp1, tmp2) -> Fun(x, sbst tmp1, sbst tmp2)
  | Inter tmps -> Inter (List.map sbst tmps)

let rec subst_constr x t constr =
  let sbst = subst_constr x t in
  match constr with
  | Exp t' -> Exp (Term_util.subst x t t')
  | And(c1, c2) -> _And (sbst c1) (sbst c2)
  | Imply(c1, c2) -> _Imply [sbst c1] (sbst c2)
  | Sub(tmp1, tmp2) -> Sub(subst_template x t tmp1, subst_template x t tmp2)

let term_of_base pred_map typ =
  let p,ts = Option.get @@ decomp_base typ in
  let p' = Option.get p in
  make_app (make_var @@ List.assoc p' pred_map) (pred_var_term::ts)
let rec decomp_constraints pred_map constr =
  let dsc = decomp_constraints pred_map in
  match constr with
  | Exp t -> Exp t
  | And(c1, c2) -> _And (dsc c1) (dsc c2)
  | Imply(Exp c, c2) -> assert false
  | Imply _ -> assert false
  | Sub(Singleton t1, typ2) when is_base typ2 ->
      _Imply [Exp t1] @@ Exp (term_of_base pred_map typ2)
  | Sub(typ1, typ2) when is_base typ1 && is_base typ2 ->
(*
      let p1,ts1 = Option.get @@ decomp_base typ1 in
      Imply(App()
 *)
      let p2,_ = Option.get @@ decomp_base typ2 in
      if p2 = None
      then Exp true_term
      else _Imply [Exp (term_of_base pred_map typ1)] @@ Exp (term_of_base pred_map typ2)
  | Sub(typ1, Inter typs) ->
      List.fold_left (fun acc typ2 -> _And acc (dsc @@ Sub(typ1, typ2))) (Exp true_term) typs
  | Sub(Fun(x1,typ11,typ12), Fun(x2,typ21,typ22)) when is_base typ11 ->
      assert (is_base typ21);
      let c1 = dsc @@ Sub(typ21,typ11) in
      let t = subst_var pred_var x2 @@ term_of_base pred_map typ21 in
      let c2 = subst_constr x1 (make_var x2) @@ dsc @@ Sub(typ12, typ22) in
      _And c1 @@ _Imply [Exp t] c2
  | Sub(Fun(x1,typ11,typ12), Fun(x2,typ21,typ22)) -> unsupported "decomp_constraints"
  | Sub _ ->
      Format.printf "%a@." print_constr constr;
      assert false

let rec pred_vars_typ typ =
  match typ with
  | Var _ -> []
  | Arg(_, typ') -> pred_vars_typ typ'
  | App(typ', ts) -> pred_vars_typ typ'
  | Singleton _ -> []
  | Base None -> []
  | Base (Some p) -> [p]
  | Fun(_, typ1, typ2) -> pred_vars_typ typ1 @ pred_vars_typ typ2
  | Inter typs -> List.flatten_map pred_vars_typ typs
let rec pred_vars constr =
  match constr with
  | Exp _ -> []
  | And(c1, c2) -> pred_vars c1 @ pred_vars c2
  | Imply(_, c) -> pred_vars c
  | Sub(typ1, typ2) -> pred_vars_typ typ1 @ pred_vars_typ typ2
let make_pred_map constr =
  List.map (fun i -> i, Id.make i "P" typ_unknown) @@ List.unique @@ pred_vars constr


let rec flatten constr =
  match constr with
  | Exp t -> [[], t]
  | And(c1, c2) -> flatten c1 @ flatten c2
  | Imply(Exp t, c) -> List.map (Pair.map_fst @@ List.cons t) @@ flatten c
  | Imply _ -> assert false
  | Sub _ -> assert false



let normalize = make_trans ()
let normalize_term t =
  let t' = normalize.tr_term_rec t in
  match t'.desc with
  | App(t1, ts) ->
      let ts',binds =
        let aux t2 =
          match t2.desc with
          | Fun _ ->
              let x = new_var_of_term t2 in
              make_var x, Some (x, [], t2)
          | _ -> t2, None
        in
        let ts',binds = List.split_map aux ts in
        ts', List.filter_map Fun.id binds
      in
      make_lets binds {t' with desc=App(t1, ts')}
  | _ -> t'
let () = normalize.tr_term <- normalize_term
let normalize = normalize.tr_term



let infer spec parsed ce_set =
  let normalized =
    if true
    then normalize parsed
    else Trans.inline_var @@ Trans.flatten_let @@ Trans.normalize_let parsed
  in
  Format.printf "INPUT: %a@.@." Print.term normalized;
  let fbindings,main = decomp_prog normalized in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (snd |- List.map Triple.to_pair_r) fbindings in
  let ce_set =
    let aux =
      match !Flag.filename with
      | "test.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]] | _ -> assert false)
      | "test1.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test2.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test3.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "apply" -> [f, []] | "double" -> [f, []] | _ -> assert false)
      | "test4.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | _ -> assert false
    in
    List.flatten @@ List.map aux fun_env
  in
  let main = Option.get @@ get_last_definition normalized in
  let xs,comp_tree = Comp_tree.from_program fun_env ce_set main in
  let templates = make_template xs comp_tree in
  Format.printf "TEMPLATES: @[%a@.@." (List.print @@ Pair.print Id.print print_template) templates;
  let constrs = generate_constraints templates xs comp_tree in
  Format.printf "CONSTR1: @[%a@.@." (List.print print_constr) constrs;(*
  let constr = inline_template templates constr in
  Format.printf "CONSTR2: @[%a@.@." print_constr constr;
  let pred_map = make_pred_map constr in
  (*  Format.printf "PRED_MAP: @[%a@.@." (List.print @@ Pair.print Format.pp_print_int Id.print) pred_map;*)
  let constr = decomp_constraints pred_map constr in
  Format.printf "CONSTR3: @[%a@.@." print_constr constr;
  let constr = flatten constr in
  Format.printf "CONSTR4: @[%a@.@." print_horn_clauses constr;*)
  constrs
