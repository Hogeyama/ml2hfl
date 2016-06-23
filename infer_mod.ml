open Util
open Syntax
open Term_util
open Type
open Modular_syntax

let debug () = List.mem "Infer_mod" !Flag.debug_module

module CT = Comp_tree


type constr =
  | Exp of typed_term
  | And of constr * constr
  | Imply of constr * constr
  | Sub of type_template * type_template
  | Pred of id * typed_term list
and type_template =
  | Var of id
  | Arg of type_template * typed_term list
  | PApp of type_template * typed_term list
  | Singleton of typed_term
  | Base of (Ref_type.base * pred_var) option (** None represents the result type X *)
  | Const of id * typed_term
  | Fun of id * type_template * type_template
  | Inter of type_template list
and pred_var = int
and horn_clause = typed_term list * typed_term
and horn_clauses = horn_clause list

let pred_var = Id.make (-1) "v" TInt
let pred_var_term = make_var pred_var
let make_pred_var p ts =
  let typs = List.map Syntax.typ ts in
  let typ = List.fold_right make_tfun typs TInt in
  Id.make p "P" typ
let is_pred_var x = Id.name x = "P"

let rec print_constr fm = function
  | Exp t -> Format.fprintf fm "%a" Print.term t
  | And(c1, c2) -> Format.fprintf fm "@[<hov 3>(%a@ &&@ %a)@]" print_constr c1 print_constr c2
  | Imply(c1, c2) -> Format.fprintf fm "@[<hov 3>(%a@ =>@ %a)@]" print_constr c1 print_constr c2
  | Sub(typ1, typ2) -> Format.fprintf fm "@[<hov 3>(%a@ <:@ %a)@]" print_template typ1 print_template typ2
  | Pred(x,ts) -> Format.fprintf fm "@[P%a%a@]" Id.print x (List.print Print.term) ts

and print_template fm = function
  | Var f -> Id.print fm f
  | Arg(typ, ts) -> Format.fprintf fm "%a(%a)" print_template typ (print_list Print.term ",") ts
  (*  | PApp(typ, []) -> print_template fm typ*)
  | PApp(typ, ts) -> Format.fprintf fm "%a%a" print_template typ (List.print Print.term) ts
  | Singleton t -> Format.fprintf fm "{%a}" Print.term t
  | Base None -> Format.fprintf fm "unit"
  | Base (Some(base, p)) -> Format.fprintf fm "%a[P_%d]" Ref_type.print_base base p
  | Const(x,t) -> Format.fprintf fm "{%a:int | %a}" Id.print x Print.term t
  | Fun(x, tmp1, tmp2) when is_fun_var x -> Format.fprintf fm "(@[<hov 2>%a ->@ %a@])" print_template tmp1 print_template tmp2
  | Fun(x, tmp1, tmp2) -> Format.fprintf fm "(@[<hov 2>%a:%a ->@ %a@])" Id.print x print_template tmp1 print_template tmp2
  | Inter [] -> Format.fprintf fm "T"
  | Inter tmps -> Format.fprintf fm "(@[%a@])" (print_list print_template " /\\@ ") tmps

let print_tmp_env fm env =
  let print_idx fm (x,nid) =
    match nid with
    | None -> Format.fprintf fm "%a" Id.print x
    | Some nid' -> Format.fprintf fm "%a@%d" Id.print x nid'
  in
  Format.fprintf fm "%a" (List.print @@ Pair.print print_idx print_template) env

let print_horn_clause fm (pre,constr) =
  let pr_aux fm t =
    if true then
      Print.term fm t
    else (* For rcaml *)
      t
      |> Format.asprintf "%a" Print.term
      |> String.remove_char '_'
      |> String.remove_char '\''
      |> Format.fprintf fm "@[%s@]"
  in
  let pr fm t =
    match t.desc with
    | Var p when is_pred_var p -> Format.fprintf fm "%a()" pr_aux t
    | App(p, ts) -> Format.fprintf fm "@[%a(%a)@]" pr_aux p (print_list pr_aux ",") ts
    | _ -> pr_aux fm t
  in
  if constr = false_term
  then Format.fprintf fm "@[?- %a.@]" (print_list pr ",@ ") pre
  else Format.fprintf fm "@[%a :- %a.@]" pr constr (print_list pr ",@ ") pre
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
let rec inline_PApp typ ts =
  match typ with
  | Var _ -> PApp(typ, ts)
  | Arg _ -> PApp(typ, ts)
  | PApp(typ', ts') -> PApp(typ', ts'@ts)
  | Singleton _ -> typ
  | Base None -> typ
  | Base _ -> PApp(typ, ts)
  | Const _ -> typ
  | Fun(x, typ1, typ2) -> Fun(x, _PApp typ1 ts, _PApp typ2 ts)
  | Inter typs -> Inter (List.map (inline_PApp -$- ts) typs)
and _PApp typ ts =
  let dbg = 0=9 in
  let typ' = inline_PApp typ @@ List.filter (is_base_typ -| Syntax.typ) ts in
  if dbg then Format.printf "_PApp(%a, %a)" print_template typ (List.print Print.term) ts;
  if dbg then Format.printf " = %a@." print_template typ';
  match typ' with
  | PApp(typ'', []) -> typ''
  | _ -> typ'
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

let rec apply f = function
  | Exp t -> Exp t
  | And(c1,c2) -> And(apply f c1, apply f c2)
  | Imply(c1, c2) -> Imply(apply f c1, apply f c2)
  | Sub(typ1, typ2) -> Sub(f typ1, f typ2)
  | Pred(x,ts) -> Pred(x,ts)
let apply = ()

let rec from_ref_type typ =
  match typ with
  | Ref_type.Base(_, x, t) -> Const(x, t)
  | Ref_type.Fun(x, typ1, typ2) -> Fun(x, from_ref_type typ1, from_ref_type typ2)
  | Ref_type.Tuple _ -> assert false
  | Ref_type.Inter typs -> _Inter @@ List.map from_ref_type typs
  | Ref_type.Union _ -> assert false
  | Ref_type.ExtArg _ -> assert false
  | Ref_type.List _ -> assert false

let rec get_fv_typ = function
  | Var f -> [f]
  | Arg(typ, ts) -> get_fv_typ typ @ List.flatten_map Syntax.get_fv ts
  | PApp(typ, ts) -> get_fv_typ typ @ List.flatten_map Syntax.get_fv ts
  | Singleton t -> Syntax.get_fv t
  | Base _ -> []
  | Const _ -> []
  | Fun(x,typ1,typ2) -> List.filter_out (Id.eq x) @@ (get_fv_typ typ1 @ get_fv_typ typ2)
  | Inter typs -> List.flatten_map get_fv_typ typs
let rec get_fv_constr = function
  | Exp t -> Syntax.get_fv t
  | And(c1, c2) -> get_fv_constr c1 @ get_fv_constr c2
  | Imply _ -> assert false
  | Sub(typ1,typ2) -> get_fv_typ typ1 @ get_fv_typ typ2
  | Pred(x,ts) -> x :: List.flatten_map Syntax.get_fv ts
let get_fv_constr = ()

let rec constr_of_typ typ =
  match typ with
  | PApp(Base(Some(_,p1)), ts) ->
      let ts' = List.filter_out (Syntax.typ |- is_fun_typ) ts in
      if !!debug then Format.printf "      constr_of_typ: ts: %a@." (List.print Print.term) ts;
      Exp (make_app (make_var @@ make_pred_var p1 ts') ts')
  | PApp(Const(x, t), _) -> Exp t
  | Fun _
  | PApp(Fun _, _) -> Exp true_term
  | Inter [] -> Exp true_term
  | Inter typs -> (* TODO *)
      _Ands (List.map constr_of_typ typs)
  | _ ->
      Format.printf "  typ: %a@." print_template typ;
      assert false

let rec subst_template x t tmp =
  let sbst = subst_template x t in
  match tmp with
  | Var _ -> tmp
  | Arg(tmp', ts) -> Arg(sbst tmp', List.map (Term_util.subst x t) ts)
  | PApp(tmp', ts) -> PApp(sbst tmp', List.map (Term_util.subst x t) ts)
  | Singleton t' -> Singleton (Term_util.subst x t t')
  | Base _ -> tmp
  | Const _ -> tmp
  | Fun(x, tmp1, tmp2) -> Fun(x, sbst tmp1, sbst tmp2)
  | Inter tmps -> Inter (List.map sbst tmps)

let rec subst_constr x t constr =
  let sbst = subst_constr x t in
  match constr with
  | Exp t' -> Exp (Term_util.subst x t t')
  | And(c1, c2) -> _And (sbst c1) (sbst c2)
  | Imply(c1, c2) -> _Imply [sbst c1] (sbst c2)
  | Sub(tmp1, tmp2) -> Sub(subst_template x t tmp1, subst_template x t tmp2)
  | Pred _ -> assert false

let rec expand_type templates typ =
  let dbg = 0=10 in
  let pr f = if dbg then Format.printf @@ "        ET " ^^ f else Format.ifprintf Format.std_formatter f in
  let et = expand_type templates in
  let r =
  match typ with
  | Singleton _
  | Base _
  | Const _ -> typ
  | Var x ->
      pr "  TEMPLATE: %a@." print_tmp_env templates;
      templates
      |> List.filter (Id.eq x -| fst -| fst)
      |> List.map (fun ((x,nid),typ) -> pr "  VAR[%a]: typ: %a@." Id.print x print_template typ; nid,typ)
      |> List.sort
      |> List.hd
      |> snd
      |@> pr "  VAR[%a]: typ: %a@." Id.print x print_template
      |> et
  | PApp(Base None, _) -> Base None
  | PApp(Singleton t, _) -> Singleton t
  | PApp(typ, ts) ->
      begin
        match et typ with
        | PApp(typ', ts') -> PApp(typ', ts@ts')
        | Fun(x, typ1, typ2) -> Fun(x, et (PApp(typ1,ts)), et (PApp(typ2,ts)))
        | Inter typs -> Inter (List.map (fun typ' -> et @@ PApp(typ',ts)) typs)
        | typ' -> PApp(typ', ts)
      end
  | Arg(typ, ts) ->
      begin
        match et typ with
        | Fun(x, typ1, typ2) ->
            begin
              match ts with
              | [] -> typ1
              | t::ts' ->
                  let typ2' =
                    if is_fun_typ t.typ
                    then typ2
                    else PApp(typ2, [t])
                  in
                  let typ2' = typ2 in
                  et @@ subst_template x t @@ Arg(typ2', ts')
                  |@> pr "@[<hov 2>[%a |-> %a]%a = %a@]@." Id.print x Print.term t print_template (Arg(typ2', ts')) print_template
            end
        | Inter typs ->
            Inter (List.map (fun typ -> et @@ Arg(typ, ts)) typs)
        | typ' ->
            Format.printf "@.typ: %a@." print_template typ;
            Format.printf "typ': %a@.@." print_template typ';
            assert false
      end
  | Fun(x, typ1, typ2) -> Fun(x, et typ1, et typ2)
  | Inter typs -> Inter (List.map et typs)
  in
  if dbg then pr "typ: %a@." print_template typ;
  if dbg then pr "r: %a@." print_template r;
  r

let rec inline_sub templates typ1 typ2 =
  let _dbg = false in
  let r =
  match typ1,typ2 with
  | Inter typs1, Inter typs2 -> _Ands @@ List.map2 (inline_sub templates) typs1 typs2
  | Singleton t, _ -> constr_of_typ @@ _PApp typ2 [t]
  | Base None, Base None -> Exp true_term
  | PApp(Base(Some p1), ts1), PApp(Base(Some p2), ts2) ->
      _PApp typ2 [pred_var_term]
      |> constr_of_typ
      |> _Imply [constr_of_typ @@ _PApp typ1 [pred_var_term]]
  | Fun(_, (Fun _ as typ11), typ12), Fun(_, (Fun _ as typ21), typ22) ->
      _And (inline_sub templates typ21 typ11) (inline_sub templates typ12 typ22)
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) ->
      let c1 = subst_constr x1 (make_var x2) @@ inline_sub templates typ21 typ11 in
      let app typ =
        if is_fun_var x2
        then typ
        else expand_type [] @@ _PApp typ [make_var x2]
      in
      let c2 = constr_of_typ @@ app typ21 in
      let c3 = subst_constr x1 (make_var x2) @@ inline_sub templates (app typ12) (app typ22) in
      _And c1 @@ _Imply [c2] c3
  | _ ->
      Format.printf "  typ1: %a@." print_template typ1;
      Format.printf "  typ2: %a@." print_template typ2;
      assert false
  in
  if !!debug then Format.printf "    typ1: %a@." print_template typ1;
  if !!debug then Format.printf "    typ2: %a@." print_template typ2;
  if !!debug then Format.printf "    r: %a@.@." print_constr r;
  r
(*
let inline_sub templates typ1 typ2 =
  if !!debug then Format.printf "  typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  typ2: %a@." print_template typ2;
  inline_sub templates typ1 typ2
*)
let get_nid (Rose_tree.Node({CT.nid}, _)) = nid

let in_comp_tree ct nid =
  List.exists (fun {CT.nid=nid'} -> nid' = nid) @@ Rose_tree.flatten ct


let hd_of_inter typ =
  match typ with
  | Inter typs -> List.hd typs
  | _ -> typ

let make_sub_flag = ref true (* for debug *)

let rec decomp_tfun typ =
  match typ with
  | Var _ -> assert false
  | Fun(x,typ1,typ2) -> Pair.map_fst (List.cons (x,typ1)) @@ decomp_tfun typ2
  | Inter [typ'] -> decomp_tfun typ'
  | Inter _ ->
      Format.printf "decomp_tfun: %a@." print_template typ;
      assert false
  | _ -> [], typ

let rec copy_template cnt typ =
  match typ with
  | Var _ -> assert false
  | Arg _ -> assert false
  | PApp(typ, _) -> copy_template cnt typ
  | Singleton _ -> assert false
  | Base None -> Base None
  | Base (Some(base, p)) -> Base (Some (base, Counter.gen cnt))
  | Const _ -> typ
  | Fun(x,typ1,typ2) -> Fun(x, copy_template cnt  typ1, copy_template cnt typ2)
  | Inter typs -> Inter (List.map (copy_template cnt) typs)

let rec elim_papp typ =
  match typ with
  | PApp(typ, _) -> elim_papp typ
  | Fun(x,typ1,typ2) -> Fun(x, elim_papp typ1, elim_papp typ2)
  | Inter typs -> Inter (List.map elim_papp typs)
  | _ -> typ

let merge_template ts = List.flatten ts
let new_pred base cnt = Base (Some(base, Counter.gen cnt))

let rec decomp_fun typ =
  match typ with
  | Fun(x, typ1, typ2) -> Pair.map_fst (List.cons (x, typ1)) @@ decomp_fun typ2
  | _ -> [], typ

let base_of_typ typ =
  match typ with
  | TUnit -> Ref_type.Unit
  | TInt -> Ref_type.Int
  | TBool -> Ref_type.Bool
  | _ ->
      Format.printf "%a@." Print.typ typ;
      assert false

let rec init_with_pred_var cnt typ =
  let ip typ = init_with_pred_var cnt typ in
  match typ with
  | Var _ -> assert false
  | Arg _ -> assert false
  | PApp _ -> assert false
  | Singleton _ -> assert false
  | Base None -> Base None
  | Base (Some (base, _)) -> new_pred base cnt
  | Const(x, _) -> (try new_pred (base_of_typ @@ Id.typ x) cnt with _ -> Format.printf "%a@." print_template typ;  assert false)
  | Fun(x, typ1, typ2) -> Fun(x, ip typ1, ip typ2)
  | Inter typs -> _Inter @@ List.map ip typs

let make_sub templates typ1 typ2 =
  if !!debug then Format.printf "      make_sub: %a@." print_constr (Sub(typ1,typ2));
  let typ1' = expand_type templates typ1 in
  let typ2' = expand_type templates typ2 in
  if !!debug then Format.printf "      make_sub: %a@." print_constr (Sub(typ1',typ2'));
  let r = inline_sub templates typ1' typ2' in
  if !!debug then Format.printf "          ===>: %a@." print_constr r;
  r(*;
  Sub(typ1',typ2')*)

let filter_assumption val_env assumption =
  let vars = List.map fst val_env in
  assumption
  |> List.filter (fun t -> List.Set.subset (Syntax.get_fv t) vars)
  |> List.map (fun t -> Exp t)

let make_assumption templates val_env =
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf "  MA: Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
  let aux x =
    let cmp (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
    if dbg then Format.printf "    MA: x: %a@." Id.print x;(*
    val_env
    |> List.dropwhile (not -| Id.eq x -| fst)
    |> List.map fst
    |@> Format.printf "    MA: xs: %a@." (List.print Id.print)
    |> List.filter is_base_var
    |> List.map make_var
                                                *)
    [make_var x]
    |> _PApp @@ List.assoc ~cmp (x,None) templates
    |@dbg&> Format.printf "      MA: typ of %a: %a@." Id.print x print_template
    |> constr_of_typ
    |@dbg&> Format.printf "      MA: constr: %a@." print_constr
  in
  val_env
  |> List.map fst
  |@dbg&> Format.printf "  MA: vars: %a@." (List.print Id.print)
  |> List.filter is_base_var
  |@dbg&> Format.printf "  MA: base_vars: %a@." (List.print Id.print)
  |> List.map aux
  |@dbg&> Format.printf "  MA: base_vars_constr: %a@." (List.print print_constr)


let subst_template x t typ =
  let r = subst_template x t typ in
  if !!debug then Format.printf "ST: [%a |-> %a] %a = %a@." Id.print x Print.term t print_template typ print_template r;
  r

let _Imply typs = _Imply @@ List.rev typs (* for debug *)
let rec generate_constraints templates assumption (Rose_tree.Node({CT.nid; CT.var_env; CT.val_env; CT.label}, children) as ct) =
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf "label: %a@." Comp_tree.print_label label;
  if dbg then Format.printf "  Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
  let open Comp_tree in
  let r =
  let constr tmp asm = List.flatten_map (generate_constraints tmp asm) children in
  let templates' = List.filter (fst |- snd |- Option.for_all (in_comp_tree ct)) templates in
  match label with
  | App((f, _), map) ->
      if dbg then Format.printf "  map: %a@." (List.print @@ Pair.print Id.print Comp_tree.print_value) map;(*
      let arg_templates =
        let cmp (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
        let typ = List.assoc ~cmp (f,Some nid) templates' in
        let typs,_ = decomp_tfun typ in
        if dbg then Format.printf "  typ: %a@." print_template typ;
        let aux (x,vl) (_,typ) =
          let aux' y (_,typ') =
            Format.printf "  MAT: %a : %a@." Id.print y print_template typ';
            (y,None), typ'
          in
          let ys,_ = decomp_funs @@ term_of_value vl in
          let typs,_ = decomp_tfun typ in
          List.map2 aux' ys typs
        in
        let typs,_ = decomp_tfun typ in
        List.flatten @@ List.map2 aux map typs
        |> List.filter (fst |- fst |- is_base_var)
      in*)
      let asm =
        let var_env', val_env' =
          try
            let Closure(var_env, val_env, _) = Id.assoc f val_env in
            var_env, val_env
          with Not_found -> assert false; [], []
        in
        if dbg then Format.printf "  Dom(val_env'): %a@." (List.print Id.print) @@ List.map fst val_env';
        let asm1 = filter_assumption val_env assumption in
        let asm2 = make_assumption templates' val_env in
        if dbg then Format.printf "  asm1: %a@." (List.print print_constr) asm1;
        if dbg then Format.printf "  asm2: %a@." (List.print print_constr) asm2;
        asm1 @ asm2
      in
      let constr1 =
        let aux (env,acc) (_,vl) =
          let Closure(var_env,val_env, t) = vl in
          let constr =
            if is_base_typ t.typ then
              let typ1 = Singleton t in
              let typ2 = Arg(Var f, env) in
              if dbg then Format.printf "    Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
              make_sub templates' typ1 typ2
            else
              _PApp (Arg(Var f, env)) [t]
              |@dbg&> Format.printf "      ARG: typ: %a@." print_template
              |> expand_type templates'
              |@dbg&> Format.printf "      ARG: typ': %a@." print_template
              |> constr_of_typ
              |@dbg&> Format.printf "      ARG: constr: %a@." print_constr
          in
          if dbg then Format.printf "    nid: %d@." nid;
          if dbg then Format.printf "    constr: %a@." print_constr constr;
          let env' = env @ [t] in
          let acc' =
            if constr = Exp true_term then
              acc
            else
              _Imply asm constr :: acc
          in
          env', acc'
        in
        if dbg then Format.printf "    map: %a@." (List.print @@ Pair.print Id.print print_value) map;
        snd @@ List.fold_left aux ([],[]) map
      in
      if dbg then Format.printf "  constr1: %a@." (List.print print_constr) constr1;
      if dbg then Format.printf "@.";
      constr1 @ constr templates assumption
  | Let(f, t) ->
      constr templates assumption
  | Spawn(f, None) -> constr templates assumption
  | Spawn(f, Some tids) ->
(*      let constrs1 =
        if true
        then []
        else
          let asm1 = filter_assumption vars assumption in
          let asm2 = make_assumption templates vars var_env in
          let asm = asm1 @ asm2 in
          let app typ = PApp(typ, List.map make_var vars) in
          List.map (fun (g,tid) -> _Imply asm @@ make_sub ct templates (app @@ Var f) (app @@ Var g)) tids
      in
      constrs1 @*) constr templates assumption
  | Assume t ->
      constr templates @@ t::assumption
  | Value _ ->
      assert false
  | Bottom -> [Exp true_term]
  | Fail ->
      assert (children = []);
      let asm1 = filter_assumption val_env assumption in
      let asm2 = make_assumption templates val_env in
      let asm = asm1 @ asm2 in
      if dbg then Format.printf "  FAIL: asm1: %a@." (List.print print_constr) asm1;
      if dbg then Format.printf "  FAIL: asm2: %a@." (List.print print_constr) asm2;
      [_Imply asm @@ Exp false_term]
  in
  if dbg then Format.printf "  label: %a@." print_label label;
  if dbg then Format.printf "  assumption: %a@." (List.print Print.term) assumption;
  if dbg then Format.printf "  r: %a@.@." (List.print print_constr) r;
  r

let generate_constraints templates ct = generate_constraints templates [] ct



let rec make_template cnt env args (Rose_tree.Node({CT.nid; CT.var_env; CT.val_env; CT.label; CT.ref_typ}, children)) =
  let cmp (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
  let dbg = 0=0 && !!debug in
  let pr f = if dbg then Format.printf @@ "MT " ^^ f else Format.ifprintf Format.std_formatter f in
  let r=
  let templates = merge_template @@ List.map (make_template cnt env args) children in
  let open Comp_tree in
  match label with
  | App((f, _), map) when Option.is_some ref_typ ->
      ((f, Some nid), init_with_pred_var cnt @@ from_ref_type @@ Option.get ref_typ)::templates
  | App((f, kind), map) ->
      pr "APP: %a@." print_label label;
      pr "  TEMPLATE: %a@." print_tmp_env templates;
      pr "  Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
      let typ =
        let assoc_parg x =
          try
            term_of_value @@ Id.assoc x val_env
          with Not_found ->
            Format.printf "Not_found %a@." Id.print x;
            assert false
        in
        let aux' (pargs,k) (x,vl) =
          let Closure(_, _, t) = vl in
          pr "  pargs,t: %a, %a@." (List.print Print.term) pargs Print.term t;
          let tmp1 =
            if is_base_typ t.typ then
              new_pred (base_of_typ t.typ) cnt
            else
              templates
              |> List.filter (Id.eq x -| fst -| fst)
              |> List.map (Pair.map_fst fst)
              |> List.sort
              |> List.map snd
              |> List.map elim_papp
              |*@> Format.printf "LENGTH: %a@." (List.print print_template)
              |> _Inter
          in
          let tmp1' =
            if tmp1 = Inter [] then
              t.typ
              |> Ref_type.from_simple
              |> from_ref_type
              |> init_with_pred_var cnt
            else
              _PApp tmp1 pargs
          in
          pr "  [%a] tmp1: %a@." Id.print x print_template tmp1;
          pr "  [%a] tmp1': %a@." Id.print x print_template tmp1';
          make_var x :: pargs, fun tmp2 -> k @@ Fun(x, tmp1', tmp2)
        in
        let pargs' =
          pr "  f: %a@." Id.print f;
          pr "  var_env: %a@." (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env;
          pr "  Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
          Id.assoc f var_env
          |@> pr "  var_env(%a): %a@." Id.print f (List.print Id.print)
          |*> List.map assoc_parg
          |> List.map make_var
        in
        (*        let pargs' = [] in*)
        pr "  pargs': %a@." (List.print Print.term) pargs';
        _PApp ((snd @@ List.fold_left aux' ([], Fun.id) map) @@ Base None) pargs'
      in
      pr "  typ[%a]: %a@." Id.print f print_template typ;
      (*
      let arg_templates = []
        let typ'' =
          if assert false(*local*)
          then
            let cmp (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
            if dbg then Format.printf "f: %a@." Id.print f;
            List.assoc ~cmp (Id.assoc f fun_env) templates
          else typ
        in
        let xtyps,_ =  decomp_tfun typ'' in
        let aux (_,typ) (x,vl) =
          match vl with
          | Closure(_, t) ->
              let g = Option.get @@ decomp_var t in
              let typs = List.filter_map (fun ((h,_),typ'') -> if Id.eq x h then Some typ'' else None) templates in
              if dbg then Format.printf "  typ1 for %a: %a@." Id.print g print_template @@ _PApp (copy_template cnt @@ _Inter typs) (List.map make_var @@ Id.assoc g var_env);
              Some ((g, Some nid), _PApp (copy_template cnt @@ _Inter typs) (List.map make_var @@ Id.assoc g var_env))
          | BaseValue _ ->
              None
              (*(x, None), typ*)
        in
        List.filter_map2 aux xtyps map
      in
       *)
      let arg_templates =(*
        match kind with
        | CT.ArgFun ->
            assert (List.for_all (fun (x,_) -> is_base_var x) map); (* TODO *)
            []
        | _ ->*)
            let typs,_ = decomp_tfun typ in
            if dbg then Format.printf "  typs: %a@." (List.print print_template) @@ List.map snd typs;
            assert (List.length map = List.length typs);
            List.map2 (fun (x,_) (_,typ) -> ((x,None), typ)) map typs(*;
            let aux (x,vl) (_,typ) acc =
              let aux' y (_,typ') =
                Format.printf "  MAT: %a : %a@." Id.print y print_template typ';
                (y,None), typ'
              in
              let ys,_ = decomp_funs @@ term_of_value vl in
              let typs,_ = decomp_tfun typ in
              ((x,None), typ) :: List.map2 aux' ys typs @ acc
            in
            List.fold_right2 aux map typs []*)
            |> List.filter (fst |- fst |- is_base_var)
      in
      let arg_templates' =
        if dbg then Format.printf "  var_env(%a): %a@." Id.print f (List.print Id.print) @@ Id.assoc f var_env;
        let Closure(var_env_f,val_env_f,t_f) = Id.assoc f val_env in
        if dbg then Format.printf "  t_f[%a]: %a@." Id.print f Print.term t_f;
        if dbg then Format.printf "  Dom(var_env_f)[%a]: %a@." Id.print f (List.print Id.print) @@ List.map fst var_env_f;
        if dbg then Format.printf "  Dom(val_env_f)[%a]: %a@." Id.print f (List.print Id.print) @@ List.map fst val_env_f;
        let vars = Id.assoc f var_env in
        let Closure(var_env_f,_,_) = Id.assoc f val_env in
        let aux y typ =
          if Id.mem_assoc y var_env_f then
            typ
          else
            subst_template y (term_of_value @@ Id.assoc y val_env) typ
        in
        List.map (Pair.map_snd @@ List.fold_right aux vars) arg_templates
      in
      if dbg then Format.printf "  arg_templates[%a]: %a@." Id.print f print_tmp_env arg_templates;
      if dbg &&arg_templates<>arg_templates' then Format.printf "  arg_templates'[%a]: %a@." Id.print f print_tmp_env arg_templates';
      let arg_templates = arg_templates' in
      let templates' =
        let aux ((x,nid),typ) =
          let typ' =
            if Id.mem_assoc x map then
              List.fold_right (fun (y,vl) typ -> subst_template y (term_of_value vl) typ) map typ
            else
              typ
          in
          (x,nid), typ'
        in
        List.map aux templates
      in
      pr "  TEMPLATE: %a@." print_tmp_env templates;
      pr "  TEMPLATE': %a@." print_tmp_env templates';
      ((f,Some nid), typ) :: arg_templates @ templates
  | Let(f, t) ->
      assert (is_fun_var f);
      templates
  | Spawn(f, None) ->
      let typ = from_ref_type @@ Option.get ref_typ in
      ((f,Some nid),typ);
      templates
  | Spawn(f, Some gs) ->
      let nids = List.map get_nid children in
      pr "  TEMPLATES: @[%a@.@."  print_tmp_env templates;
      pr "  SPAWN: %a@.@." Id.print f;
      let typ = _Inter @@ List.map2 (fun g nid -> List.assoc ~cmp (g,Some nid) templates) gs nids in
      ((f,Some nid), typ)::templates
  | Assume _ -> templates
  | Value _ -> assert false
  | Bottom -> []
  | Fail -> []
  in
  pr "  %a@." Comp_tree.print_label label;
  pr "  LEN: %d@." (List.length r);
  r
(*
let rec make_arg_template cnt fun_env var_env templates (Rose_tree.Node({CT.nid; CT.val_env; CT.label}, children)) =
  let open Comp_tree in
  let arg_templates = List.flatten_map (make_arg_template cnt fun_env var_env templates) children in
  match label with
  | App((f,_), map) ->
      let cmp (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
      let typ' =
        if assert false(*local*) then
          let g,nid = (Id.assoc f fun_env) in
          Format.printf "MAT: g,nid: %a, %a@." Id.print g (Option.print Format.pp_print_int) nid;
          List.assoc ~cmp (g,nid) templates
        else
          List.assoc ~cmp (f, Some nid) templates
      in
      let xtyps,_ =  decomp_tfun typ' in
      let aux (_,typ) (x,t) =
        if is_fun_var x then
          None
        else
          Some ((x, None), typ)
      in
      List.filter_map2 aux xtyps map
  | Let _
  | Spawn _
  | Assume _
  | Fail -> arg_templates
  | Bottom -> arg_templates
  | Value _ -> assert false
 *)
let make_template env t =
  make_template (Counter.create()) env [] t


let rec make_var_env (Rose_tree.Node({CT.val_env; CT.label}, children)) =
  let open Comp_tree in
  let var_env = List.flatten_map make_var_env children in
  match label with
  | App(_, map) ->
      assert false(*
      snd @@ List.fold_left (fun (vs,env) (x,_) -> x::vs, (x,vs)::env) (vars', var_env) map*)
  | Let(f, t) ->
      assert false
      (*
      (f,vars) :: var_env*)
  | Spawn(_, gs) ->
      assert false(*
      List.map (Pair.add_right @@ Fun.const []) gs @ var_env*)
  | Assume t ->
      var_env
  | Value _ ->
      assert false
  | Fail ->
      var_env
  | Bottom ->
      var_env



let rec make_fun_env (Rose_tree.Node({CT.nid; CT.label}, children)) =
  let open Comp_tree in
  let fun_env = List.flatten_map make_fun_env children in
  match label with
  | App(_, map) ->
      let fun_env1 =
        let aux (x,vl) =
          let Closure(_,_,t) = vl in
          if is_fun_typ t.typ then
            Some (x, (Option.get @@ decomp_var t, Some nid))
          else
            None
        in
        List.filter_map aux map
      in
      let fun_env2 = List.map (fun (f,(g,i)) -> f, (try Id.assoc g fun_env1 with Not_found -> g, i)) fun_env in
      fun_env1 @ fun_env2
  | Let _
  | Spawn _
  | Assume _
  | Fail -> fun_env
  | Value _ -> assert false



let elim_not t =
  match t.desc with
  | Not {desc=BinOp(Leq, t1, t2)} -> make_gt t1 t2
  | _ -> t

let rec to_horn_clause asm constr =
  match constr with
  | Exp t -> [asm, elim_not t]
  | And(c1, c2) -> to_horn_clause asm c1 @ to_horn_clause asm c2
  | Imply(c1, c2) ->
      let t =
        match c1 with
        | Exp t -> elim_not t
        | Pred(p, ts) -> assert false
        | _ -> assert false
      in
      to_horn_clause (asm@[t]) c2
  | Sub _ -> assert false
  | Pred _ -> assert false
let to_horn_clause constr = to_horn_clause [] constr

let check_arity hc =
  let ts = List.flatten_map (Fun.uncurry List.snoc) hc in
  let aux env t =
    match t.desc with
    | App({desc=Var f}, ts) ->
        let n = List.length ts in
        begin
          try
            if Id.assoc f env <> List.length ts then
              (Format.printf "%a@." Id.print f; assert false);
            env
          with Not_found -> (f,n)::env
        end
    | _ -> env
  in
  ignore @@ List.fold_left aux [] ts


let rec pvars t =
  match t.desc with
  | App({desc=Var p}, _) -> [p]
  | BinOp(_, t1, t2) -> pvars t1 @ pvars t2
  | _ -> []

let print_solution fm (p,(xs,t)) =
  Format.fprintf fm "P_%d(%a) := %a" p (print_list Id.print ",") xs Print.term t

let solve hc =
  let vars =
    hc
    |> List.flatten_map (fun (body,head) -> List.flatten_map pvars body @ pvars head)
    |> List.unique ~cmp:Id.eq
  in
  let sol =
    let tr t =
      t
      |> FpatInterface.of_typed_term
      |> Fpat.Formula.of_term
    in
    hc
    |> List.map @@ Pair.map (List.map tr) tr
    |> List.map @@ Pair.swap
    |> List.map (Fun.uncurry Fpat.HornClause.of_formulas)
    |> Fpat.HCCSSolver.solve_dyn
    |> Fpat.PredSubst.normalize
  in
  if !!debug then Format.printf "SOLUTION: %a@." Fpat.PredSubst.pr sol;
  let sol' =
    let tr_typ typ =
      if typ = Fpat.Type.mk_int then
        Type.TInt
      else if typ = Fpat.Type.mk_bool then
        Type.TBool
      else if typ = Fpat.Type.mk_unit then
        Type.TUnit
      else
        (Format.printf "%a@." Fpat.Type.pr typ;
         assert false)
    in
    let aux p =
      let var_of_env (x,typ) = Id.from_string (Fpat.Idnt.string_of x) (tr_typ typ) in
      let sbst xtyp y = subst_var (var_of_env xtyp) y in
      let sol' = List.assoc_all (Fpat.Idnt.make @@ Id.to_string p) sol in
      let xs = List.map var_of_env @@ fst @@ List.hd sol' in
      let tr = CEGAR_trans.trans_inv_term -| FpatInterface.inv_term -| Fpat.Formula.term_of  in
      xs, make_ors @@ List.map (fun (tenv,phi) -> List.fold_right2 sbst tenv xs @@ tr phi) sol'
    in
    List.map (Pair.make Id.id aux) vars
  in
  if !!debug then Format.printf "SOLUTION: %a@." (List.print print_solution) sol';
  sol'




let rec apply_sol sol x vars tmp =
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf "AS tmp: %a@." print_template tmp;
  match tmp with
  | Base(Some _)
  | PApp(Base (Some _), _) ->
      if x = None then
        Ref_type.Base(Ref_type.Unit, Id.new_var TUnit, true_term)
      else
        let base,p,ts =
          match tmp with
          | Base(Some(base,p)) -> base, p, []
          | PApp(Base (Some (base,p)), ts) -> base, p, [](*ts*)
          | _ -> assert false
        in
        let x' = Option.get x in
        let p =
          let ts' = List.rev @@ make_var x' :: List.map make_var vars @ ts in
          try
            let xs,t = List.assoc p sol in
            if dbg then Format.printf "xs: %a@." (List.print Id.print) xs;
            if dbg then Format.printf "ts': %a@." (List.print Print.term) ts';
            List.fold_right2 subst xs ts' t
          with Not_found -> false_term
        in
        Ref_type.Base(base, x', p)
  | Base None -> Ref_type.Base(Ref_type.Unit, Id.new_var TUnit, true_term)
  | Fun(y,typ1,typ2) -> Ref_type.Fun(y, apply_sol sol (Some y) vars typ1, apply_sol sol None (y::vars) typ2)
  | Inter tmps -> Ref_type.Inter (List.map (apply_sol sol x vars) tmps)
  | _ ->
      Format.eprintf "%a@." print_template tmp;
      assert false
let apply_sol sol tmp = apply_sol sol None [] tmp




(*
let eta = make_trans ()
let eta_desc desc =
  match eta.tr_desc_rec desc with
  | Let(flag, [f, xs, t1], t2) ->
      let t1' =
        let ys,ys',bindings =
          let aux x =
            let x' = Id.new_var_id x in
            let zs,_ = Type.decomp_tfun @@ Id.typ x in
            let zs' = List.map Id.new_var_id zs in
            x, x', (x', zs', make_app (make_var x) @@ List.map make_var zs)
          in
          List.split3 @@ List.filter_map (Option.make is_fun_var aux) xs
        in
        make_lets bindings @@ List.fold_right2 subst_var ys ys' t1
      in
      Let(flag, [f, xs, t1'], t2)
  | desc' -> desc'
let () = eta.tr_desc <- eta_desc
let eta = eta.tr_term
 *)




let normalize = Trans.reduce_fail_unit |- Trans.reconstruct

let trans_CPS env t =
  let fs = List.map fst env in
  let t',make_get_rtyp =
    t
    |> make_letrecs @@ List.map Triple.of_pair_r env
    |@!!debug&> Format.printf "trans_CPS: %a@." Print.term
    |> CPS.trans_as_direct
    |> Pair.map_fst normalize
  in
  if !!debug then Format.printf "trans_CPS t': %a@." Print.term t';
  let env',t'' = decomp_prog t' in
  if !!debug then Format.printf "trans_CPS t'': %a@." Print.term t'';
  let env1,env2 =
    env'
    |> List.flatten_map (snd |- List.map Triple.to_pair_r)
    |> List.partition (fst |- Id.mem -$- fs)
  in
  if !!debug then Format.printf "fs: %a@." (List.print Id.print) fs;
  if !!debug then Format.printf "env1: %a@." (List.print @@ Pair.print Id.print @@ Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) env1;
  if !!debug then Format.printf "env2: %a@." (List.print @@ Pair.print Id.print @@ Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) env2;
  env1, make_letrecs (List.map Triple.of_pair_r env2) t'', make_get_rtyp

let replace_if_with_bottom = make_trans ()
let replace_if_with_bottom_term t =
  match t.desc with
  | If _ -> make_bottom t.typ
  | _ -> replace_if_with_bottom.tr_term_rec t
let () = replace_if_with_bottom.tr_term <- replace_if_with_bottom_term
let replace_if_with_bottom = replace_if_with_bottom.tr_term

let add_context for_infer prog f typ =
  let dbg = 0=10 in
  if dbg then Format.printf "ADD_CONTEXT: %a :? %a@." Print.id f Ref_type.print typ;
  let t' = Trans.ref_to_assert (Ref_type.Env.of_list [f,typ]) unit_term in
  if dbg then Format.printf "ADD_CONTEXT t': %a@." Print.term t';
(*
  let env' = List.filter (fst |- Id.same f) prog.fun_def_env in
 *)
  let env' = prog.fun_def_env in
  trans_CPS env' t'

let infer prog f typ ce_set =
  let ce_set =
    if 0=1 then
      List.filter (fun (x,ce) -> Format.printf "%a, %a@.?: @?" Id.print x (List.print Format.pp_print_int) ce; read_int() <> 0) ce_set
    else
      ce_set
  in
  let {fun_typ_env=env; fun_def_env=fun_env} = prog in
  let fun_env',t,make_get_rtyp = add_context true prog f typ in
  if !!debug then Format.printf "INFER prog: %a@." print_prog prog;
  if !!debug then Format.printf "t: %a@.@." Print.term t;
  let comp_tree =
    let env' =
      env
      |> Ref_type.Env.filter_key_out (Id.same f)
(*
      |*> List.map (Pair.map_snd @@ init_with_pred_var cnt -| from_ref_type)
 *)
      |> Ref_type.Env.map_value CPS.trans_ref_typ_as_direct
    in
    if !!debug then Format.printf "Dom(Fun_env'): %a@.@." (List.print Id.print) @@ List.map fst fun_env';
    if !!debug then Format.printf "t with def: %a@.@." Print.term @@ make_letrecs (List.map Triple.of_pair_r fun_env') t;
    if !!debug then Format.printf "t: %a@.@." Print.term t;
    if !!debug then Format.printf "env': %a@.@." Ref_type.Env.print env';
    Comp_tree.from_program env' fun_env' ce_set t
  in
  let fun_env = [](*make_fun_env comp_tree*) in
  if !!debug then Format.printf "fun_env: %a@.@." (List.print @@ Pair.print Id.print @@ Pair.print Id.print @@ Option.print Format.pp_print_int) fun_env;
  let templates = make_template env comp_tree in
  if !!debug then Format.printf "TEMPLATES: @[%a@.@." print_tmp_env templates;
  let constrs = generate_constraints templates comp_tree in
  if !!debug then Format.printf "CONSTR: @[%a@.@." (List.print print_constr) constrs;
  let hc = List.flatten_map to_horn_clause constrs in
  let hc = List.map (Pair.map (List.map Trans.init_rand_int) Trans.init_rand_int) hc in
  if !!debug then Format.printf "HORN CLAUSES: @[%a@.@." print_horn_clauses hc;
(*
  if false then Format.fprintf (Format.formatter_of_out_channel @@ open_out "test.hcs") "%a@." print_horn_clauses hc;
 *)
  check_arity hc;
  (*
  let constr = inline_template templates constr in
  Format.printf "CONSTR2: @[%a@.@." print_constr constr;
  let pred_map = make_pred_map constr in
  (*  Format.printf "PRED_MAP: @[%a@.@." (List.print @@ Pair.print Format.pp_print_int Id.print) pred_map;*)
  let constr = decomp_constraints pred_map constr in
  Format.printf "CONSTR3: @[%a@.@." print_constr constr;
  let constr = flatten constr in
  Format.printf "CONSTR4: @[%a@.@." print_horn_clauses constr;*)
  match Option.try_any (fun _ -> solve hc) with
  | None -> None
  | Some sol ->
      let top_funs = List.filter_out (Id.same f) @@ Ref_type.Env.dom env in
      if !!debug then Format.printf "TOP_FUNS: %a@.@." (List.print Id.print) top_funs;
      let env' = List.filter_map (fun ((f,_),tmp) -> if Id.mem f top_funs then Some (f, apply_sol sol tmp) else None) templates in
      let env'' =
        let aux (x,typ') =
          let typ = Ref_type.Env.assoc x prog.fun_typ_env in
          let x',_ = Ref_type.Env.find (fst |- Id.same x) env in
          if !!debug then Format.printf "%a: %a@." Id.print x' Ref_type.print typ';
          let typ_ =
            if !!debug then Format.printf "typ: %a@." Ref_type.print typ;
            if !!debug then Format.printf "typ': %a@." Ref_type.print typ';
            make_get_rtyp (fun y -> assert (Id.same y x); typ') x
          in
          if !!debug then Format.printf "typ_: %a@." Ref_type.print typ_;(*
          let typ'' = CPS.uncps_ref_type typ' typ in*)
          x', typ_
        in
        List.map aux env'
        |*> List.flatten_map (fun (x,typ) -> List.map (fun typ -> x, typ) @@ Ref_type.decomp_inter typ)
        |> Ref_type.Env.of_list
      in
      if !!debug then Format.printf "Infer_mod.infer: %a@.@." Ref_type.Env.print env'';
      Some env''
