open Util
open Syntax
open Term_util
open Type
open Modular_syntax
open Horn_clause

let debug () = List.mem "Infer_mod" !Flag.debug_module

module CT = Comp_tree


let infer_stronger = ref false


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
  | Inter of typ * type_template list


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
  | Inter(_, []) -> Format.fprintf fm "T"
  | Inter(_, tmps) -> Format.fprintf fm "(@[%a@])" (print_list print_template " /\\@ ") tmps

let print_tmp_env fm env =
  let print_idx fm (x,nid) =
    match nid with
    | None -> Format.fprintf fm "%a" Id.print x
    | Some nid' -> Format.fprintf fm "%a@%d" Id.print x nid'
  in
  Format.fprintf fm "%a" (List.print @@ Pair.print print_idx print_template) env


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
  | Inter(styp, typs) -> Inter(styp, List.map (inline_PApp -$- ts) typs)
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
let _Inter styp tmps =
  match tmps with
  | [tmp] -> tmp
  | _ -> Inter(styp, tmps)

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
  | Ref_type.Inter(styp, typs) -> _Inter styp @@ List.map from_ref_type typs
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
  | Inter(_, typs) -> List.flatten_map get_fv_typ typs
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
  | Inter(_, typs) ->
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
  | Inter(typ, tmps) -> Inter(typ, List.map sbst tmps)

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
        | Fun(x, typ1, typ2) -> Fun(x, et (PApp(typ1,ts)), et (PApp(typ2,ts)))
        | Inter(typ, typs) -> Inter(typ, List.map (fun typ' -> et @@ PApp(typ',ts)) typs)
        | typ' -> _PApp typ' ts
      end
  | Arg(typ, ts) ->
      begin
        match et typ with
        | Fun(x, typ1, typ2) ->
            begin
              match ts with
              | [] -> typ1
              | t::ts' ->
(*
                  let typ2' =
                    if is_fun_typ t.typ
                    then typ2
                    else PApp(typ2, [t])
                  in
 *)
                  let typ2' = typ2 in
                  et @@ subst_template x t @@ Arg(typ2', ts')
                  |@> pr "@[<hov 2>[%a |-> %a]%a = %a@]@." Id.print x Print.term t print_template (Arg(typ2', ts')) print_template
            end
        | Inter(styp, typs) ->
            Inter(styp, List.map (fun typ -> et @@ Arg(typ, ts)) typs)
        | typ' ->
            Format.printf "@.typ: %a@." print_template typ;
            Format.printf "typ': %a@.@." print_template typ';
            assert false
      end
  | Fun(x, typ1, typ2) -> Fun(x, et typ1, et typ2)
  | Inter(styp, typs) -> Inter(styp, List.map et typs)
  in
  if dbg then pr "typ: %a@." print_template typ;
  if dbg then pr "r: %a@." print_template r;
  r

let _PAppSelf typ t =
  let typ',ts =
    match typ with
    | Base _ -> typ, []
    | PApp(Base(Some _) as typ', ts) -> typ', ts
    | _ -> assert false
  in
  _PApp typ' (t::ts)

let rec inline_sub templates typ1 typ2 =
  let _dbg = false in
  let r =
  match typ1,typ2 with
  | Inter(_,typs1), Inter(_,typs2) -> _Ands @@ List.map2 (inline_sub templates) typs1 typs2
  | Singleton t, _ -> constr_of_typ @@ _PAppSelf typ2 t
  | Base None, Base None -> Exp true_term
  | PApp(Base(Some p1) as typ1', ts1), PApp(Base(Some p2) as typ2', ts2) ->
      _PApp typ2' (pred_var_term::ts2)
      |> constr_of_typ
      |> _Imply [constr_of_typ @@ _PApp typ1' (pred_var_term::ts1)]
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
  | Inter(_, typs) -> List.hd typs
  | _ -> typ

let make_sub_flag = ref true (* for debug *)

let rec decomp_inter typ =
  match typ with
  | Inter(styp, typs) -> List.flatten_map decomp_inter typs
  | _ -> [typ]

let rec decomp_tfun typ =
  match typ with
  | Var _ -> assert false
  | Fun(x,typ1,typ2) -> Pair.map_fst (List.cons (x,typ1)) @@ decomp_tfun typ2
  | Inter(_, [typ']) -> decomp_tfun typ'
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
  | Inter(styp, typs) -> Inter(styp, List.map (copy_template cnt) typs)

let rec elim_papp typ =
  match typ with
  | PApp(typ, _) -> elim_papp typ
  | Fun(x,typ1,typ2) -> Fun(x, elim_papp typ1, elim_papp typ2)
  | Inter(styp, typs) -> Inter(styp, List.map elim_papp typs)
  | _ -> typ

let merge_template ts = List.flatten ts
let new_pred base cnt = Base (Some(base, Counter.gen cnt))

let rec decomp_fun typ =
  match typ with
  | Fun(x, typ1, typ2) -> Pair.map_fst (List.cons (x, typ1)) @@ decomp_fun typ2
  | _ -> [], typ

let base_of_typ typ =
  match elim_tpred typ with
  | TUnit -> Ref_type.Unit
  | TInt -> Ref_type.Int
  | TBool -> Ref_type.Bool
  | TData s -> Ref_type.Abst s
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
  | Const(x, _) -> new_pred (base_of_typ @@ Id.typ x) cnt
  | Fun(x, typ1, typ2) -> Fun(x, ip typ1, ip typ2)
  | Inter(styp, typs) -> _Inter styp @@ List.map ip typs

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
  let dbg = 0=1 && !!debug in
  if dbg then Format.printf "  MA: Dom(val_env): %a@." (List.print Id.print) @@ List.map fst val_env;
  let aux x =
    try
      let eq (x1,id1) (x2,id2) = Id.eq x1 x2 && id1 = id2 in
      if dbg then Format.printf "    MA: x: %a@." Id.print x;(*
      val_env
      |> List.dropwhile (not -| Id.eq x -| fst)
      |> List.map fst
      |@> Format.printf "    MA: xs: %a@." (List.print Id.print)
      |> List.filter is_base_var
      |> List.map make_var
                                                  *)
      make_var x
      |> _PAppSelf @@ List.assoc ~eq (x,None) templates
      |@dbg&> Format.printf "      MA: typ of %a: %a@." Id.print x print_template
      |> constr_of_typ
      |@dbg&> Format.printf "      MA: constr: %a@." print_constr
      |> Option.some
    with Not_found -> None
  in
  val_env
  |> List.map fst
  |@dbg&> Format.printf "  MA: vars: %a@." (List.print Id.print)
  |> List.filter is_base_var
  |@dbg&> Format.printf "  MA: base_vars: %a@." (List.print Id.print)
  |> List.filter_map aux
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
          with Not_found -> assert false
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
              |> _Inter t.typ
          in
          let tmp1' =
            match tmp1 with
            | Inter(styp, []) ->
                if false then
                  Inter(styp, [])
                else
                  styp
                  |> Ref_type.of_simple
                  |> from_ref_type
                  |> init_with_pred_var cnt
            | _ ->
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
(*
      let typ = from_ref_type @@ Option.get ref_typ in
      ((f,Some nid),typ);
 *)
      templates
  | Spawn(f, Some gs) ->
      let nids = List.map get_nid children in
      pr "  TEMPLATES: @[%a@.@."  print_tmp_env templates;
      pr "  SPAWN: %a@.@." Id.print f;
      pr "  NIDS: %a@.@." (List.print Format.pp_print_int) nids;
      let typ = _Inter (Id.typ f) @@ List.flatten_map (fun g -> List.map snd @@ List.filter (fst |- fst |- Id.eq g) templates) gs in
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


(*
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
 *)


let elim_not t =
  match t.desc with
  | Not {desc=BinOp(Leq, t1, t2)} -> make_gt t1 t2
  | _ -> t

let map_horn_clause f (body, head) = List.map f body, f head

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

let check_arity hcs =
  let ts = List.flatten_map (Fun.uncurry List.snoc) hcs in
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

let solve hcs =
  let dbg = 0=1 && !!debug in
  let vars =
    hcs
    |> List.flatten_map (fun (body,head) -> List.flatten_map pvars body @ pvars head)
    |> List.unique ~cmp:Id.eq
    |@dbg&> Format.printf "vars: %a@." (List.print Id.print)
  in
  let sol =
    let tr t =
      t
      |> FpatInterface.of_typed_term
      |> Fpat.Formula.of_term
    in
    hcs
    |> List.map @@ Pair.map (List.map tr) tr
    |> List.map @@ Pair.swap
    |> List.map (Fun.uncurry Fpat.HornClause.of_formulas)
    |> Fpat.HCCSSolver.solve_dyn
    |> Fpat.PredSubst.normalize
  in
  if !!debug then Format.printf "SOLUTION: %a@." Fpat.PredSubst.pr sol;
  let sol' =
    let tr_typ typ =
      match typ with
      | _ when typ = Fpat.Type.mk_int -> Type.TInt
      | _ when typ = Fpat.Type.mk_bool -> Type.TBool
      | _ when typ = Fpat.Type.mk_unit -> Type.TUnit
      | _ ->
          Format.printf "%a@." Fpat.Type.pr typ;
          assert false
    in
    let aux p =
      if !!debug then Format.printf "p: %a@." Print.id_typ p;
      let var_of_env (x,typ) = Id.from_string (Fpat.Idnt.string_of x) (tr_typ typ) in
      let sbst xtyp y = subst_var (var_of_env xtyp) y in
      let sol' = List.assoc_all (Fpat.Idnt.make @@ Id.to_string p) sol in
      match sol' with
      | [] -> fst @@ Type.decomp_tfun @@ Id.typ p, false_term
      | (env,_)::_ ->
          let xs = List.map var_of_env env in
          let tr = CEGAR_trans.trans_inv_term -| FpatInterface.inv_term -| Fpat.Formula.term_of  in
          xs, make_ors @@ List.map (fun (tenv,phi) -> List.fold_right2 sbst tenv xs @@ tr phi) sol'
    in
    List.map (Pair.make Id.id aux) vars
  in
  if !!debug then Format.printf "SOLUTION: %a@." (List.print print_solution) sol';
  sol'
let solve_option hcs =
  try Some (solve hcs) with
  | Fpat.HCCSSolver.Unknown -> None



let rec apply_sol sol x vars pos tmp =
  let dbg = 0=0 && !!debug in
  let r =
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
        if dbg then Format.printf "  P_%d@." p;
        if dbg then Format.printf "  Dom(sol): %a@." (List.print Format.pp_print_int) @@ List.map fst sol;
        let x' = Option.get x in
        let p =
          let vars' = List.filter is_base_var vars in
          let ts' = make_var x' :: List.map make_var vars' @ ts in
          try
            let xs,t = List.assoc p sol in
            if dbg then Format.printf "  P_%d@." p;
            if dbg then Format.printf "  t: %a@." Print.term t;
            if dbg then Format.printf "  xs: %a@." (List.print Id.print) xs;
            if dbg then Format.printf "  ts': %a@." (List.print Print.term) ts';
            List.fold_right2 subst xs ts' t
          with Not_found ->
               if pos && !infer_stronger then
                 false_term
               else
                 true_term
        in
        Ref_type.Base(base, x', p)
  | Base None -> Ref_type.Base(Ref_type.Unit, Id.new_var TUnit, true_term)
  | Fun(y,typ1,typ2) ->
      Ref_type.Fun(y, apply_sol sol (Some y) vars (not pos) typ1, apply_sol sol None (y::vars) pos typ2)
  | Inter(styp, []) ->
      let r =
      if pos then
        Ref_type.make_strongest styp
      else
        Ref_type.make_weakest styp
      in
      Format.printf "  AS TOP: %a ==> %a@." Print.typ styp Ref_type.print r;
      if !infer_stronger then r else Ref_type.of_simple styp
  | Inter(styp, tmps) -> Ref_type.Inter(styp, List.map (apply_sol sol x vars pos) tmps)
  | _ ->
      Format.eprintf "%a@." print_template tmp;
      assert false
  in
  if dbg then Format.printf "AS tmp: %a@." print_template tmp;
  if dbg then Format.printf "AS r: %a@." Ref_type.print r;
  r
let apply_sol sol pos tmp = apply_sol sol None [] pos tmp




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

let trans_CPS env funs t =
  let t',make_get_rtyp_cps =
    t
    |> List.fold_right (fun (f,(xs,t1)) t -> add_attr ADoNotInline @@ make_letrec [f,xs,t1] t) env
    |@!!debug&> Format.printf "trans_CPS: %a@." Print.term
    |> CPS.trans_as_direct
  in
  if !!debug then Format.printf "trans_CPS t': %a@." Print.term t';
  Type_check.check t' TUnit;
  let t'',make_get_rtyp_pair =
    Curry.remove_pair_direct t'
  in
  if !!debug then Format.printf "trans_CPS t'': %a@." Print.term t'';
  let env',t_main =
    t''
    |@!!debug&> Format.printf "trans_CPS INPUT: %a@." Print.term
    |> Trans.reduce_fail_unit
    |@!!debug&> Format.printf "trans_CPS reduce_fail_unit: %a@." Print.term
    |> Trans.reconstruct
    |@!!debug&> Format.printf "trans_CPS normalized: %a@." Print.term
    |> decomp_prog
  in
  let make_get_rtyp = make_get_rtyp_cps -| make_get_rtyp_pair in
  let env1,env2 =
    let fs = List.map fst env in
    env'
    |> List.flatten_map (snd |- List.map Triple.to_pair_r)
    |> List.partition (fst |- Id.mem -$- fs)
  in
  if !!debug then Format.printf "funs: %a@." (List.print Id.print) funs;
  if !!debug then Format.printf "env1: %a@." (List.print @@ Pair.print Id.print @@ Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) env1;
  if !!debug then Format.printf "env2: %a@." (List.print @@ Pair.print Id.print @@ Print.term) @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) env2;
  if not @@ List.for_all (Id.mem_assoc -$- env1) funs then
    (let removed = List.filter_out (Id.mem_assoc -$- env1) funs in
     Format.printf "REMOVED: %a@." (List.print Id.print) removed;
     assert false);
  env1, make_letrecs (List.map Triple.of_pair_r env2) t_main, make_get_rtyp

let replace_if_with_bottom = make_trans ()
let replace_if_with_bottom_term t =
  match t.desc with
  | If _ -> make_bottom t.typ
  | _ -> replace_if_with_bottom.tr_term_rec t
let () = replace_if_with_bottom.tr_term <- replace_if_with_bottom_term
let replace_if_with_bottom = replace_if_with_bottom.tr_term

let add_context for_infer prog f typ =
  let dbg = 0=0 && !!debug in
  if dbg then Format.printf "ADD_CONTEXT: %a :? %a@." Print.id f Ref_type.print typ;
  let t' = Trans.ref_to_assert (Ref_type.Env.of_list [f,typ]) unit_term in
  if dbg then Format.printf "ADD_CONTEXT t': %a@." Print.term t';
(*
  let env' = List.filter (fst |- Id.same f) prog.fun_def_env in
 *)
  let env = prog.fun_def_env in
  let funs =
    env
    |> List.map fst
    |> List.takewhile (not -| Id.same f)
  in
  trans_CPS env funs t'

module Dependency =
  Set.Make(
    struct
      type t = int * int
      let compare = compare
    end)

let add_dependency deps (x,y) =
  if 0=1 && !!debug then Format.printf "ADD_DEP: %d -> %d@." x y;
  let deps' = Dependency.elements deps in
  let from_y = y :: List.filter_map (fun (z,w) -> if y = z then Some w else None) deps' in
  let to_x   = x :: List.filter_map (fun (z,w) -> if x = w then Some z else None) deps' in
  let new_dep = Dependency.of_list @@ List.map Pair.of_list @@ Combination.take_each [to_x; from_y] in
  Dependency.union new_dep deps

let get_pred_ids_hcs hcs =
  hcs
  |> List.flatten_map (fun (body,head) -> (Option.to_list @@ get_pred_id_of_term head) @ List.filter_map get_pred_id_of_term body)
  |> List.unique

let get_dependencies hcs =
  let dbg = 0=1 && !!debug in
  let aux acc (body,head) =
    if dbg then Format.printf "  HC: %a@." print_horn_clause (body,head);
    if dbg then Format.printf "  deps_cls: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements acc;
    let fv1 = List.filter_map get_pred_id_of_term body in
    let fv2 = Option.to_list @@ get_pred_id_of_term head in
    let new_dep = List.flatten_map (fun x -> List.map (Pair.pair x) fv2) fv1 in
    List.fold_left add_dependency acc new_dep
  in
  List.fold_left aux Dependency.empty hcs

let transitive_closure deps =
  List.fold_left add_dependency Dependency.empty deps

let save_dep deps_cls hcs filename =
  let dbg = 0=1 in
  let aux acc (body,head) =
    let fv1 = List.filter_map get_pred_id_of_term body in
    let fv2 = Option.to_list @@ get_pred_id_of_term head in
    let new_dep = List.flatten_map (fun x -> List.map (Pair.pair x) fv2) fv1 in
    let aux' acc (x,y) = new_dep @@@ acc in
    List.fold_left aux' acc new_dep
  in
  let deps = List.unique @@ List.fold_left aux [] hcs in
  let deps_cls' = transitive_closure deps in
  let vertices = List.map (fun x -> string_of_int x, "") @@ List.unique @@ List.flatten_map Pair.to_list deps in
  let edges = List.map (fun (x,y) -> string_of_int x, string_of_int y, "") deps in
  Format.printf "Save %s@." filename;
  save_as_dot filename vertices edges;
  if dbg then Format.printf "deps_cls: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements deps_cls;
  if dbg then Format.printf "deps_cls': %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements deps_cls';
  if dbg then Format.printf "deps_cls'\\deps_cls: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements @@ Dependency.diff deps_cls' deps_cls;
  assert (Dependency.subset deps_cls' deps_cls)


let merge_predicate_variables candidates hcs =
(*
  let rec get_map candidates dependencies =
    match candidates with
    | [] -> []
    | (p1,p2)::candidates' ->
        if List.mem (p1,p2) dependencies then
          get_map candidates' dependencies
        else
          let dependencies' =
            let sbst p = if p = p1 then p2 else p in
            dependencies
            |> List.map @@ Pair.map sbst sbst
            |> List.unique
          in
          (p1,p2) :: get_map candidates' dependencies'
  in
  if !!debug then save_dep hcs "tmp/test.dot";
*)
  let dependencies = get_dependencies hcs in
(*
  let map =
    dependencies
    |@!!debug&> (fun _ -> Format.printf "MGV1@.")
    |*> transitive_closure
    |@!!debug&> (fun _ -> Format.printf "MGV2@.")
    |*> get_map candidates
  in
 *)
  dependencies, candidates

let subst_horn_clause x t (body,head) =
  List.map (subst x t) body, subst x t head


let replace_id p1 p2 t =
  match t.desc with
  | App({desc=Var x}, ts) when get_pred_id x = p1 ->
      assert (is_pred_var x);
      make_app (make_var @@ Id.set_id x p2) ts
  | _ -> t


let cnt = ref 0

let same_last_sol last_sol p1 p2 =
  try
    let xs1,t1 = List.assoc p1 last_sol in
    let xs2,t2 = List.assoc p2 last_sol in
    t2 = List.fold_right2 subst_var xs1 xs2 t1
  with Not_found -> false

let add_merged (p1,p2) merged =
  if List.mem_assoc p1 merged then
    (p2, List.assoc p1 merged)::merged
  else if List.mem_assoc p2 merged then
    (p1, List.assoc p2 merged)::merged
  else
    (p1,p2)::merged

let solve_merged merge_candidates hcs =
  let dependencies,map = merge_predicate_variables merge_candidates hcs in
  let sbst (p1,p2) map =
    let aux p = if p = p1 then p2 else p in
    List.map (Pair.map aux aux) map
  in
  let rec aux used last_sol merged deps map hcs =
    if !!debug then Format.printf "merged: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) merged;
    match map with
    | [] -> last_sol, merged
    | (p1,p2)::map' when p1 = p2 ->
        aux used last_sol merged deps map' hcs
    | (p1,p2)::map' when Dependency.mem (p1,p2) deps || Dependency.mem (p2,p1) deps ->
        if !!debug then Format.printf "NOT MERGE %d, %d@." p1 p2;
        aux used last_sol merged deps map' hcs
    | (p1,p2)::map' when not (List.mem p1 used && List.mem p2 used) ->
        if !!debug then Format.printf "MERGE1 %d, %d@." p1 p2;
        let merged' = add_merged (p1,p2) merged in
        let map'' = sbst (p1,p2) map' in
        aux used last_sol merged' deps map'' hcs
    | (p1,p2)::map' when same_last_sol last_sol p1 p2 ->
        if !!debug then Format.printf "MERGE2 %d, %d@." p1 p2;
        let merged' = add_merged (p1,p2) merged in
        let map'' = sbst (p1,p2) map' in
        aux used last_sol merged' deps map'' hcs
    | (p1,p2)::map' ->
        let hcs' = List.map (map_horn_clause @@ replace_id p1 p2) hcs in
        incr cnt;
        match solve_option hcs' with
        | exception e -> Format.printf "DEPS: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements deps; raise e
        | None ->
            if !!debug then Format.printf "CANNOT MERGE %d, %d@." p1 p2;
            aux used last_sol merged deps map' hcs
        | Some sol ->
            if !!debug then Format.printf "MERGE3 %d, %d@." p1 p2;
            let merged' = add_merged (p1,p2) merged in
            let map'' = sbst (p1,p2) map' in
            let deps' = add_dependency (add_dependency deps (p1,p2)) (p2,p1) in
            if !!debug then save_dep deps' hcs' @@ Format.sprintf "tmp/test%d.dot" !cnt;
            if !!debug then Format.printf "SOLVED@.";
            if List.for_all (fun (_,(_,t)) -> t.desc = Const True) sol then
              sol, merged
            else
(*
              if !!debug then Format.printf "new_deps: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ List.Set.diff deps' deps;
 *)
              let used' = List.remove used p1 in
              aux used' sol merged' deps' map'' hcs'
  in
  if !!debug then Format.printf "init_deps: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) @@ Dependency.elements dependencies;
  if !!debug then save_dep dependencies hcs "tmp/test.dot";
  match solve_option hcs with
  | None -> None
  | Some sol ->
      let used = get_pred_ids_hcs hcs in
      let sol',merged = aux used sol [] dependencies map hcs in
      if !!debug then Format.printf "map: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) map;
      if !!debug then Format.printf "merged: %a@." (List.print @@ Pair.print Format.pp_print_int Format.pp_print_int) merged;
      if !!debug then Format.printf "Dom(sol'): %a@." (List.print Format.pp_print_int) @@ List.map fst sol';
      let aux' (x,y) =
        try
          Some (x, List.assoc y sol')
        with Not_found -> None
      in
      Some (List.filter_map aux' merged @ sol')

let assoc_pred_var p hcs =
  if 0=1 then Format.printf "APV: P_%d@." p;
  let rec find hcs =
    match hcs with
    | [] -> make_pred_var p []
    | (body,head)::hcs' ->
        match List.find_option (fun x -> is_pred_var x && Id.id x = p) @@ get_fv @@ make_ands (head::body) with
        | None -> find hcs'
        | Some x -> x
  in
  find hcs

let rec get_merge_candidates_aux typ1 typ2 =
  match typ1, typ2 with
  | Base None, Base _ -> []
  | Base _, Base None -> []
  | Base (Some(_, p1)), Base (Some(_, p2)) -> [p1, p2]
  | PApp(typ1', _), _ -> get_merge_candidates_aux typ1' typ2
  | _, PApp(typ2', _) -> get_merge_candidates_aux typ1 typ2'
  | Fun(_, typ11, typ12), Fun(_, typ21, typ22) -> get_merge_candidates_aux typ11 typ21 @ get_merge_candidates_aux typ12 typ22
  | Inter(_, typs), _ -> List.flatten_map (get_merge_candidates_aux typ2) typs
  | _, Inter(_, typs) -> List.flatten_map (get_merge_candidates_aux typ1) typs
  | _ ->
      Format.printf "get_merge_candidates_aux typ1: %a@." print_template typ1;
      Format.printf "get_merge_candidates_aux typ2: %a@." print_template typ2;
      assert false

let get_merge_candidates templates hcs =
  let aux typs =
    if !!debug then Format.printf "GMC typs: %a@." (List.print print_template) typs;
    List.flatten_map (get_merge_candidates_aux @@ List.hd typs) @@ List.tl typs
  in
  templates
  |> List.map (Pair.map_fst fst)
  |> List.classify ~eq:(Compare.eq_on ~eq:Id.eq fst)
  |> List.map (List.flatten_map (snd |- decomp_inter))
  |> List.flatten_map aux

let infer prog f typ (ce_set:ce_set) extend =
  let ce_set =
    if 0=1 then
      List.filter (fun (x,ce) -> Format.printf "%a, %a@.?: @?" Id.print x print_ce ce; read_int() <> 0) ce_set
    else
      ce_set
  in
  let {fun_typ_env=env; fun_def_env=fun_env} = prog in
  if !!debug then Format.printf "INFER prog: %a@." print_prog prog;
  let fun_env',t,make_get_rtyp = add_context true prog f typ in
  if !!debug then Format.printf "t: %a@.@." Print.term t;
  let comp_tree =
    let env' =
      env
      |> Ref_type.Env.filter_key_out (Id.same f)
      |> Ref_type.Env.map_value CPS.trans_ref_typ_as_direct
    in
    if !!debug then Format.printf "Dom(Fun_env'): %a@.@." (List.print Id.print) @@ List.map fst fun_env';
    if !!debug then Format.printf "t with def: %a@.@." Print.term @@ make_letrecs (List.map Triple.of_pair_r fun_env') t;
    if !!debug then Format.printf "t: %a@.@." Print.term t;
    if !!debug then Format.printf "env': %a@.@." Ref_type.Env.print env';
    Comp_tree.from_program env' fun_env' ce_set extend t
  in
  let fun_env = [](*make_fun_env comp_tree*) in
  if !!debug then Format.printf "fun_env: %a@.@." (List.print @@ Pair.print Id.print @@ Pair.print Id.print @@ Option.print Format.pp_print_int) fun_env;
  let templates = make_template env comp_tree in
  if !!debug then Format.printf "TEMPLATES: @[%a@.@." print_tmp_env templates;
  let constrs = generate_constraints templates comp_tree in
  if !!debug then Format.printf "CONSTR: @[%a@.@." (List.print print_constr) constrs;
  let hcs =
    constrs
    |> List.flatten_map to_horn_clause
    |> List.map @@ Pair.map (List.map Trans.init_base_rand) Trans.init_base_rand
    |@!!debug&> Format.printf "HORN CLAUSES: @[%a@.@." print_horn_clauses
    |@!!debug&> check_arity
  in
  let merge_candidates = get_merge_candidates templates hcs in
  match solve_merged merge_candidates hcs with
  | None -> None
  | Some sol ->
      if !!debug then Format.printf "TEMPLATES of TOP_FUNS: @[%a@.@." print_tmp_env @@ List.filter (fun ((f,_),_) -> Id.mem_assoc f fun_env') templates;
      if !!debug then Format.printf "  Dom(sol): %a@." (List.print Format.pp_print_int) @@ List.map fst sol;
      let top_funs = used_by f prog in
      if !!debug then Format.printf "TOP_FUNS: %a@.@." (List.print Id.print) top_funs;
      let env' =
        let aux ((g,_),tmp) =
          if Id.mem g top_funs then
            Some (g, apply_sol sol true tmp)
          else
            None
        in
        List.filter_map aux templates
      in
      let env'' =
        let aux (x,typ') =
          let typ = Ref_type.Env.assoc x prog.fun_typ_env in
          let x',_ = Ref_type.Env.find (fst |- Id.same x) env in
          if !!debug then Format.printf "  %a: %a@." Id.print x' Ref_type.print typ';
          let typ_ =
            if !!debug then Format.printf "  typ: %a@." Ref_type.print typ;
            if !!debug then Format.printf "  typ': %a@." Ref_type.print typ';
            let typ'' = Ref_type.contract typ' in
            make_get_rtyp (fun y -> assert (Id.same y x); typ'') x
          in
          if !!debug then Format.printf "  typ_: %a@." Ref_type.print typ_;
          x', typ_
        in
        env'
        |> List.map aux
        |> List.flatten_map (fun (x,typ) -> List.map (fun typ -> x, typ) @@ Ref_type.remove_equiv @@ Ref_type.decomp_inter typ)
        |> List.filter_out (fun (g,typ') -> Id.same f g && Ref_type.subtype typ typ')
        |> Ref_type.Env.of_list
        |*> Ref_type.Env.normalize
      in
      let env_unused =
        let aux f =
          if Id.mem_assoc f env' then
            None
          else if !infer_stronger then
            Some (f, Ref_type.make_strongest @@ Id.typ f)
          else
            Some (f, Ref_type.of_simple @@ Id.typ f)
        in
        Ref_type.Env.of_list @@ List.filter_map aux top_funs
      in
      let env''' = Ref_type.Env.merge env_unused env'' in
      if !!debug then Format.printf "Infer_mod.infer: %a@.@." Ref_type.Env.print env''';
      Some env'''
