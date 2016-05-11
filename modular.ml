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





let normalize = make_trans ()
let normalize_desc desc =
  let desc' = normalize.tr_desc_rec desc in
  match desc' with
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
      (make_lets binds @@ make_app t1 ts').desc
  | If(t1, t2, t3) ->
      begin
        match t1.desc with
        | Var x -> (make_if t1 t2 t3).desc
        | _ ->
            let x = new_var_of_term t1 in
            (make_let [x,[],t1] @@ make_if (make_var x) t2 t3).desc
      end
  | _ -> desc'
let () = normalize.tr_desc <- normalize_desc
let normalize = Trans.short_circuit_eval -| normalize.tr_term



(************************************************************************************************************)
(************************************************************************************************************)
(***********************************************************************************************************




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
  | Base of pred_var option
  | Fun of id * type_template * type_template
  | Inter of type_template list
and pred_var = int
and horn_clause = typed_term list * typed_term
and horn_clauses = horn_clause list

let pred_var = Id.make (-1) "v" TInt
let pred_var_term = make_var pred_var
let make_pred_var p ts =
  let n = List.length ts in
  let typ = List.fold_right make_tfun (List.make n TInt) TInt in
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
  | Base (Some p) -> Format.fprintf fm "P_%d" p
  | Fun(x, tmp1, tmp2) when is_fun_typ @@ Id.typ x -> Format.fprintf fm "(@[<hov 2>%a ->@ %a@])" print_template tmp1 print_template tmp2
  | Fun(x, tmp1, tmp2) -> Format.fprintf fm "(@[<hov 2>%a:%a ->@ %a@])" Id.print x print_template tmp1 print_template tmp2
  | Inter [] -> Format.fprintf fm "T"
  | Inter tmps -> Format.fprintf fm "(@[%a@])" (print_list print_template " /\\@ ") tmps

let __PApp loc (typ,ts) =
  if List.exists (is_fun_typ -| Syntax.typ) ts then failwith loc;
  PApp(typ,ts)

let print_horn_clause fm (pre,constr) =
  let pr_aux fm t =
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
let rec _PApp typ ts =
  match ts with
  | [] -> typ
  | t'::ts' ->
      match _PApp typ ts' with
      | PApp(typ', ts'') -> PApp(typ', t'::ts'')
      | typ' -> PApp(typ', [t'])
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

let rec get_fv_typ = function
  | Var f -> [f]
  | Arg(typ, ts) -> get_fv_typ typ @ List.flatten_map Syntax.get_fv ts
  | PApp(typ, ts) -> get_fv_typ typ @ List.flatten_map Syntax.get_fv ts
  | Singleton t -> Syntax.get_fv t
  | Base _ -> []
  | Fun(x,typ1,typ2) -> List.filter_out (Id.same x) @@ (get_fv_typ typ1 @ get_fv_typ typ2)
  | Inter typs -> List.flatten_map get_fv_typ typs
let rec get_fv = function
  | Exp t -> Syntax.get_fv t
  | And(c1, c2) -> get_fv c1 @ get_fv c2
  | Imply _ -> assert false
  | Sub(typ1,typ2) -> get_fv_typ typ1 @ get_fv_typ typ2
  | Pred(x,ts) -> x :: List.flatten_map Syntax.get_fv ts


let rec constr_of_typ typ =
  match typ with
  | PApp(Base(Some p1), ts) ->
      let ts' = List.filter_out (Syntax.typ |- is_fun_typ) ts in
      Format.printf "  constr_of_typ: ts: %a@." (List.print Print.term) ts;
      Exp (make_app (make_var @@ make_pred_var p1 ts') ts')
  | Inter typs -> constr_of_typ @@ List.hd typs
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
  let et = expand_type templates in
  let r =
  match typ with
  | Singleton _ -> typ
  | Base None -> Base None
  | Base (Some p) -> Base (Some p)
  | Var x ->
      let typ =
        templates
        |> List.filter (Id.same x -| fst -| fst)
        |> List.map (fun ((x,nid),typ) -> nid,typ)
        |> List.sort
        |> List.hd
        |> snd
      in
      et @@ typ
  | PApp(Base None, _) -> Base None
  | PApp(Singleton t, _) -> Singleton t
  | PApp(typ, ts) ->
      begin
        match et typ with
        | PApp(typ', ts') -> PApp(typ', ts@ts')
        | Fun(x, typ1, typ2) -> Fun(x, et (PApp(typ1,ts)), et (PApp(typ2,ts)))
        | Inter typs -> assert false(*Inter (List.map (fun typ' -> et @@ PApp(typ',ts)) typs)*)
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
                  let r = et @@ subst_template x t @@ Arg(typ2', ts') in
                  Format.printf "[%a |-> %a]%a = %a@." Id.print x Print.term t print_template (Arg(typ2', ts')) print_template r;
                  r
            end
(*
        | Inter typs ->
            Inter (List.map (fun typ -> et @@ Arg(typ, ts)) typs)
*)
        | typ' ->
            Format.printf "  typ: %a@." print_template typ;
            Format.printf "  typ': %a@." print_template typ';
            assert false
      end
  | Fun(x, typ1, typ2) -> Fun(x, et typ1, et typ2)
  | Inter typs ->
      assert false
             (*
      Inter (List.map et typs)
              *)
  in
(*
  if !!debug then Format.printf "NT: typ: %a@." print_template typ;
  if !!debug then Format.printf "NT: r: %a@." print_template r;
 *)
  r
let rec inline_sub templates typ1 typ2 =
  let _dbg = false in
  let r =
  match typ1,typ2 with
  | Inter typs1, Inter typs2 -> _Ands @@ List.map2 (inline_sub templates) typs1 typs2
  | Singleton t, _ -> constr_of_typ @@ _PApp typ2 [t]
  | Base None, Base None -> Exp true_term
  | PApp(Base(Some p1), ts1), PApp(Base(Some p2), ts2) ->
      _Imply [constr_of_typ @@ _PApp typ1 [pred_var_term]] @@ constr_of_typ @@ _PApp typ2 [pred_var_term]
  | Fun(_, (Fun _ as typ11), typ12), Fun(_, (Fun _ as typ21), typ22) ->
      _And (inline_sub templates typ21 typ11) (inline_sub templates typ12 typ22)
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) ->
      let c1 = subst_constr x1 (make_var x2) @@ inline_sub templates typ21 typ11 in
      let app typ =
        if is_fun_typ @@ Id.typ x2
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

let inline_sub templates typ1 typ2 =
  if !!debug then Format.printf "  typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  typ2: %a@." print_template typ2;
  inline_sub templates typ1 typ2

let get_nid (Rose_tree.Node((nid,_,_), _)) = nid

let in_comp_tree ct nid =
  List.exists (Triple.fst |- (=) nid) @@ Rose_tree.flatten ct


let hd_of_inter typ =
  match typ with
  | Inter typs -> List.hd typs
  | _ -> typ

let make_sub_flag = ref true (* for debug *)


let make_sub templates typ1 typ2 =
  if !!debug then Format.printf "  make_sub: typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  make_sub: typ2: %a@." print_template typ2;
  if !make_sub_flag then
    let typ1' = expand_type templates typ1 in
    let typ2' = expand_type templates typ2 in
    let r = inline_sub templates typ1' typ2' in
    if !!debug then Format.printf "  make_sub: %a@." print_constr (Sub(typ1,typ2));
    if !!debug then Format.printf "  make_sub: %a@." print_constr (Sub(typ1',typ2'));
    if !!debug then Format.printf "      ===>: %a@." print_constr r;
    r(*;
    Sub(typ1',typ2')*)
  else
    Sub(typ1,typ2)

let filter_assumption vars assumption =
  assumption
  |> List.filter (fun t -> List.Set.subset (Syntax.get_fv t) vars)
  |> List.map (fun t -> Exp t)

let make_assumption templates vars var_env =
  let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
  let aux x =
    Format.printf "    MA: x: %a@." Id.print x;
    let xs = x::List.assoc x var_env in
    Format.printf "    MA: xs: %a@." (List.print Id.print) xs;
    let xs' = List.filter (not -| is_fun_typ -| Id.typ) xs in
    let ts = List.map make_var xs' in
    let typ = PApp(List.assoc ~cmp (x,None) templates, ts) in
    constr_of_typ typ
    |@> Format.printf "    MA: constr: %a@.@." print_constr
  in
  vars
  |@> Format.printf "  MA: vars: %a@." (List.print Id.print)
  |> List.filter (not -| is_fun_typ -| Id.typ)
  |@> Format.printf "  MA: base_vars: %a@." (List.print Id.print)
  |> List.map aux
  |@> Format.printf "  MA: base_vars_constr: %a@." (List.print print_constr)


(*let vars_of_arg vars var_env*)
let _Imply typs = _Imply @@ List.rev typs (* for debug *)
let rec generate_constraints templates var_env assumption (Rose_tree.Node((nid,vars,label), children) as ct) =
  Format.printf "label: %a@." Comp_tree.print_label label;
  let dbg = 0=0 in
  let open Comp_tree in
  let r =
  let constr asm = List.flatten_map (generate_constraints templates var_env asm) children in
  let templates' = List.filter (fst |- snd |- Option.for_all (in_comp_tree ct)) templates in
  match label with
  | App((f, _), _, vars', map) ->
      let constr1 =
        let constrs =
          let aux (env,typs) (_,t) =
            let constr =
              if is_fun_typ t.typ then
                let typ1 =
                  match t.desc with
                  | Syntax.Var g -> Var g
                  | _ -> Singleton t
                in
                let typ2 = Arg(Var f, env) in
                let constr = make_sub templates' typ1 typ2 in
                constr
              else
                let typ = PApp(Arg(Var f, env), [t]) in
                let typ' = expand_type templates' typ in
                let constr = constr_of_typ typ' in
                if dbg then Format.printf "      ARG: typ: %a@." print_template typ;
                if dbg then Format.printf "      ARG: typ': %a@." print_template typ';
                if dbg then Format.printf "      ARG: constr: %a@." print_constr constr;
                constr
            in
            if dbg then Format.printf "    nid: %d@." nid;
            if dbg then Format.printf "    constr: %a@." print_constr constr;
            env@[t], [constr]@typs
          in
          if dbg then Format.printf "    map: %a@." (List.print @@ Pair.print Id.print Print.term) map;
          snd @@ List.fold_left aux ([],[]) map
        in
        if dbg then Format.printf "  label: %a@." print_label label;
        let asm1 = filter_assumption vars assumption in
        let asm2 = make_assumption templates' vars var_env in
        let asm = asm1 @ asm2 in
        if dbg then Format.printf "  asm1: %a@." (List.print print_constr) asm1;
        if dbg then Format.printf "  asm2: %a@." (List.print print_constr) asm2;
        List.map (_Imply asm) constrs
      in
      if dbg then Format.printf "  vars: %a@." (List.print Id.print) vars;
      if dbg then Format.printf "  vars': %a@." (List.print Id.print) vars';
      constr1 @ constr assumption
  | Let(f, _, t) ->
      constr assumption
  | Spawn(f, tids) ->
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
      constrs1 @*) constr assumption
  | Assume t ->
      constr @@ t::assumption
  | Fail ->
      assert (children = []);
      let asm1 = filter_assumption vars assumption in
      let asm2 = make_assumption templates vars var_env in
      let asm = asm1 @ asm2 in
      if dbg then Format.printf "  FAIL: asm1: %a@." (List.print print_constr) asm1;
      if dbg then Format.printf "  FAIL: asm2: %a@." (List.print print_constr) asm2;
      [_Imply asm @@ Exp false_term]
  in
  if dbg then Format.printf "label: %a@." print_label label;
  if dbg then Format.printf "vars: %a@." (List.print Id.print) vars;
  if dbg then Format.printf "assumption: %a@." (List.print Print.term) assumption;
  if dbg then Format.printf "r: %a@.@." (List.print print_constr) r;
  r

let generate_constraints templates var_env ct = generate_constraints templates var_env [] ct



let rec decomp_tfun typ =
  match typ with
  | Var _ -> assert false
  | Fun(x,typ1,typ2) -> Pair.map_fst (List.cons (x,typ1)) @@ decomp_tfun typ2
  | Inter [typ'] -> decomp_tfun typ'
  | Inter _ ->
      Format.printf "decomp_tfun: %a@." print_template typ;
      assert false
  | _ -> [], typ

let print_tmp_env fm env =
  let print_idx fm (x,nid) =
    match nid with
    | None -> Format.fprintf fm "%a" Id.print x
    | Some nid' -> Format.fprintf fm "%a@%d" Id.print x nid'
  in
  Format.fprintf fm "%a" (List.print @@ Pair.print print_idx print_template) env

let rec copy_template cnt typ =
  match typ with
  | Var _ -> assert false
  | Arg _ -> assert false
  | PApp(typ, _) -> copy_template cnt typ
  | Singleton _ -> assert false
  | Base None -> Base None
  | Base (Some p) -> Base (Some (Counter.gen cnt))
  | Fun(x,typ1,typ2) -> Fun(x, copy_template cnt  typ1, copy_template cnt typ2)
  | Inter typs -> Inter (List.map (copy_template cnt) typs)

let elim_papp typ =
  match typ with
  | PApp(typ, _) -> typ
  | _ -> typ

let merge_template ts = List.flatten ts
let new_pred cnt = Base (Some (Counter.gen cnt))
let rec make_template cnt fun_env var_env (Rose_tree.Node((nid,vars,label), children)) =
  let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
  let dbg = 0=0 in
  let r=
  let templates = merge_template @@ List.map (make_template cnt fun_env var_env) children in
  let open Comp_tree in
  match label with
  | App((f, _), local, vars', map) ->
      if dbg then Format.printf "APP: %a@." print_label label;
      if dbg then Format.printf "  TEMPLATE: %a@." print_tmp_env templates;
      let typ =
        let aux' (x,t) tmp2 =
          let tmp1 =
            if is_fun_typ @@ Id.typ x then
              templates
              |> List.filter (Id.same x -| fst -| fst)
              |> List.map (Pair.map_fst fst)
              |> List.sort
              |> List.map snd
              |> List.map elim_papp
              |> _Inter
            else
              new_pred cnt
          in
          Fun(new_var_of_term t, tmp1, tmp2)
        in
        let r = List.fold_right aux' map (Base None) in
        if dbg then Format.printf "  map: %a@." (List.print Id.print) @@ List.map fst map;
        if dbg then Format.printf "  typ: %a@." print_template r;
        r
      in
      let typ' = _PApp typ (List.map make_var @@ Id.assoc f var_env) in
      if dbg then Format.printf "  typ2 for %a: %a@." Id.print f (List.print Id.print) (Id.assoc f var_env);
      if dbg then Format.printf "  typ2 for %a: %a@." Id.print f print_template typ';
      let arg_templates =
        let typ'' =
          if local
          then
            let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
            if dbg then Format.printf "f: %a@." Id.print f;
            List.assoc ~cmp (Id.assoc f fun_env) templates
          else typ
        in
        let xtyps,_ =  decomp_tfun typ'' in
        let aux (_,typ) (x,t) =
          if is_fun_typ @@ Id.typ x then
            let g = Option.get @@ decomp_var t in
            let typs = List.filter_map (fun ((h,_),typ'') -> if Id.same x h then Some typ'' else None) templates in
            if dbg then Format.printf "  typ1 for %a: %a@." Id.print g print_template @@ _PApp (copy_template cnt @@ _Inter typs) (List.map make_var @@ Id.assoc g var_env);
            Some ((g, Some nid), _PApp (copy_template cnt @@ _Inter typs) (List.map make_var @@ Id.assoc g var_env))
          else
            None
              (*(x, None), typ*)
        in
        List.filter_map2 aux xtyps map
      in
      ((f,Some nid), typ')::arg_templates@templates
  | Let(f, var_env, t) ->
      assert (is_fun_typ @@ Id.typ f);
      templates
  | Spawn(f,tids) ->
      let nids = List.map get_nid children in
      if dbg then Format.printf "  TEMPLATES: @[%a@.@."  print_tmp_env templates;
      if dbg then Format.printf "  SPAWN: %a@.@." Id.print f;
      let typ = _Inter @@ List.map2 (fun (g,_) nid -> List.assoc ~cmp (g,Some nid) templates) tids nids in
      ((f,Some nid), typ)::templates
  | Assume _ -> templates
  | Fail -> []
  in
  if dbg then Format.printf "  %a@." Comp_tree.print_label label;
  if dbg then Format.printf "  LEN: %d@." (List.length r); r
let rec make_arg_template cnt fun_env var_env templates (Rose_tree.Node((nid,vars,label), children)) =
  let open Comp_tree in
  let arg_templates = List.flatten_map (make_arg_template cnt fun_env var_env templates) children in
  match label with
  | App((f,_), local, _, map) ->
      let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
      let typ' =
        if local then
          let g,nid = (Id.assoc f fun_env) in
          Format.printf "MAT: g,nid: %a, %a@." Id.print g (Option.print Format.pp_print_int) nid;
          List.assoc ~cmp (g,nid) templates
        else
          List.assoc ~cmp (f, Some nid) templates
      in
      let xtyps,_ =  decomp_tfun typ' in
      let aux (_,typ) (x,t) =
        if is_fun_typ @@ Id.typ x then
          None
        else
          Some ((x, None), typ)
      in
      List.filter_map2 aux xtyps map
  | Let _
  | Spawn _
  | Assume _
  | Fail -> arg_templates
let make_template xs fun_env var_env t =
  let cnt = Counter.create() in
  let templates1 = List.map (fun x -> (x,None), new_pred cnt) xs in
  Format.printf "templates1: @[%a@.@." print_tmp_env templates1;
  let templates2 = make_template cnt fun_env var_env t in
  Format.printf "templates2: @[%a@.@." print_tmp_env templates2;
  let templates3 = make_arg_template cnt fun_env var_env templates2 t in
  Format.printf "templates3: @[%a@.@." print_tmp_env templates3;
  templates1 @ templates2 @ templates3



let rec make_var_env (Rose_tree.Node((_,vars,label), children)) =
  let open Comp_tree in
  let var_env = List.flatten_map make_var_env children in
  match label with
  | App(_, _, vars', map) ->
      snd @@ List.fold_left (fun (vs,env) (x,_) -> x::vs, (x,vs)::env) (vars', var_env) map
  | Let(f, _, t) ->
      (f,vars) :: var_env
  | Spawn(_, tids) ->
      List.map (Pair.map_snd @@ Fun.const []) tids @ var_env
  | Assume t ->
      var_env
  | Fail ->
      var_env



let rec make_fun_env (Rose_tree.Node((nid,_,label), children)) =
  let open Comp_tree in
  let fun_env = List.flatten_map make_fun_env children in
  match label with
  | App(_, _, _, map) ->
      let fun_env' = List.filter_map (fun (x,t) -> if is_fun_typ @@ Id.typ x then Some (x, (Option.get @@ decomp_var t, Some nid)) else None) map in
      fun_env' @ List.map (fun (f,(g,i)) -> f, (try Id.assoc g fun_env' with Not_found -> g, i)) fun_env
  | Let _
  | Spawn _
  | Assume _
  | Fail -> fun_env



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
  let fun_defs = List.flatten_map (snd |- List.map Triple.to_pair_r) fbindings in
  let ce_set =
    let aux =
      match !Flag.filename with
      | "test.ml" when 9=0 -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]] | _ -> assert false)
      | "test.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test2.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test3.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [0;1]] | "apply" -> [f, []] | "double" -> [f, []] | _ -> assert false)
      | "test4.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test5.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | _ -> [f, [1]])
      | "test6.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [(*f, [0];*) f, [1;0]] | "double" -> [f, [0]] | _ -> assert false)
      | _ -> assert false
    in
    List.flatten @@ List.map aux fun_defs
  in
  let main = Option.get @@ get_last_definition normalized in
  let xs,comp_tree = Comp_tree.from_program fun_defs ce_set main in
  let fs = List.map fst fun_defs in
  let var_env = make_var_env comp_tree in
  let var_env' = List.map (fun x -> x, []) (xs@fs) @ var_env in
  Format.printf "var_env': %a@.@." (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
  let fun_env = make_fun_env comp_tree in
  Format.printf "fun_env': %a@.@." (List.print @@ Pair.print Id.print @@ Pair.print Id.print @@ Option.print Format.pp_print_int) fun_env;
  let templates = make_template xs fun_env var_env comp_tree in
  Format.printf "TEMPLATES: @[%a@.@." print_tmp_env templates;
  let constrs = generate_constraints templates var_env comp_tree in
  Format.printf "CONSTR: @[%a@.@." (List.print print_constr) constrs;
  let hc = List.flatten_map to_horn_clause constrs in
  Format.printf "HORN CLAUSES: @[%a@.@." print_horn_clauses hc;
  Format.fprintf (Format.formatter_of_out_channel @@ open_out "test.hcs") "%a@." print_horn_clauses hc;
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
  constrs






(************************************************************************************************************)
(************************************************************************************************************)
***********************************************************************************************************)










module RefTypInfer = struct
  open Fpat
  open Util
  open Combinator

  let infer_etrs fs is_cp prog etrs =
    etrs
    |> Fpat.RefTypInfer.infer_etrs fs is_cp prog
    |*@> Format.printf "refinement types:@,  %a@." Fpat.RefType.pr_env
(*
    |> List.map (Pair.map_snd Fpat.AbsType.of_refinement_type)
    |> Fpat.Util.List.classify (Fpat.Combinator.comp2 (=) fst fst)
    |> List.map
         (function
           | (f, sty) :: fstys ->
              f, Fpat.AbsType.merge (sty :: List.map snd fstys)
           | _ -> assert false)
*)

  let refine prog fs is_cp cexs feasible ext_cexs =
    let etrs =
      Fpat.Util.List.concat_map2
        (fun cex ext_cex ->
         let penv =
           List.map
             (fun (p, ps) ->
              let cnt = ref 0 in
              p,
              fun ts ->
              let (tenv, phi) = List.nth ps !cnt in
              let tenv = tenv @ [p, Type.mk_int] in
              cnt := !cnt + 1;
              Logger.debug_assert
                (fun () -> List.length tenv = List.length ts)
                ~on_failure:
                (fun () ->
                 Format.printf
                   "AbsTypInfer.refine: the lengths of %a and %a are different"
                   TypEnv.pr tenv
                   Term.pr_list ts);
              let tsub = List.map2 (fun (x, _) t -> x, t) tenv ts in
              let tts = List.map2 (fun (_, ty) t -> t, ty) tenv ts in
              Pva.make
                (Idnt.T(Idnt.T(p, !cnt, List.length tenv - 1), -1, 0))
                tts,
              Formula.subst tsub phi)
             ext_cex
         in
         CompTreeExpander.error_traces_of prog feasible penv [cex])
        cexs ext_cexs
    in
    infer_etrs fs is_cp prog etrs
end



type program = (id * typed_term) list

let print_ce_set fm ce_set =
  let pr fm (f, ce) = Format.fprintf fm "%a: %a" Id.print f (List.print Format.pp_print_int) ce in
  Format.fprintf fm "%a" (print_list pr ",@ ") ce_set

let to_CEGAR_ref_type_base base =
  match base with
  | Ref_type.Unit -> CEGAR_ref_type.Unit
  | Ref_type.Bool -> CEGAR_ref_type.Bool
  | Ref_type.Int -> CEGAR_ref_type.Int
  | Ref_type.Abst s -> CEGAR_ref_type.Abst s
let rec to_CEGAR_ref_type typ =
  match typ with
  | Ref_type.Base(base, x, p) -> CEGAR_ref_type.Base(to_CEGAR_ref_type_base base, Id.to_string x, snd @@ CEGAR_trans.trans_term p)
  | Ref_type.Fun(x, typ1, typ2) -> CEGAR_ref_type.Fun(Id.to_string x, to_CEGAR_ref_type typ1, to_CEGAR_ref_type typ2)
  | Ref_type.Inter typs -> CEGAR_ref_type.Inter (List.map to_CEGAR_ref_type typs)
  | _ -> unsupported "Ref_type.to_CEGAR_ref_type"

let rec add_id_event t =
  match t.desc with
  | Let(flag, bindings, t') ->
      let bindings' = List.map (Triple.map_trd @@ make_seq (make_event_unit "id")) bindings in
      {t with desc=Let(flag, bindings', add_id_event t')}
  | _ -> t

let infer_ref_type ces t =
  if !!debug then Format.printf "infer_ref_type t: %a@." Print.term t;
  if !!debug then Format.printf "infer_ref_type ces: %a@." (List.print @@ List.print Format.pp_print_int) ces;
  Main_loop.init_typ_excep ();
  let t' = add_id_event t in
  if !!debug then Format.printf "infer_ref_type t': %a@." Print.term t';
  let prog, make_get_rtyp, info = Main_loop.preprocess t' Spec.init in
  if !!debug then Format.printf "prog: %a@." CEGAR_print.prog prog;
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  if !!debug then Format.printf "inliend_functions: [%s]@." @@ String.join "; " inlined_functions;
  let ces' = List.map (CEGAR_trans.trans_ce labeled prog) ces in
  let prog = FpatInterface.conv_prog prog in
  let env = RefTypInfer.refine prog inlined_functions is_cp ces' false (List.map (Fun.const []) ces) in
  if !!debug then Format.printf "ENV: @[%a@." Fpat.RefType.pr_env env;
  let get_rtyp f =
    List.assoc (FpatInterface.conv_var f) env
    |*@> Format.printf "typ1: %a@." Fpat.RefType.pr
    |> Ref_type.from_fpat
    |*@> Format.printf "typ2: %a@." Ref_type.print
    |> to_CEGAR_ref_type
    |*@> Format.printf "typ3: %a@." CEGAR_ref_type.print
  in
  make_get_rtyp get_rtyp

(*
let infer_abs_type spec ces parsed =
  assert (spec.Spec.ref_env <> []);
  let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
  let t,main = Trans.ref_to_assert ref_env parsed in
  Main_loop.init_typ_excep ();
  let prog, get_rtyp, info = Main_loop.preprocess t spec in
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  let ces' = List.map (CEGAR_trans.trans_ce labeled prog) ces in
  let prog = FpatInterface.conv_prog prog in
  let typ =
    RefTypInfer.refine prog inlined_functions is_cp ces' false (List.map (Fun.const []) ces)
    |@true&> Format.printf "ENV: @[%a@." Fpat.RefType.pr_env
  in
  typ, main
*)

let make_fix f xs t =
  make_letrec [f, xs, t] @@ make_var f
let decomp_fix t =
  match t.desc with
  | Let(Recursive, [f, xs, t'], {desc=Var g}) when f = g -> Some (f, xs, t')
  | _ -> None
let is_fix t = decomp_fix t <> None

let get_arg_num = List.length -| Triple.snd -| Option.get -| decomp_fix


let counter = Counter.create ()
let new_label () = Counter.gen counter

let add_label = make_trans2 ()
let add_label_term l t =
  let t' = add_label.tr2_term_rec l t in
  match t.desc with
  | If(t1, t2, t3) -> add_id l t'
  | _ -> t'
let () = add_label.tr2_term <- add_label_term
let add_label = add_label.tr2_term
let get_label t = Option.try_with (fun () -> get_id t) ((=) @@ Invalid_argument "get_id")

let bool_of_term' t = Option.try_any (fun _ -> bool_of_term t)

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let exists_fail ts = List.exists is_fail ts

let append_path path rs =
  List.map (Triple.map_trd @@ (@) path) rs

type dir = Single_to_Modular | Modular_to_Single
type eval_result =
  | Single of (typed_term * (int * int list) list * int list) list
  | Modular of (typed_term * int list * int list)

let init_result_of_dir dir t ce_env =
  match dir with
  | Modular_to_Single -> Single [t, ce_env, []]
  | Single_to_Modular -> Modular(t, snd @@ List.get ce_env, [])

let decomp_single r =
  match r with
  | Single rs -> rs
  | Modular _ -> assert false

let decomp_modular r =
  match r with
  | Single _ -> assert false
  | Modular r -> r


(* ASSUME: Input must be normal form *)
let rec eval dir fun_env ce_set ce_env label_env t =
  let dbg = 0=0 in
  if dbg then Format.printf"@[ORIG(%d): %a@\n  @[" (List.length ce_set) Print.term t;
  let r =
  (if dbg then match dir with Single_to_Modular -> Format.printf "Single to Modular.@\n" | Modular_to_Single -> Format.printf "Modular to Single.@\n");
  if dbg then Format.printf "Dom(FUN_ENV): %a@\n" (List.print Id.print) @@ List.map fst fun_env;
  if dbg then Format.printf "CE_ENV: %a@\n" (List.print @@ Pair.print Format.pp_print_int @@ List.print Format.pp_print_int) ce_env;
  match t.desc with
  | Const _
  | Var _
  | BinOp _
  | Not _
  | Fun _
  | Event _ -> init_result_of_dir dir t ce_env
  | _ when is_fail t -> init_result_of_dir dir t ce_env
  | Bottom ->
      assert (dir = Modular_to_Single);
      Single []
  | App({desc=Const(RandValue _)}, [t2]) ->
      assert (t2 = unit_term);
      init_result_of_dir dir t ce_env
  | App({desc=Fun(x,t1)}, v::vs) when List.for_all is_value vs ->
      let t' = make_app (subst x v t1) vs in
      eval dir fun_env ce_set ce_env label_env t'
  | App(t1, ts) when is_fix t1 ->
      let n = get_arg_num t1 in
      if n < List.length ts then
        init_result_of_dir dir t ce_env
      else if n > List.length ts then
        unsupported "Modular.eval: App(fix, _)"
      else
        let f,xs,t1' = Option.get @@ decomp_fix t1 in
        let t1'' = subst f t1 t1' in
        let t' = List.fold_right2 subst xs ts t1'' in
        eval dir fun_env ce_set ce_env label_env t'
  | App({desc=Var f}, ts) when List.length ts > List.length @@ fst @@ Id.assoc f fun_env ->
      let n = List.length @@ fst @@ Id.assoc f fun_env in
      let ts1,ts2 = List.split_nth n ts in
      eval dir fun_env ce_set ce_env label_env @@ make_app2 (make_app (make_var f) ts1) ts2
  | App({desc=Var f}, ts) ->
      let ys,t_f = Id.assoc f fun_env in
      assert (List.length ts <= List.length ys);
      if List.length ts < List.length ys then
        init_result_of_dir dir t ce_env
      else
        let label = new_label () in
        let label_env' = (label,f)::label_env in
        if dbg then Format.printf "LABEL: %a, %d@\n  @[" Id.print f label;
        begin
          match dir with
          | Modular_to_Single ->
              let t_f' = add_label label t_f in
              let paths = List.assoc_all ~cmp:Id.eq f ce_set in
              if paths = [] then
                let xs,typ = decomp_tfun t_f.typ in
                let t' = make_funs xs @@ unit_term in
                if dbg then Format.printf "typ: %a@\n" Print.typ typ;
                assert (can_unify typ TUnit);
                Single [t', ce_env, []]
              else
                let aux path =
                  if dbg then Format.printf "PATH: %d, %a@\n" label (List.print Format.pp_print_int) path;
                  let t' = make_app (make_fix f ys t_f') ts in
                  let ce_env' = (label,path)::ce_env in
                  decomp_single @@ eval dir fun_env ce_set ce_env' label_env' t'
                in
                let r = List.flatten_map aux paths in
                if dbg then Format.printf "@]";
                Single r
          | Single_to_Modular ->
              let t' = make_app (make_fix f ys t_f) ts in
              eval dir fun_env ce_set ce_env label_env' t'
        end
  | App(t1, [t2]) ->
      assert (not @@ is_value t1);
      assert (is_value t2);
      let r = eval dir fun_env ce_set ce_env label_env t1 in
      begin
        match r with
        | Single rs ->
            let aux (v,ce_env,path) =
              if is_fail v then
                [fail_unit_term, ce_env, path]
              else
                match v.desc with
                | Fun(x,t1') -> decomp_single @@ eval dir fun_env ce_set ce_env label_env @@ subst x t2 t1'
                | _ -> assert false
            in
            Single (List.flatten_map aux rs)
        | Modular(v, ce, path) ->
            if is_fail v then
              Modular(fail_unit_term, ce, path)
            else
              match v.desc with
              | Fun(x,t1') -> eval dir fun_env ce_set ce_env label_env @@ subst x t2 t1'
              | _ -> assert false
      end
  | If(_, t2, t3) ->
      begin
        match dir with
        | Modular_to_Single ->
            let label = get_label t in
            let r =
              match label with
              | None ->
                  let aux br =
                    let t23 = if br = 0 then t2 else t3 in
                    append_path [br] @@ decomp_single @@ eval dir fun_env ce_set ce_env label_env t23
                  in
                  aux 0 @ aux 1
              | Some label ->
                  let ce,ce_env' = List.decomp_assoc label ce_env in
                  match ce with
                  | [] -> []
                  | 2::_ -> []
                  | br::ce' ->
                      if dbg then
                        Format.printf "CE_ENV[%d]: %a@\n" label (List.print @@ Pair.print Format.pp_print_int @@ List.print Format.pp_print_int) ce_env;
                      if dbg then
                        Format.printf "CE[%d]: %a@\n" label (List.print Format.pp_print_int) ce;
                      let t23 = if br = 0 then t2 else t3 in
                      let ce_env'' = (label,ce')::ce_env' in
                      if dbg then
                        Format.printf "CE_ENV''[%d]: %a@\n" label (List.print @@ Pair.print Format.pp_print_int @@ List.print Format.pp_print_int) ce_env'';
                      append_path [br] @@ decomp_single @@ eval dir fun_env ce_set ce_env'' label_env t23
            in
            Single r
        | Single_to_Modular ->
            let label = get_label t in
            let f,ce = List.get ce_env in
            match ce with
            | [] -> assert false
            | br::ce' ->
                let t23 = if br = 0 then t2 else t3 in
                let v,ce_env,path = decomp_modular @@ eval dir fun_env ce_set [f,ce'] label_env t23 in
                let path' =
                  match label with
                  | None -> path
                  | Some label -> br::path
                in
                Modular(v, ce_env, path')
      end
  | Let _ when is_fix t -> init_result_of_dir dir t ce_env
  | Let(flag, [], t2) -> eval dir fun_env ce_set ce_env label_env t2
  | Let(flag, [f,xs,t1], t2) when Id.mem_assoc f fun_env ->
      let fun_env' = List.map (fun (g,def) -> if Id.same f g then f,(xs,t1) else g,def) fun_env in
      eval dir fun_env' ce_set ce_env label_env t2
  | Let(flag, [f,xs,t1], t2) ->
      assert (flag = Nonrecursive || not @@ Id.mem f @@ Term_util.get_fv t1);
      if xs = [] then
        match eval dir fun_env ce_set ce_env label_env t1 with
        | Single rs ->
            let aux (v,ce_env,path) =
              if is_fail v then
                [fail_unit_term, ce_env, path]
              else
                t2
                |> (if is_base_typ t1.typ then Fun.id else subst f v)
                |> eval dir fun_env ce_set ce_env label_env
                |> decomp_single
                |> append_path path
            in
            Single (List.flatten_map aux rs)
        | Modular(v, ce, path) ->
            if is_fail v then
              Modular(fail_unit_term, ce, path)
            else
              t2
              |> (if is_base_typ t1.typ then Fun.id else subst f v)
              |> eval dir fun_env ce_set [0,ce] label_env
      else
        let t2' = subst f (make_funs xs t1) t2 in
        eval dir fun_env ce_set ce_env label_env t2'
  | _ ->
      Format.printf "@.%a@." Print.term t;
      unsupported "Modular.eval"
  in
  if dbg then
    Format.printf"@]@\nRETURN@\n@]";
  r

type result =
  | Typable of Ref_type.env
  | Untypable of (Syntax.id * int list) list

let add_context for_infer env f (xs,t) typ =
  let dbg = 0=9 in
  if dbg then Format.printf "ADD_CONTEXT: %a :? %a@." Print.id f Ref_type.print typ;
  let env' = List.takewhile (not -| Id.same f -| fst) env in
  if dbg then Format.printf "ADD_CONTEXT ENV': %a@." (List.print @@ Pair.print Id.print Ref_type.print) env';
  let t' = add_label 0 t in
  make_letrec [f,xs,t'] unit_term
  |@dbg&> Format.printf "ADD_CONTEXT ORIG: %a@.@." Print.term
  |> Trans.ref_to_assert [f,typ]
  |> fst
  |@dbg&> Format.printf "ADD_CONTEXT REF_TO_ASSERT: %a@.@." Print.term
  |> Trans.make_ext_funs ~asm:for_infer env'
  |@dbg&> Format.printf "ADD_CONTEXT MAKE_EXT_FUNS: %a@.@." Print.term
  |> normalize
  |@dbg&> Format.printf "ADD_CONTEXT MAKE_EXT_FUNS: %a@.@." Print.term

let infer env fun_defs f typ t ce_set =
  let t = add_context true env f (Id.assoc f fun_defs) typ in
  if !!debug then Format.printf "CE_SET: %a@." (List.print @@ Pair.print Id.print (List.print Format.pp_print_int)) ce_set;
  let rs = decomp_single @@ eval Modular_to_Single fun_defs ce_set [] [] t in
  let rs' = List.filter (is_fail -| Triple.fst) rs in
  let ces = List.map (Triple.trd) rs' in
  if !!debug then Format.printf "@.Counterexamples: %a@." (List.print @@ List.print Format.pp_print_int) ces;
  let get_rtyp = infer_ref_type ces t in
  let fun_defs' = List.takewhile (not -| Id.same f -| fst) fun_defs in
  if !!debug then Format.printf "@.fun_defs': %a@," (List.print Id.print) @@ List.map fst fun_defs';
  let cand_env = List.map (fun (g,_) -> g, get_rtyp g) fun_defs' in
  if !!debug then Format.printf "@.candidate refinement types:@,  %a@," (List.print @@ Pair.print Id.print Ref_type.print) cand_env;
  cand_env

let check env f fun_defs typ =
  if !Flag.print_progress then Format.printf "  Check %a : %a@." Id.print f Ref_type.print typ;
  let t = add_context false env f (Id.assoc f fun_defs) typ in
  if !!debug then Format.printf "  Check: %a : %a@." Print.term t Ref_type.print typ;
  let (result, make_get_rtyp, set_target'), main, set_target = Main_loop.verify [] Spec.init t in
  match result with
  | CEGAR.Safe env ->
      Typable (Main_loop.trans_env (List.map fst fun_defs) make_get_rtyp env)
  | CEGAR.Unsafe(sol, ModelCheck.CESafety ce) ->
      if !!debug then Format.printf "  Untypable@.@.";
      let path =
        Format.printf "  CE_INIT: %a@\n" (List.print Format.pp_print_int) ce;
        let v,ce',path = decomp_modular @@ eval Single_to_Modular fun_defs [] [0,ce] [] t in
        assert (v = fail_unit_term);
        assert (ce' = []);
        if path = [] then [2] else path
      in
      Untypable [f, path]
  | CEGAR.Unsafe _ -> assert false

(* "t" must be "add_context env f _ _ typ" *)
let rec main_loop c orig env fun_defs f typ t ce_set =
  if !!debug then Format.printf "MAIN_LOOP[%a,%d]: %a :? %a@." Id.print f c Id.print f Ref_type.print typ;
  if !!debug then Format.printf "MAIN_LOOP[%a,%d] env: %a@." Id.print f c (List.print @@ Pair.print Id.print Ref_type.print) env;
  if !!debug then Format.printf "MAIN_LOOP[%a,%d] t: %a@.@." Id.print f c Print.term t;
  match check env f fun_defs typ with
  | Typable env' ->
      if !!debug then Format.printf "TYPABLE: %a : %a@.@." Id.print f Ref_type.print typ;
      Typable env'
  | Untypable ce_set' ->
      if !!debug then Format.printf "UNTYPABLE: %a : %a@." Id.print f Ref_type.print typ;
      if !!debug then Format.printf "UNTYPABLE ce_set': %a@.@." (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) ce_set';
      let rec refine_loop ce_set'' =
        let candidate = infer env fun_defs f typ t ce_set'' in
        let aux r (g,typ') =
          if !!debug then Format.printf "CHECK CANDIDATE: %a :? %a@.@." Id.print g Ref_type.print typ';
          match r with
          | Typable env ->
              let t' = add_context false env g (Id.assoc g fun_defs) typ' in
              main_loop 0 orig env fun_defs g typ' t' ce_set''
          | Untypable ce_set -> Untypable ce_set
        in
        match List.fold_left aux (Typable env) candidate with
        | Typable env' ->
            if !!debug then Format.printf "ALL CANDIDATES ARE VALID@.@.";
            main_loop (c+1) orig env' fun_defs f typ t ce_set''
        | Untypable ce_set''' ->
            if !!debug then Format.printf "CANDIDATE IS INVALID@.@.";
            refine_loop (ce_set''' @ ce_set'')
      in
      refine_loop (ce_set' @ ce_set)
let main_loop orig env fun_defs f typ t ce_set = main_loop 0 orig env fun_defs f typ t ce_set

let main orig spec parsed =
  if spec <> Spec.init then unsupported "Modular.main: spec";
  let normalized = Trans.inline_var @@ Trans.flatten_let @@ Trans.normalize_let parsed in
  if !!debug then Format.printf "NORM: %a@.@." Print.term normalized;
  let fbindings,main = decomp_prog normalized in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_defs = List.flatten_map (fun (_,bindings) -> List.map (fun (f,xs,t) -> f, (xs,t)) bindings) fbindings in
  if !!debug then Format.printf "FUN_DEFS: %a@." (List.print Id.print) @@ List.map fst fun_defs;
  let _,(f,_) = List.decomp_snoc fun_defs in
  let typ = Ref_type.from_simple @@ Id.typ f in
(*
  let ce_set = List.flatten @@ List.mapi (fun i (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [1;0](*; f, [1;0;1]*)] | _ -> assert false) fun_defs in
 *)
  let env_init = List.map (fun (f,_) -> f, Ref_type.make_weakest @@ Trans.inst_tvar_tunit_typ @@ Id.typ f) fun_defs in
  if !!debug then Format.printf "ENV_INIT: %a@." (List.print @@ Pair.print Id.print Ref_type.print) env_init;
  let t = add_context false env_init f (Id.assoc f fun_defs) typ in
  match main_loop parsed env_init fun_defs f typ t [] with
  | Typable _ -> true
  | Untypable _ -> false
