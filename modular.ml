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
  | Arg(typ, ts) -> Format.fprintf fm "%a@%a" print_template typ (List.print Print.term) ts
  (*  | PApp(typ, []) -> print_template fm typ*)
  | PApp(typ, ts) -> Format.fprintf fm "%a%a" print_template typ (List.print Print.term) ts
  | Singleton t -> Format.fprintf fm "{%a}" Print.term t
  | Base None -> Format.fprintf fm "unit"
  | Base (Some p) -> Format.fprintf fm "P_%d" p
  | Fun(x, tmp1, tmp2) when is_fun_typ @@ Id.typ x -> Format.fprintf fm "(@[<hov 2>%a ->@ %a@])" print_template tmp1 print_template tmp2
  | Fun(x, tmp1, tmp2) -> Format.fprintf fm "(@[<hov 2>%a:%a ->@ %a@])" Id.print x print_template tmp1 print_template tmp2
  | Inter [] -> Format.fprintf fm "T"
  | Inter tmps -> Format.fprintf fm "(@[%a@])" (print_list print_template " /\\@ ") tmps

let print_horn_clause fm (pre,constr) =
  let pr_aux fm t =
    Format.fprintf fm "@[%s@]" @@ String.remove_char '_' @@ Format.asprintf "%a" Print.term t
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


let constr_of_typ typ =
  match typ with
  | PApp(Base(Some p1), ts1) ->
      Format.printf "constr_of_typ: ts1: %a@.@." (List.print Print.term) ts1;
      Exp (make_app (make_var @@ make_pred_var p1 ts1) ts1)
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

let rec inline_sub templates typ1 typ2 =
  let dbg = true in
  let r =
  match typ1,typ2 with
  | _, Inter typs -> _Ands @@ List.map (inline_sub templates typ1) typs
  | Singleton t, _ -> constr_of_typ @@ _PApp typ2 [t]
  | Base None, Base None -> Exp true_term
  | PApp(Base(Some p1), ts1), PApp(Base(Some p2), ts2) ->
      (*
      let xs = List.filter_map (function ((x,_), Base (Some p)) when p=p1 -> Some x | _ -> None) templates in
      constr_of_typ @@ _PApp typ2 [make_var @@ List.get xs]
       *)
      _Imply [constr_of_typ @@ _PApp typ1 [pred_var_term]] @@ constr_of_typ @@ _PApp typ2 [pred_var_term]
  | Fun(_, (Fun _ as typ11), typ12), Fun(_, (Fun _ as typ21), typ22) ->
      _And (inline_sub templates typ21 typ11) (inline_sub templates typ12 typ22)
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) ->
      let c1 = subst_constr x1 (make_var x2) @@ inline_sub templates typ21 typ11 in
      let c2 = constr_of_typ typ21 in
      let c3 = subst_constr x1 (make_var x2) @@ inline_sub templates typ12 typ22 in
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

let rec normalize_type templates typ =
  let nt typ' = normalize_type templates typ' in
  match typ with
  | Singleton _ -> typ
  | Base None -> Base None
  | Base (Some p) -> Base (Some p)
  | Var x ->
      let typs =
        templates
        |> List.filter (Id.same x -| fst -| fst)
        |> List.map (Pair.map_fst fst)
        |> List.sort
        |> List.map snd
      in
      _Inter typs
  | PApp(Base None, _) -> Base None
  | PApp(Singleton t, _) -> Singleton t
  | PApp(typ, ts) ->
      begin
        match nt typ with
        | PApp(typ', ts') -> PApp(typ', ts@ts')
        | Fun(x, typ1, typ2) -> Fun(x, nt (PApp(typ1,ts)), nt (PApp(typ2,ts)))
        | Inter typs -> Inter (List.map (fun typ' -> nt @@ PApp(typ',ts)) typs)
        | typ' -> PApp(typ', ts)
      end
  | Arg(typ, ts) ->
      begin
        match nt typ with
        | Fun(x, typ1, typ2) ->
            begin
              match ts with
              | [] -> typ1
              | t::ts' -> nt @@ subst_template x t @@ Arg(PApp(typ2, [t]), ts')
            end
        | Inter typs ->
            Inter (List.map (fun typ -> nt @@ Arg(typ, ts)) typs)
        | typ' ->
            Format.printf "  typ: %a@." print_template typ';
            assert false
      end
  | Fun(x, typ1, typ2) -> Fun(x, nt typ1, nt typ2)
  | Inter typs -> Inter (List.map nt typs)

let inline_sub templates typ1 typ2 =
  if !!debug then Format.printf "  typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  typ2: %a@." print_template typ2;
  inline_sub templates typ1 typ2

let get_nid (Rose_tree.Node((nid,_), _)) = nid

let in_comp_tree ct nid =
  List.exists (fst |- (=) nid) @@ Rose_tree.flatten ct


let make_sub_flag = ref true (* for debug *)

let make_sub ct templates typ1 typ2 =
  let templates' = List.filter (fst |- snd |- Option.for_all (in_comp_tree ct)) templates in
  if !!debug then Format.printf "  make_sub: typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  make_sub: typ2: %a@." print_template typ2;
  if !make_sub_flag then
(*    match typ1 with
    | Var x when not @@ is_fun_typ @@ Id.typ x -> constr_of_typ @@ normalize_type templates' @@ PApp(typ2, [make_var x])
    | PApp(Var x, ts) when not @@ is_fun_typ @@ Id.typ x -> constr_of_typ @@ normalize_type templates' @@ PApp(typ2, make_var x::ts)
    | _ ->*)
    let r = inline_sub templates' (normalize_type templates' typ1) (normalize_type templates' typ2) in
    if !!debug then Format.printf "  make_sub: %a@." print_constr (Sub(typ1,typ2));
    if !!debug then Format.printf "      ===>: %a@." print_constr r;
    r
  else
    Sub(typ1,typ2)

let filter_assumption env assumption =
  let check constr =
    let fv = get_fv constr in
    let fv' = List.filter_out is_pred_var fv in
    List.Set.subset fv' env
  in
  List.filter check assumption

let rec generate_constraints templates vars var_env assumption (Rose_tree.Node((_,label), children) as ct) =
  Format.printf "var_env: %a@." (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env;
  let dbg = 0=0 in
  let open Comp_tree in
  let r =
  let constr vs env asm = List.flatten_map (generate_constraints templates vs env asm) children in
  match label with
  | App((f, kind), local, vars', map) ->
      let assumption' =
        let aux (acc,env) (x,v) =
          if is_fun_typ @@ Id.typ x
          then acc, env
          else
            let env' = x::env in
            let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
            let asm1 = constr_of_typ @@ PApp(List.assoc ~cmp (x,None) templates, List.map make_var env') in
            let asm2 = Exp (make_eq (make_var x) v) in
            acc @ [asm1; asm2], env'
        in
        let base_vars = List.filter_out (is_fun_typ -| Id.typ) vars' in
        fst @@ List.fold_left aux (assumption, base_vars) map
      in
      let _,var_env' = List.fold_left (fun (vs,env) (x,_) -> x::vs, (x,vs)::env) (vars',var_env) map in
      let constr1 =
        if local then
          []
        else
          let _,constrs =
            let aux (env,typs) (_,t) =
              let env' = env @ [t] in
              let constr =
                let typ1 =
                  match t.desc with
                  | Syntax.Var f ->
                      let args = List.filter_map (Option.make (not -| is_fun_typ -| Id.typ) make_var) @@ Id.assoc f var_env' in
                      PApp(Var f, args)
                  | _ -> Singleton t
                in
                let typ2 =
                  let args = List.filter_map (Option.make (not -| is_fun_typ -| Id.typ) make_var) vars' in
                  if dbg then Format.printf "      args: %a@." (List.print Print.term) args;
                  PApp(Arg(Var f, env), args)
                in
                if dbg then Format.printf "      env: %a@." (List.print Print.term) env;
                if dbg then Format.printf "      gt: typ1: %a@." print_template typ1;
                if dbg then Format.printf "      gt: typ2: %a@." print_template typ2;
                make_sub ct templates typ1 typ2
              in
              if dbg then Format.printf "    constr: %a@."print_constr constr;
              env', constr::typs
            in
            List.fold_left aux ([],[]) map
          in
          let asm = filter_assumption vars assumption in
          if dbg then Format.printf "  asm: %a@." (List.print print_constr) asm;
          if dbg then Format.printf "  assumption: %a@." (List.print print_constr) assumption;
          if dbg then Format.printf "  assumption': %a@." (List.print print_constr) assumption';
          List.map (_Imply asm) constrs
      in
      let vars'' = List.rev_map fst map @ vars' in
      if dbg then Format.printf "  vars: %a@." (List.print Id.print) vars;
      if dbg then Format.printf "  vars': %a@." (List.print Id.print) vars';
      if dbg then Format.printf "  vars'': %a@.@." (List.print Id.print) vars'';
      constr1 @ constr vars'' var_env' assumption'
  | Let(f, vars', t) ->
(*
      if dbg then Format.printf "  vars: %a@." (List.print Id.print) vars;
      if dbg then Format.printf "  vars': %a@.@." (List.print Id.print) vars';
 *)
      assert (List.for_all2 Id.same vars vars');
      constr vars ((f,vars')::var_env) assumption
  | Spawn(f, tids) ->
      let asm = filter_assumption vars assumption in
      let app typ = PApp(typ, List.map make_var vars) in
      let constrs1 =
        if true
        then []
        else List.map (fun (g,tid) -> _Imply asm @@ make_sub ct templates (app @@ Var f) (app @@ Var g)) tids
      in
      constrs1 @ constr vars var_env assumption
  | Assume t ->
      constr vars var_env @@ Exp t::assumption
  | Fail ->
      assert (children = []);
      let asm = filter_assumption vars assumption in
      [_Imply asm @@ Exp false_term]
  in
  if dbg then Format.printf "label: %a@." print_label label;
  if dbg then Format.printf "vars: %a@." (List.print Id.print) vars;
  if dbg then Format.printf "assumption: %a@." (List.print print_constr) assumption;
  if dbg then Format.printf "r: %a@.@." (List.print print_constr) r;
  r
let generate_constraints xs fs templates ct =
  let env = List.map (fun x -> x, []) (xs@fs) in
  let constr = List.map (fun x -> constr_of_typ (normalize_type templates @@ PApp(Var x, [make_var pred_var]))) xs in
  constr @ generate_constraints templates [] env [] ct



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

let merge_template ts = List.flatten ts
let new_pred cnt = Base (Some (Counter.gen cnt))
let rec make_template cnt (Rose_tree.Node((nid,label), children)) =
  let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
  let dbg = false in
  let r=
  let templates = merge_template @@ List.map (make_template cnt) children in
  let open Comp_tree in
  match label with
  | App((f, _), _, var_env, map) ->
      if dbg then Format.printf "APP: %a@." print_label label;
      if dbg then Format.printf "  TEMPLATE: %a@." print_tmp_env templates;
      let aux label' =
        match label' with
        | App(_,_,_,map) ->
            let aux' (x,t) tmp2 =
              let tmp1 =
                if is_fun_typ @@ Id.typ x then
                  let tmp = (****)
                    templates
                    |> List.filter (Id.same x -| fst -| fst)
                    |> List.map (Pair.map_fst fst)
                    |> List.sort
                    |> List.map snd
                  in
                  match tmp with
                  | [] -> Inter []
                  | typ::_ -> typ
                else
                  new_pred cnt
              in
              Fun(new_var_of_term t, tmp1, tmp2)
            in
            let r = List.fold_right aux' map (Base None) in
            if dbg then Format.printf "  map: %a@." (List.print Id.print) @@ List.map fst map;
            if dbg then Format.printf "  typ: %a@." print_template r;
            r
        | _ ->
            if dbg then Format.printf "%a@." print_label label;
            assert false
      in
      let templates1,templates2 = List.partition (Id.same f -| fst -| fst(***)) templates in
      let templates1,templates2 = [], templates in
      let typ = aux label in
      let typs =
        if templates1 = []
        then [typ]
        else [typ; snd @@ List.get templates1]
      in
      let arg_templates =
        let xtyps,_ =  decomp_tfun typ in
        List.filter_map2 (fun (_,typ) (x,_) -> if is_fun_typ @@ Id.typ x then None else Some ((x,None),typ)) xtyps map
      in
      ((f,Some nid), _Inter typs)::arg_templates@templates2
  | Let(f, var_env, t) ->
      assert (is_fun_typ @@ Id.typ f);
      templates
  | Spawn(f,tids) ->
      let nids = List.map get_nid children in
      if dbg then Format.printf "  TEMPLATES: @[%a@.@."  print_tmp_env templates;
      if dbg then Format.printf "  SPAWN: %a@.@." Id.print f;
      ((f,Some nid), _Inter @@ List.map2 (fun (g,_) nid -> List.assoc ~cmp (g,Some nid) templates) tids nids)::templates
  | Assume _ -> templates
  | Fail -> []
  in
  if dbg then Format.printf "  %a@." Comp_tree.print_label label;
  if dbg then Format.printf "  LEN: %d@." (List.length r); r
let make_template xs t =
  let cnt = Counter.create() in
  List.map (fun x -> (x,None), new_pred cnt) xs @ make_template cnt t

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
      | "test.ml" when 9=0 -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]] | _ -> assert false)
      | "test.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test2.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test3.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "apply" -> [f, []] | "double" -> [f, []] | _ -> assert false)
      | "test4.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test5.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | _ -> [f, [1]])
      | _ -> assert false
    in
    List.flatten @@ List.map aux fun_env
  in
  let main = Option.get @@ get_last_definition normalized in
  let xs,comp_tree = Comp_tree.from_program fun_env ce_set main in
  let fs = List.map fst fun_env in
  let templates = make_template xs comp_tree in
  Format.printf "TEMPLATES: @[%a@.@." print_tmp_env templates;
  make_sub_flag := false;
  let constrs = generate_constraints xs fs templates comp_tree in
  Format.printf "CONSTR1: @[%a@.@." (List.print print_constr) constrs;
  make_sub_flag := true;
  let constrs = generate_constraints xs fs templates comp_tree in
  Format.printf "CONSTR2: @[%a@.@." (List.print print_constr) constrs;
  let hc = List.flatten_map to_horn_clause constrs in
  Format.printf "HORN CLAUSES: @[%a@.@." print_horn_clauses hc;
  Format.fprintf (Format.formatter_of_out_channel @@ open_out "test.hcs") "%a@." print_horn_clauses hc;
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
