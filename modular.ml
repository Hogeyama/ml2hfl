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
  | Arg of int * type_template
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
  | Arg(i, typ) -> Format.fprintf fm "#%d(%a)" i print_template typ
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

let rec add_bind f map typ =
  match typ with
  | Var g -> if Id.same f g then _PApp typ map else typ
  | Arg(i, typ) -> Arg(i, add_bind f map typ)
  | PApp(typ1, map1) -> _PApp (add_bind f map typ1) map1
  | Singleton _ -> typ
  | _ -> assert false
let add_bind f map typ =
  let map' = List.filter_out (is_fun_typ -| Id.typ) map in
  add_bind f (List.map make_var map') typ

let rec apply f = function
  | Exp t -> Exp t
  | And(c1,c2) -> And(apply f c1, apply f c2)
  | Imply(c1, c2) -> Imply(apply f c1, apply f c2)
  | Sub(typ1, typ2) -> Sub(f typ1, f typ2)
  | Pred(x,ts) -> Pred(x,ts)
let apply = ()

let rec get_fv_typ = function
  | Var f -> [f]
  | Arg(_,  typ) -> get_fv_typ typ
  | PApp(typ, _) -> get_fv_typ typ
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

(*
let rec decomp_base tmp =
  match tmp with
  | Base _ -> assert false
  | PApp(tmp, ts) -> Option.map (Pair.map_snd @@ (-$-) (@) ts) @@ decomp_base tmp
  | Fun _ -> None
  | Inter _ -> None
  | Var _
  | Arg _
  | Singleton _ ->
      Format.printf "%a@." print_template tmp;
      assert false
let is_base = Option.is_some -| decomp_base
 *)
(*
let term_of_base templates typ =
  let p,ts = Option.get @@ decomp_base typ in
  let p' = Option.get p in
  make_app (make_var @@ List.assoc p' templates) (pred_var_term::ts)
 *)

(*
let rec make_sub_inline templates typ1 typ2 = assert false
  match typ1, typ2 with
  | Singleton t1, _ when is_base typ2 ->
      _Imply [Exp t1] @@ Exp (term_of_base templates typ2)
  | _ when is_base typ1 && is_base typ2 ->
      let p2,_ = Option.get @@ decomp_base typ2 in
      if p2 = None
      then Exp true_term
      else _Imply [Exp (term_of_base pred_map typ1)] @@ Exp (term_of_base pred_map typ2)
  | _, Inter typs ->
      List.fold_left (fun acc typ2 -> _And acc (dsc @@ Sub(typ1, typ2))) (Exp true_term) typs
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) when is_base typ11 ->
      assert (is_base typ21);
      let c1 = dsc @@ Sub(typ21,typ11) in
      let t = subst_var pred_var x2 @@ term_of_base pred_map typ21 in
      let c2 = subst_constr x1 (make_var x2) @@ dsc @@ Sub(typ12, typ22) in
      _And c1 @@ _Imply [Exp t] c2
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) -> unsupported "decomp_constraints"
  | _ ->
      Format.printf "%a@." print_constr constr;
      assert false
*)

(*
let rec decomp_var tmp =
  match tmp with
  | Var x -> Some (x, [])
  | App(tmp, ts) -> Option.map (Pair.map_snd @@ (-$-) (@) ts) @@ decomp_var tmp
  | Base p -> None
  | Fun _ -> None
  | Inter _ -> None
  | Arg _
  | None ->
      Format.printf "%a@." print_template tmp;
      assert false
let is_var = Option.is_some -| decomp_var
 *)

let constr_of_typ typ =
  match typ with
  | PApp(Base(Some p1), ts1) ->
      Exp (make_app (make_var @@ make_pred_var p1 ts1) ts1)
  | _ ->
      Format.printf "  typ: %a@." print_template typ;
      assert false

let rec subst_template x t tmp =
  let sbst = subst_template x t in
  match tmp with
  | Var _ -> tmp
  | Arg(i, tmp') -> Arg(i, sbst tmp')
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
      let xs = List.filter_map (function ((x,_), Base (Some p)) when p=p1 -> Some x | _ -> None) templates in
      constr_of_typ @@ _PApp typ2 [make_var @@ List.get xs]
  | Arg _, _ -> assert false
  | _, Arg _ -> assert false
  | Fun(_, (Fun _ as typ11), typ12), Fun(_, (Fun _ as typ21), typ22) ->
      _And (inline_sub templates typ21 typ11) (inline_sub templates typ12 typ22)
  | Fun(x1,typ11,typ12), Fun(x2,typ21,typ22) ->
      let c1 = inline_sub templates typ21 typ11 in
      let c2 = subst_constr pred_var (make_var x2) @@ constr_of_typ typ21 in
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
  | Arg(i, typ) ->
      begin
        match nt typ with
        | Fun(x, typ1, typ2) ->
            if i = 0
            then typ1
            else nt @@ Arg(i-1, PApp(typ2, [make_var x]))
        | Inter typs ->
            Inter (List.map (fun typ -> Arg(i, typ)) typs)
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

let make_sub ct nid templates typ1 typ2 =
  let templates' = List.filter (fst |- snd |- Option.for_all (in_comp_tree ct)) templates in
  if !!debug then Format.printf "  typ1: %a@." print_template typ1;
  if !!debug then Format.printf "  typ2: %a@." print_template typ2;
  if !make_sub_flag then
(*    match typ1 with
    | Var x when not @@ is_fun_typ @@ Id.typ x -> constr_of_typ @@ normalize_type templates' @@ PApp(typ2, [make_var x])
    | PApp(Var x, ts) when not @@ is_fun_typ @@ Id.typ x -> constr_of_typ @@ normalize_type templates' @@ PApp(typ2, make_var x::ts)
    | _ ->*)
    inline_sub templates' (normalize_type templates' typ1) (normalize_type templates' typ2)
  else
    Sub(typ1,typ2)

let filter_assumption env assumption =
  let check constr =
    let fv = get_fv constr in
    let fv' = List.filter_out is_pred_var fv in
    List.Set.subset fv' env
  in
  List.filter check assumption

let rec generate_constraints templates vars var_env assumption (Rose_tree.Node((nid,label), children) as ct) =
  let dbg = 0=1 in
  let open Comp_tree in
  let r =
  let constr vs env asm = List.flatten_map (generate_constraints templates vs env asm) children in
  match label with
  | App((f, kind), vars', map) ->
      let assumption' =
        let aux (acc,env) (x,v) =
          if is_fun_typ @@ Id.typ x
          then acc, env
          else
            let env' = x::env in
            let cmp (x1,id1) (x2,id2) = Id.same x1 x2 && id1 = id2 in
            let asm = constr_of_typ @@ PApp(List.assoc ~cmp (x,None) templates, List.map make_var env') in
            acc @ [asm], env'
        in
        let base_vars = List.filter_out (is_fun_typ -| Id.typ) vars' in
        fst @@ List.fold_left aux (assumption, base_vars) map
      in
      let vars'' = List.rev_map fst map @ vars' in
      let _,var_env' = List.fold_left (fun (vs,env) (x,_) -> x::vs, (x,vs)::env) (vars',var_env) map in
      let constr1 =
        let _,_,constrs =
          let aux (i,env,typs) (_,t) =
            let env' =
              if is_fun_typ t.typ
              then env
              else env @ [t]
            in
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
                PApp(Arg(i, Var f), args)
              in
              if dbg then Format.printf "      env: %a@." (List.print Print.term) env;
              if dbg then Format.printf "      typ1: %a@." print_template typ1;
              if dbg then Format.printf "      typ2: %a@." print_template typ2;
              make_sub ct nid templates typ1 typ2
            in
            if dbg then Format.printf "    constr: %a@."print_constr constr;
            i+1, env', constr::typs
          in
          List.fold_left aux (0,[],[]) map
        in
        let asm = filter_assumption vars assumption in
        if dbg then Format.printf "  asm: %a@." (List.print print_constr) asm;
        if dbg then Format.printf "  assumption: %a@." (List.print print_constr) assumption;
        if dbg then Format.printf "  assumption': %a@." (List.print print_constr) assumption';
        if dbg then Format.printf "  vars: %a@." (List.print Id.print) vars;
        if dbg then Format.printf "  vars': %a@." (List.print Id.print) vars';
        if dbg then Format.printf "  vars'': %a@.@." (List.print Id.print) vars'';
        List.map (_Imply asm) constrs
      in
      let constr2 =
        match kind with
        | Local
        | Thread _ -> constr vars'' var_env' assumption'
        | TopLevel -> List.flatten_map (generate_constraints templates vars'' var_env' assumption') children
      in
      constr1 @ constr2
  | Let(f, vars', t) ->
      if dbg then Format.printf "  vars: %a@." (List.print Id.print) vars;
      if dbg then Format.printf "  vars': %a@.@." (List.print Id.print) vars';
      assert (List.for_all2 Id.same vars vars');
      constr vars ((f,vars')::var_env) assumption
  | Spawn(f, tids) ->
      let asm = filter_assumption vars assumption in
      let app typ = PApp(typ, List.map make_var vars) in
      let constrs1 =
        if true
        then []
        else List.map (fun (g,tid) -> _Imply asm @@ make_sub ct nid templates (app @@ Var f) (app @@ Var g)) tids
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
let generate_constraints xs templates ct =
  let env = List.map (fun x -> x, []) xs in
  let constr = List.map (fun x -> constr_of_typ (normalize_type templates @@ PApp(Var x, [make_var pred_var]))) xs in
  (*constr @*) generate_constraints templates [] env [] ct



(*
let filter_assumption env assumption =
  let vars = List.map fst env in
  let check constr =
    let fv = get_fv constr in
    List.Set.subset fv vars
  in
  List.filter check assumption

let rec generate_constraints templates var_env assumption (Rose_tree.Node(label, children)) =
  let dbg = 0=0 in
  let open Comp_tree in
  let r =
  let constr env asm = List.flatten_map (generate_constraints templates env asm) children in
  match label with
  | App((f, kind), vars', map) ->
      let assumption', var_env' =
        let aux (asm,env,vs) (x,v) =
          let vs' = x::vs in
          let asm' =
            if is_fun_typ @@ Id.typ x
            then asm
            else
              let args = List.filter_map (Option.make (not -| is_fun_typ -| Id.typ) make_var) vs' in
              asm @ [constr_of_typ @@ PApp(Id.assoc x templates, args)]
          in
          asm', (x,vs)::env, vs'
        in
        let asm,env,_ = List.fold_left aux (assumption, var_env, vars') map in
        asm, env
      in
      let constr1 =
        let asm = filter_assumption var_env assumption in
        let _,_,constrs =
          let aux (i,env,typs) (_,t) =
            let env' =
              if is_fun_typ t.typ
              then env
              else env @ [t]
            in
            let constr =
              let args = env @ List.filter_map (fun (x,_) -> if is_fun_typ @@ Id.typ x then None else Some (make_var x)) var_env in
              let typ1 = typ_of t in
              let typ2 = PApp(Arg(i, Var f), args) in
              if dbg then Format.printf "      env: %a@." (List.print Print.term) env;
              if dbg then Format.printf "      typ1: %a@." print_template typ1;
              if dbg then Format.printf "      typ2: %a@." print_template typ2;
              if dbg then Format.printf "      args: %a@." (List.print Print.term) args;
              make_sub templates typ1 typ2
            in
            if dbg then Format.printf "    constr: %a@."print_constr constr;
            i+1, env', constr::typs
          in
          List.fold_left aux (0,[],[]) map
        in
        if dbg then Format.printf "  asm: %a@." (List.print print_constr) asm;
        if dbg then Format.printf "  assumption: %a@." (List.print print_constr) assumption;
        if dbg then Format.printf "  assumption': %a@." (List.print print_constr) assumption';
        if dbg then Format.printf "  vars': %a@." (List.print Id.print) vars';
        if dbg then Format.printf "  var_env': %a@.@." (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env';
        List.map (_Imply asm) constrs
      in
      let constr2 =
        match kind with
        | Local
        | Thread _ -> constr var_env' assumption'
        | TopLevel -> List.flatten_map (generate_constraints templates var_env' assumption') children
      in
      constr1 @ constr2
  | Let(f, env, t) ->
      constr ((f,env)::var_env) assumption
  | Spawn(f, tids) ->
      let asm = filter_assumption var_env assumption in
      let app typ = PApp(typ, List.map (make_var -| fst) var_env) in
      let constrs1 =
        if true
        then []
        else List.map (fun (g,tid) -> _Imply asm @@ make_sub templates (app @@ Var f) (app @@ Var g)) tids
      in
      constrs1 @ constr var_env assumption
  | Assume t ->
      constr var_env @@ Exp t::assumption
  | Fail ->
      assert (children = []);
      let asm = filter_assumption var_env assumption in
      [_Imply asm @@ Exp false_term]
  in
  if dbg then Format.printf "label: %a@." print_label label;
  if dbg then Format.printf "var_env: %a@." (List.print @@ Pair.print Id.print @@ List.print Id.print) var_env;
  if dbg then Format.printf "assumption: %a@." (List.print print_constr) assumption;
  if dbg then Format.printf "r: %a@.@." (List.print print_constr) r;
  r
let generate_constraints templates ct = generate_constraints templates [] [] ct
 *)


(*
let rec make_var_scope var_env (Rose_tree.Node(label, children)) =
  let var_scope env = List.flatten_map (make_var_scope env) children in
  match label with
  | App((f, kind), var_env', map) ->
      var_scope
  | Let(f, var_env', _) -> (f,var_env') :: var_scope var_env
  | Spawn _ -> var_scope var_env
  | Assume _ -> var_scope var_env
  | Fail -> var_scope var_env
let rec make_var_scope ct = make_var_scope [] ct
*)
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
  | App((f, _), var_env, map) ->
      if dbg then Format.printf "APP: %a@." print_label label;
      if dbg then Format.printf "  TEMPLATE: %a@." print_tmp_env templates;
      let aux label' =
        match label' with
        | App(_,_,map) ->
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
        List.map2 (fun (_,typ) (x,_) -> (x,None), typ) xtyps map
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
(*

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


(*
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


 *)
*)

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
      | "test.ml" when 9=9 -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [1;0]] | _ -> assert false)
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
  let templates = make_template xs comp_tree in
  Format.printf "TEMPLATES: @[%a@.@." print_tmp_env templates;
  make_sub_flag := false;
  let constrs = generate_constraints xs templates comp_tree in
  Format.printf "CONSTR1: @[%a@.@." (List.print print_constr) constrs;
  make_sub_flag := true;
  let constrs = generate_constraints xs templates comp_tree in
  Format.printf "CONSTR2: @[%a@.@." (List.print print_constr) constrs;
  let hc = List.flatten_map to_horn_clause constrs in
  Format.printf "HORN CLAUSES: @[%a@.@." print_horn_clauses hc;
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
