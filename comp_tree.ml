open Util
open Syntax
open Term_util
open Type

let debug () = List.mem "Comp_tree" !Flag.debug_module

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
let get_label t = get_id t

let bool_of_term' t = Option.try_ (fun _ -> bool_of_term t)

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let exists_fail ts = List.exists is_fail ts

let append_path path rs =
  List.map (Triple.map_trd @@ (@) path) rs

let rec from_term fun_env ce_set ce_env t =
  Format.printf"@[ORIG(%d): %a@\n  @[" (List.length ce_set) Print.term t;
  let r =
  match t.desc with
  | BinOp(And, _, _) -> assert false
  | BinOp(Or, _, _) -> assert false
  | Const _
  | Var _
  | BinOp _
  | Not _
  | Fun _
  | Event _ -> [t, ce_env, []]
  | _ when is_fail t -> [t, ce_env, []]
  | Bottom -> []
  | App(t1, ts) when is_fix t1 ->
      let n = get_arg_num t1 in
      if n < List.length ts then
        [t, ce_env, []]
      else if n > List.length ts then
        unsupported "Comp_tree.from_term: App(fix, _)"
      else
        let f,xs,t1' = Option.get @@ decomp_fix t1 in
        let t' = List.fold_right2 subst xs ts t1' in
        from_term fun_env ce_set ce_env t'
  | App(t1, ts) ->
      let f = var_of_term t1 in
      let ys,t_f = Id.assoc f fun_env in
      assert (List.length ts <= List.length ys);
      if List.length ts < List.length ys
      then [t, ce_env, []]
      else
        let label = new_label () in
        Format.printf "LABEL: %a, %d@\n  @[" Id.print f label;
        let t_f' = add_label label t_f in
        let paths = List.assoc_all ~cmp:Id.eq f ce_set in
        let aux path =
          Format.printf "PATH: %d, %a@\n" label (List.print Format.pp_print_int) path;
          let t' = make_app (make_fix f ys t_f') ts in
          let ce_env' = (label,path)::ce_env in
          from_term fun_env ce_set ce_env' t'
        in
        let r = List.flatten_map aux paths in
        Format.printf "@]";
        r
  | If(_, t2, t3) ->
      let label = get_label t in
      let ce,ce_env' = List.decomp_assoc label ce_env in
      begin
        match ce with
        | [] -> []
        | br::ce' ->
            Format.printf "CE[%d]: %a@\n" label (List.print Format.pp_print_int) ce;
            let t23 = if br = 0 then t2 else t3 in
            let ce_env'' = (label,ce')::ce_env' in
            append_path [br] @@ from_term fun_env ce_set ce_env'' t23
      end
  | Let _ when is_fix t -> [t, ce_env, []]
  | Let(flag, [], t2) -> from_term fun_env ce_set ce_env t2
  | Let(Nonrecursive, [f,xs,t1], t2) ->
      if xs = []
      then
        let rs = from_term fun_env ce_set ce_env t1 in
        let aux (v,ce_env,path) =
          if is_fail v then
            [fail_unit_term, ce_env, path]
          else
            t2
            |> (if is_base_typ t1.typ then Fun.id else subst f v)
            |> from_term fun_env ce_set ce_env
            |> append_path path
        in
        List.flatten_map aux rs
      else
        let t2' = subst f (make_funs xs t1) t2 in
        from_term fun_env ce_set ce_env t2'
  | _ ->
      Format.printf "%a@." Print.term t;
      unsupported "Comp_tree.from_term"
  in
  Format.printf"@]@\nRESULT: %a@]@," (List.print (Pair.print Print.term (List.print Format.pp_print_int))) @@ List.map (fun (x,y,z) -> x,z) r;r

let from_term fun_env ce_set t = from_term fun_env ce_set [] t



let infer spec parsed (ce_set: (id * int list) list) =
  let normalized = Trans.inline_var @@ Trans.flatten_let @@ Trans.normalize_let parsed in
  Format.printf "NORM: %a@." Print.term normalized;
  let fbindings,main = decomp_prog normalized in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map (fun (f,xs,t) -> f, (xs,t)) bindings) fbindings in
  let ce_set = List.flatten @@ List.mapi (fun i (f,_) -> match i with 0 -> [(*f, [0];*) f, [1]] | 1 -> [f, [0;1](*; f, [1;0;1]*)] | _ -> assert false) fun_env in
  Format.printf "CE_SET: %a@." (List.print @@ Pair.print Id.print (List.print Format.pp_print_int)) ce_set;
  let main = Option.get @@ get_last_definition normalized in
  let rs = from_term fun_env ce_set (make_app (make_var main) [make_var @@ Id.new_var ~name:"v0" TInt]) in
  let rs' = List.filter (is_fail -| Triple.fst) rs in
  Format.printf "@.Counterexamples: %a@." (List.print (List.print Format.pp_print_int)) @@ List.map (Triple.trd) rs';
  let ces = List.map (Triple.trd) rs' in
  let env = infer_ref_type spec ces normalized in
  let env' = List.filter ((Id.mem_assoc -$- fun_env) -| CEGAR_trans.trans_inv_var -| Fpat.Idnt.string_of -| fst) env in
  assert (env'<>[]);
  Format.printf "@.refinement types:@,  %a@," Fpat.RefType.pr_env env';(*
  let aenv = infer_abs_type spec ces normalized in*)
  assert false;
  env'
