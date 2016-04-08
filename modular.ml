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



(*
type program = (id * typed_term) list

let print_ce_set fm ce_set =
  let pr fm (f, ce) = Format.fprintf fm "%a: %a" Id.print f (List.print Format.pp_print_int) ce in
  Format.fprintf fm "%a" (print_list pr ",@ ") ce_set

let infer_ref_type spec ces parsed =
  assert (spec.Spec.ref_env <> []);
  let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
  let t = Trans.ref_to_assert ref_env parsed in
  Main_loop.init_typ_excep ();
  let prog, rmap, get_rtyp, info = Main_loop.preprocess t spec in
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  let ces' = List.map (CEGAR_trans.trans_ce labeled prog) ces in
  let prog = FpatInterface.conv_prog prog in
  RefTypInfer.refine prog inlined_functions is_cp ces' false (List.map (Fun.const []) ces)
  |@false&> Format.printf "ENV: @[%a@." Fpat.RefType.pr_env



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

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [_]) -> true
  | _ -> false

let exists_fail ts = List.exists is_fail ts

let append_path path rs =
  List.map (Triple.map_trd @@ (@) path) rs

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
  let rs = eval fun_env ce_set [] (make_app (make_var main) [make_var @@ Id.new_var ~name:"v0" TInt]) in
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
*)



type constr =
  | True
  | False
  | And of constr * constr
  | Imply of typed_term * constr
  | Sub of typ_tmp * typ_tmp
and typ_tmp =
  | Var of id (* Not used *)
  | Arg of int * fun_id
  | App of typ_tmp * typed_term list
  | Singleton of typed_term
  | Const of Ref_type.t
and fun_id = id * (tid * int option) option
and tid = int

let rec print_constr fm = function
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | And(c1, c2) -> Format.fprintf fm "(@[%a@ /\\@ %a@])" print_constr c1 print_constr c2
  | Imply(t, c) -> Format.fprintf fm "(@[%a@ =>@ %a@])" Print.term t print_constr c
  | Sub(typ1, typ2) -> Format.fprintf fm "(@[%a@ <:@ %a@])" print_typ_tmp typ1 print_typ_tmp typ2
and print_typ_tmp fm = function
  | Var f -> Id.print fm f
  | Arg(i, f) -> Format.fprintf fm "#%d(%a)" i print_fun_id f
  | App(typ, ts) -> Format.fprintf fm "%a%a" print_typ_tmp typ (List.print Print.term) ts
  | Singleton t -> Format.fprintf fm "{%a}" Print.term t
  | Const typ -> Format.fprintf fm "%a" Ref_type.print typ
and print_fun_id fm (f, tidn) =
  match tidn with
  | None -> Format.fprintf fm "%a" Id.print f
  | Some (tid, None) -> Format.fprintf fm "%a:%a" Id.print f print_tid tid
  | Some (tid, Some n) -> Format.fprintf fm "%a:%a-%d" Id.print f print_tid tid n
and print_tid = Format.pp_print_int

let _And c1 c2 =
  if c1 = True then
    c2
  else if c2 = True then
    c1
  else
    And(c1, c2)
let _Ands constrs = List.fold_right _And constrs True

let typ_of t =
  match t.desc with
  | Syntax.Var f -> Var f
  | _ -> Singleton t

let rec generate_constraints tid_env (Rose_tree.Node(label, children)) =
  let open Comp_tree in
  let constr () = _Ands @@ List.map (generate_constraints tid_env) children in
  match label with
  | Term t ->
      begin
        match t.desc with
        | If _ -> !!constr
        | App(t1, ts) ->
            let f = try Option.get @@ decomp_var t1 with _ -> Format.printf "%a@." Print.term t1; assert false in
            let tid = Option.try_with (fun () -> get_id t1) ((=) @@ Invalid_argument "get_id") in
            let constr1 =
              let fun_id = f, Option.map (fun tid -> tid, List.assoc_option tid tid_env) tid in
              let _,_,typs = List.fold_left (fun (i,map,typs) t -> i+1, map@[t], typs@[App(Arg(i, fun_id), map)]) (0,[],[]) ts in
              _Ands @@ List.map2 (fun typ t -> Sub(typ_of t, typ)) typs ts
            in
            let constr2 =
              if Option.for_all (List.mem_assoc -$- tid_env) tid then
                !!constr
              else
                let tid = try Option.get tid with _ -> assert false in
                _Ands @@ List.mapi (fun i child -> generate_constraints ((tid,i)::tid_env) child) children
            in
            And(constr1, constr2)
        | _ -> assert false
      end
  | Bind map -> Imply(List.fold_right make_and (List.map (fun (x,v) -> make_eq (make_var x) v) map) true_term, !!constr)
  | Assume t -> Imply(t, !!constr)
  | Fail ->
      assert (children = []);
      False
let generate_constraints comp_tree = generate_constraints [] comp_tree


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
  Format.printf "INPUT: %a@." Print.term normalized;
  let fbindings,main = decomp_prog normalized in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (snd |- List.map Triple.to_pair_r) fbindings in
  let ce_set =
    let aux =
      match !Flag.filename with
      | "test.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test2.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | _ -> assert false
    in
    List.flatten @@ List.map aux fun_env
  in
  let main = Option.get @@ get_last_definition normalized in
  let comp_tree = Comp_tree.from_program fun_env ce_set main in
  Format.printf "%a@." print_constr @@ generate_constraints comp_tree;
  comp_tree
