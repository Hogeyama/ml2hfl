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



type constr = True | False | And of constr * constr | Imply of typed_term * constr | Sub of typ * typ

let rec generate_constraints comp_tree =
  let (Rose_tree.Node(label, children)) = comp_tree in
  let constr = List.fold_right (fun ct constr -> And(generate_constraints ct, constr)) children True in
  let open Comp_tree in
  match label with
  | Term t ->
      begin
        match t.desc with
        | If _ -> constr
        | App _ ->
            let subtrees =
              assert false
            in
            subtrees
        | _ -> assert false
      end
  | Arg map -> Imply(List.fold_right make_and (List.map (fun (x,v) -> make_eq (make_var x) v) map) true_term, constr)
  | Assume t -> Imply(t, constr)
  | Fail ->
      assert (children = []);
      False

let infer spec parsed ce_set =
  let normalized =
    if true
    then parsed
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
  comp_tree
