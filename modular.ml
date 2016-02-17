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

module RefTypInfer = struct
  open Fpat
  open Util
  open Combinator

  let infer_etrs fs is_cp prog etrs =
    etrs
    |> Fpat.RefTypInfer.infer_etrs fs is_cp prog
    |@> Format.printf "refinement types:@,  %a@," Fpat.RefType.pr_env
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

let infer_ref_type spec ce parsed =
  assert (spec.Spec.ref_env <> []);
  let ref_env = Spec.get_ref_env spec parsed |@ not !Flag.only_result &> Spec.print_ref_env Format.std_formatter in
  let t = Trans.ref_to_assert ref_env parsed in
  Main_loop.init_typ_excep ();
  let prog, rmap, get_rtyp, info = Main_loop.preprocess t spec in
  FpatInterface.init prog;
  let labeled = CEGAR_abst_util.has_branch prog in
  let is_cp = FpatInterface.is_cp prog in
  let inlined_functions = CEGAR_util.inlined_functions info.CEGAR_syntax.orig_fun_list info.CEGAR_syntax.inlined prog in
  let ce' = CEGAR_trans.trans_ce labeled prog ce in
  let prog = FpatInterface.conv_prog prog in
  RefTypInfer.refine prog inlined_functions is_cp [ce'] false [[]]
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

let bool_of_term' t = Option.try_ (fun _ -> bool_of_term t)

let is_fail t =
  match t.desc with
  | App({desc=Event("fail",_)}, [{desc=Const Unit}]) -> true
  | _ -> false

let exists_fail ts = List.exists is_fail ts

let append_path path rs =
  List.map (Triple.map_trd @@ (@) path) rs

exception EvalBottom
exception EvalFail

(* ASSUME: Input must be normal form *)
let rec eval fun_env ce_set ce_env t =
  let r =
  match t.desc with
  | BinOp(And, _, _) -> assert false
  | BinOp(Or, _, _) -> assert false
  | Const _
  | Var _
  | BinOp _
  | Not _
  | Fun _ -> [t, ce_env, []]
  | _ when is_fail t -> [t, ce_env, []]
  | Bottom -> []
  | App(t1, ts) when is_fix t1 ->
      let n = get_arg_num t1 in
      if n < List.length ts then
        [t, ce_env, []]
      else if n > List.length ts then
        unsupported "Modular.eval: App(fix, _)"
      else
        let f,xs,t1' = Option.get @@ decomp_fix t1 in
        let t' = List.fold_right2 subst xs ts t1' in
        eval fun_env ce_set ce_env t'
  | App(t1, ts) ->
      let f = var_of_term t1 in
      let xs = List.map var_of_term ts in
      let ys,t_f = Id.assoc f fun_env in
      assert (List.length xs <= List.length ys);
      if List.length xs < List.length ys
      then [t, ce_env, []]
      else
        let label = new_label () in
        Format.printf "LABEL: %a, %d@." Id.print f label;
        let t_f' = add_label label t_f in
        let paths = List.assoc_all f ce_set in
        let aux path =
          let t' = make_app (make_fix f xs t_f') ts in
          let ce_env' = (label,path)::ce_env in
          eval fun_env ce_set ce_env' t'
        in
        List.flatten_map aux paths
  | If(_, t2, t3) ->
      let aux ce =
        match ce with
        | [] -> assert false
        | br::ce' ->
            let t23 = if br = 0 then t2 else t3 in
            append_path [br] @@ eval fun_env ce_set ce_env t23
      in
      let label = get_label t in
      let ce = List.assoc label ce_env in
      aux ce
  | Let _ when is_fix t -> [t, ce_env, []]
  | Let(flag, [], t2) -> eval fun_env ce_set ce_env t2
  | Let(Nonrecursive, [f,xs,t1], t2) ->
      if xs = []
      then
        let rs = eval fun_env ce_set ce_env t1 in
        let aux (v,ce_env,path) =
          t2
          |> (if is_base_typ t1.typ then Fun.id else subst f v)
          |> eval fun_env ce_set ce_env
          |> append_path path
        in
        List.flatten_map aux rs
      else
        let t2' = subst f (make_funs xs t1) t2 in
        eval fun_env ce_set ce_env t2'
  | _ ->
      Format.printf "%a@." Print.term t;
      unsupported "Modular.eval"
  in Format.printf"ORIG: %a@.RESULT: %a@.@." Print.term t (List.print Print.term) @@ List.map Triple.fst r;r
let expand_counterexamples (prog:program) (ce_set:(id*int list) list) : int list list =
  Format.printf "ce_set: %a@." print_ce_set ce_set;
  [List.flatten_map snd ce_set]

let infer spec parsed (ce_set: (id * int list) list) =
  Format.printf "NORM: %a@." Print.term @@ Trans.flatten_let @@ Trans.normalize_let parsed;
  let fbindings,main = decomp_prog parsed in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (fun (_,bindings) -> List.map (fun (f,xs,t) -> f, (xs,t)) bindings) fbindings in
  let ce_set = List.mapi (fun i (f,_) -> match i with 0 -> f, [0] | 1 -> f, [0;1] | _ -> assert false) fun_env in
  let main = Option.get @@ get_last_definition parsed in
  ignore@@eval fun_env ce_set [] (make_app (make_var main) [make_var @@ Id.new_var ~name:"v0" TInt]);
  let _ = assert false in
  let prog = [] in
  let ces = expand_counterexamples prog ce_set in
  let envs = List.map (infer_ref_type spec -$- parsed) ces in
  List.flatten envs
