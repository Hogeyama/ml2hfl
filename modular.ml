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
  let inlined_functions = CEGAR_util.inlined_functions info.orig_fun_list info.inlined prog in
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

let get_arg_num = List.length -| fst -| decomp_funs


let counter = Counter.create ()
let new_label () = Counter.gen counter

let rec eval fun_env ce_set ce_env t =
  match t.desc with
  | Const _ -> t, ce_env, [[]]
  | Var x ->
      assert (List.mem_assoc x fun_env);
      t, ce_env, [[]]
  | Fun _ -> t, ce_env, [[]]
  | App(t1, ts) ->
      let f = match t1.desc with Var f -> f | _ -> assert false in
      let t1', ce_env', path = eval fun_env ce_set ce_env t1 in
      let ts', ce_env'', path' =
        let aux t (ts,ce_env,path) =
          let t',ce_env',path' = eval fun_env ce_set ce_env t in
          t'::ts, ce_env', path@path'
        in
        List.fold_right aux ts ([],ce_env,[])
      in
      let label = new_label () in
  | If(t1, t2, t3) ->
      let _, ce_env1, path1 = eval fun_env ce_set ce_env t1 in
      let ce,ce_env1' = List.assoc_map (get_id t) List.tl ce_env1 in
      let t23 =
        if List.hd ce = 0
        then t2
        else t3
      in
      let r, ce_env23, path23 = eval fun_env ce_set ce_env1' t2 in
      r, ce_env23, path1@path23
  | Let _ when is_fix t -> t, ce_env, [[]]
  | Let(flag, bindings, t) ->
      let sbst, ce_env', path =
        let aux (sbst, ce_env, path) (f, xs, t1) =
          let t1' = make_funs xs @@ sbst t1 in
          let r,ce_env',path' = eval fun_env ce_set ce_env t1' in
          subst f r -| sbst, ce_env', path@path'
        in
        List.fold_left aux (Fun.id, ce_env, []) bindings
      in
      let r, ce_env'', path' = eval fun_env ce_set ce_env' @@ sbst t in
      r, ce_env'', path@path'
  | BinOp(And, t1, t2) ->
      let r1, ce_env', path = eval fun_env ce_set ce_env t1 in
      if not @@ bool_of_term r1
      then false_term, ce_env', path
      else eval fun_env ce_set ce_env' t2 |> Triple.map_trd ((@) path)
  | BinOp(Or, t1, t2) ->
      let r1, ce_env', path = eval fun_env ce_set ce_env t1 in
      if bool_of_term r1
      then true_term, ce_env', path
      else eval fun_env ce_set ce_env' t2 |> Triple.map_trd ((@) path)
  | BinOp(op, t1, t2) ->
      let r1, ce_env1, path1 = eval fun_env ce_set ce_env t1 in
      let r2, ce_env2, path2 = eval fun_env ce_set ce_env t2 in
      let n1 = int_of_term r1 in
      let n2 = int_of_term r2 in
      let v =
        match op with
        | Eq -> make_bool (n1 = n2)
        | Lt -> make_bool (n1 < n2)
        | Gt -> make_bool (n1 > n2)
        | Leq -> make_bool (n1 <= n2)
        | Geq -> make_bool (n1 >= n2)
        | Add -> make_int (n1 + n2)
        | Sub -> make_int (n1 - n2)
        | Mult -> make_int (n1 * n2)
        | _ -> assert false
      in
      v, ce_env2, path1@path2
  | Not t ->
      let r, ce_env', path = eval fun_env ce_set ce_env t in
      make_not r, ce_env', path

let expand_counterexamples (prog:program) (ce_set:(id*int list) list) : int list list =
  Format.printf "ce_set: %a@." print_ce_set ce_set;
  [List.flatten_map snd ce_set]

let infer spec parsed (ce_set: (id * int list) list) =
  let prog = [] in
  let ces = expand_counterexamples prog ce_set in
  let envs = List.map (infer_ref_type spec -$- parsed) ces in
  List.flatten envs
