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
    |> List.map (Pair.map_snd Fpat.AbsType.of_refinement_type)
(*
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
                PredVarApp.make
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

let infer_ref_type spec (ces: (id * int list) list) = ces

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
  let _,prog' = Refine.refine inlined_functions is_cp [] [ce'] [[]] prog in
(*
  let env = Main_loop.trans_env rmap get_rtyp prog'.env in
 *)
  let prog_fpat = FpatInterface.conv_prog prog in
(*
  let env = RefTypInfer.refine rmap get_rtyp prog'.env in
 *)
  Format.printf "%a@." CEGAR_print.prog_typ prog'
