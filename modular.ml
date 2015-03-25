open Util
open Syntax
open Term_util
open Type

let debug () = List.mem "Modular" !Flag.debug_module

let replace_ext = make_trans2 ()

let replace_ext_desc ext desc =
  match desc with
  | Let(flag, bindings, t) ->
      let bindings' = List.filter_out (fun (f,_,_) -> Id.mem f ext) bindings in
      let t' = replace_ext.tr2_term ext t in
      if bindings' = [] then t'.desc else Let(flag, bindings', t')
  | _ -> replace_ext.tr2_desc_rec ext desc

let () = replace_ext.tr2_desc <- replace_ext_desc
let replace_ext = replace_ext.tr2_term

let divide t spec =
  Format.printf "PROGRAM: %a@." Print.term t;
  Format.printf "ORIG: %a@." (List.print Print.id) @@ get_top_funs t;
  let ext = List.map fst spec.Spec.ref_env in
  let t_main = replace_ext ext t in
  Format.printf "MAIN: %a@." (List.print Print.id) @@ get_top_funs t_main;
  let make_spec f =
    let ref_env,ext_ref_env = List.partition (Id.same f -| fst) spec.Spec.ref_env in
    let spec' = {spec with Spec.ref_env; Spec.ext_ref_env} in
    Format.printf "SUB[%a]: %a@." Print.id f Spec.print spec';
    spec'
  in
  let targets = List.map (fun f -> Id.to_string f, make_spec f, t) ext in
  Format.printf "MAIN: %a@." Print.term t_main;
  ("MAIN", make_spec (Id.new_var ~name:"MAIN" TUnit), t_main)::targets


let main orig spec parsed =
  let spec' = Spec.rename spec parsed |@(not !Flag.only_result)&> Spec.print Format.std_formatter in
  let targets = divide parsed spec' in
  let verify (s,spec,t) = s, Main_loop.run orig ~spec t in
  let b = List.for_all (verify |- snd) targets in
  Format.printf "RESULT: %b@." b;b
