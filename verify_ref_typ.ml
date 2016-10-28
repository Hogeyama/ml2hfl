open Util
open Syntax
open Term_util
open Type
open Modular_syntax

module Debug = Debug.Make(struct let check = make_debug_check "Verify_ref_typ" end)

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
  Debug.printf "PROGRAM: %a@." Print.term t;
  Debug.printf "ORIG: %a@." (List.print Print.id) @@ get_top_funs t;
  let ext = List.map fst ref_env in
  let t_main = remove_ext_def ext t in
  Debug.printf "MAIN: %a@." (List.print Print.id) @@ get_top_funs t_main;
  let make_spec f =
    let ref_env,ext_ref_env = List.partition (Id.eq f -| fst) ref_env in
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
    Debug.printf "SUB[%a]: %a@." Print.id f Spec.print spec';
    spec'
  in
  let targets = List.map (fun f -> Id.to_string f, make_spec f, t) ext in
  Debug.printf "MAIN: %a@." Print.term t_main;
  ("MAIN", make_spec (Id.new_var ~name:"MAIN" TUnit), t_main)::targets


let main orig spec parsed =
  let verify (s,spec,t) =
    Debug.printf "Start verification of %s:@.%a@." s Spec.print spec;
    s, Main_loop.run orig [] ~spec t
  in
  Spec.get_ref_env spec parsed
  |@(not !Flag.only_result)&> Spec.print_ref_env Format.std_formatter
  |> divide spec parsed
  |> List.map verify
  |@> Format.printf "RESULT: %a@." (List.print @@ Pair.print Format.pp_print_string Format.pp_print_bool)
  |> List.for_all snd
