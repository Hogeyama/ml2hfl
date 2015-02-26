
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util

type result = Safe of (var * Inter_type.t) list | Unsafe of (((int list) list) * (((int * (bool list)) list) list))

let debug () = List.mem "ModelCheck" !Flag.debug_module

let pr s t =
  if debug ()
  then Format.printf "##[ModelCheck] %s:@.%a@.@." s CEGAR_print.prog_typ t;
  ignore @@ Typing.infer t

let check_aux prog labels =
  Format.printf "WARNING: model checking for non-CPS programs is unmaintained.@.";
  let prog' =
    prog
    |> Fun.flip CEGAR_CPS.trans true
    |@> pr "CPS"
    |> eta_expand
    |@> pr "eta_expand"
    |> elim_non_det
    |@> pr "elim_non_det"
    |> make_bottom
    |@> pr "make_bottom"
    |> pop_main
    |@> pr "pop_main"
    |> capitalize
  in
  let spec = make_spec labels in
  let arity_map = make_arity_map labels in
  try
    match model_check_aux (prog',arity_map,spec) with
    | ModelCheck_util.Safe(x) -> Safe(x)
    | ModelCheck_util.Unsafe(x,y) -> Unsafe(x,y)
  with
  | End_of_file -> (Format.printf "\nTRecS failed@."; assert false)

let check_aux_cps prog labels =
  let prog' =
    prog
    |> eta_expand
    |@> pr "ETA_EXPAND"
    |> elim_non_det
    |@> pr "ELIM_NON_DET"
    |> put_into_if
    |@> pr "PUT_INTO_IF"
    |> make_bottom
    |@> pr "MAKE_BOTTOM"
    |> pop_main
    |@> pr "POP_MAIN"
    |> capitalize
    |@> pr "CAPITALIZE"
  in
  let spec = make_spec labels in
  let arity_map = make_arity_map labels in
  try
    match model_check_aux (prog',arity_map,spec) with
      | ModelCheck_util.Safe(x) -> Safe(x)
      | ModelCheck_util.Unsafe(x,y) -> Unsafe(x,y)
  with End_of_file -> fatal "TRecS failed"

let check abst prog =
  let tmp = get_time () in
  if !Flag.print_progress
  then Color.printf Color.Green "(%d-2) Checking HORS ... @?" !Flag.cegar_loop;
  let labels = (List.filter_map (fun (r,_) -> Option.map make_randint_label @@ decomp_randint_name r) prog.env) in
  let result =
    if List.mem ACPS prog.attr
    then check_aux_cps abst labels
    else check_aux abst labels
  in
  add_time tmp Flag.time_mc;
  if !Flag.print_progress then Color.printf Color.Green "DONE!@.@.";
  result
