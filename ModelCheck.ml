
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util

type result = Safe of (var * Inter_type.t) list | Unsafe of int list

let debug () = List.mem "ModelCheck" !Flag.debug_module

let pr s t =
  if debug ()
  then Format.printf "##[ModelCheck] %s:@.%a@.@." s CEGAR_print.prog_typ t;
  ignore @@ Typing.infer t

let check_non_cps prog n =
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
  let spec = make_spec n in
  try
    model_check (prog',spec)
  with
  | Assert_failure(s,_,_) as e when s <> "" -> raise e
  | End_of_file -> (Format.printf "\nTRecS failed@."; assert false)

let check_cps prog n =
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
  let spec = make_spec n in
  try
    model_check (prog',spec)
  with End_of_file -> fatal "TRecS failed"

let check abst prog =
  let n = List.length prog.defs in
  let tmp = get_time () in
  if !Flag.print_progress
  then Color.printf Color.Green "(%d-2) Checking HORS ... @?" !Flag.cegar_loop;
  let result =
    if List.mem ACPS prog.attr
    then check_cps abst n
    else check_non_cps abst n
  in
  add_time tmp Flag.time_mc;
  if !Flag.print_progress then Color.printf Color.Green "DONE!@.@.";
  match result with
  | ModelCheck_util.Safe env -> Safe env
  | ModelCheck_util.Unsafe ce -> Unsafe ce
