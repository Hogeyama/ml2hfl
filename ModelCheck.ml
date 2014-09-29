
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util

type result = Safe of (var * Inter_type.t) list | Unsafe of int list

let debug () = List.mem "ModelCheck" !Flag.debug_module

let check_aux prog n =
  Format.printf "WARNING: model checking for non-CPS programs is unmaintained.@.";
  let prog' =
    prog
    |> Fun.flip CEGAR_CPS.trans true
    |@debug()&> Format.printf "CPS:@.%a@." CEGAR_print.prog_typ
    |> eta_expand
    |> elim_non_det
    |> make_bottom
    |> pop_main
    |> capitalize
  in
  let spec = make_spec n in
  try
    model_check_aux (prog',spec)
  with
  | Assert_failure(s,_,_) as e when s <> "" -> raise e
  | End_of_file -> (Format.printf "\nTRecS failed@."; assert false)

let check_aux_cps prog n =
  let prog' =
    prog
    |> eta_expand
    |@debug()&> Format.printf "ETA_EXPAND:@.%a@." CEGAR_print.prog_typ
    |> elim_non_det
    |@debug()&> Format.printf "ELIM_NON_DET:@.%a@." CEGAR_print.prog_typ
    |> put_into_if
    |@debug()&> Format.printf "PUT_INTO_IF:@.%a@." CEGAR_print.prog_typ
    |> make_bottom
    |@debug()&> Format.printf "MAKE_BOTTOM:@.%a@." CEGAR_print.prog_typ
    |> pop_main
    |@debug()&> Format.printf "POP_MAIN:@.%a@." CEGAR_print.prog_typ
    |> capitalize
    |@debug()&> Format.printf "CAPITALIZE:@.%a@." CEGAR_print.prog_typ
  in
  let spec = make_spec n in
  try
    model_check_aux (prog',spec)
  with End_of_file -> fatal "TRecS failed"

let check abst prog =
  let n = List.length prog.defs in
  let tmp = get_time () in
  if !Flag.print_progress
  then Color.printf Color.Green "(%d-2) Checking HORS ... @?" !Flag.cegar_loop;
  let result =
    match !Flag.model_check with
    | Flag.ModelCheckCPS ->
        if not @@ List.mem Flag.CPS !Flag.form then failwith "Program must be in CPS @ ModelCheckCPS";
        check_aux_cps abst n
    | Flag.ModelCheck -> check_aux abst n
  in
  add_time tmp Flag.time_mc;
  if !Flag.print_progress then Color.printf Color.Green "DONE!@.@.";
  match result with
  | ModelCheck_util.Safe env -> Safe env
  | ModelCheck_util.Unsafe ce -> Unsafe ce
