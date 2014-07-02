
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util


let debug () = List.mem "ModelCheck_CPS" !Flag.debug_module


let check prog n =
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
