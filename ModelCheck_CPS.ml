
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util



let check prog n =
  let prog = eta_expand prog in
  let () = if false then Format.printf "ETA_EXPAND:@.%a@." CEGAR_print.print_prog prog in
  let prog = elim_non_det prog in
  let () = if false then Format.printf "ELIM_NON_DET:@.%a@." CEGAR_print.print_prog prog in
  let prog = put_into_if prog in
  let () = if false then Format.printf "PUT_INTO_IF:@.%a@." CEGAR_print.print_prog prog in
  let prog = make_bottom prog in
  let () = if false then Format.printf "MAKE_BOTTOM:@.%a@." CEGAR_print.print_prog prog in
  let prog = pop_main prog in
  let () = if false then Format.printf "POP_MAIN:@.%a@." CEGAR_print.print_prog prog in
  let prog = capitalize prog in
  let () = if false then Format.printf "CAPITALIZE:@.%a@." CEGAR_print.print_prog prog in
  let spec = make_spec n in
    try
      model_check_aux (prog,spec)
    with End_of_file -> raise (Fatal "TRecS failed")
