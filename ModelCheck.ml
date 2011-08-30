
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util



let check prog n =
  let prog = CEGAR_CPS.trans' prog true in
  let () = if true then Format.printf "CPS:\n%a@." CEGAR_print.print_prog prog in
  let prog = eta_expand prog in
  let prog = elim_non_det prog in
  let prog = make_bottom prog in
  let prog = pop_main prog in
  let prog = capitalize prog in
  let spec = make_spec n in
    try
      model_check_aux (prog,spec)
    with
        Assert_failure(s,_,_) as e when s <> "" -> raise e
      | End_of_file -> (Format.printf "\nTRecS failed@."; assert false)











