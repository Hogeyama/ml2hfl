
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util

type result = Safe of (var * Inter_type.t) list | Unsafe of int list

let check_aux prog n =
  Format.printf "Warning: model checking for non-CPS programs is unmaintained.@.";
  let prog = CEGAR_CPS.trans prog true in
  let () = if true then Format.printf "CPS:\n%a@." CEGAR_print.prog prog in
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

let check abst prog =
  let n = List.length prog.defs in
  let tmp = get_time() in
  let () =
    if !Flag.print_progress
    then Format.printf "(%d-2) Checking HORS ... @?" !Flag.cegar_loop
  in
  let result =
    match !Flag.model_check with
        Flag.ModelCheckCPS ->
          if not (List.mem Flag.CPS !Flag.form)
          then failwith "Program must be in CPS @ ModelCheckCPS";
          ModelCheck_CPS.check abst n
      | Flag.ModelCheck -> check_aux abst n
  in
  let () = add_time tmp Flag.time_mc in
  let () = if !Flag.print_progress then Format.printf "DONE!@.@." in
    match result with
        ModelCheck_util.Safe env -> Safe env
      | ModelCheck_util.Unsafe ce -> Unsafe ce
