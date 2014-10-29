
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open ModelCheck_util

type result = Safe of (var * Inter_type.t) list | Unsafe of (((int list) list) * (((Fpat.Idnt.t * Fpat.Pred.t list) list) list))

let debug () = List.mem "ModelCheck" !Flag.debug_module

let check_aux prog top_funs =
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
  let labels = (List.map Id.to_string top_funs) @ (List.map (fun n -> "r"^string_of_int n) [1;2;3;4]) in
  let spec = make_spec labels in
  let arity_map = make_arity_map labels in
  try
    match model_check_aux (prog',arity_map,spec) with
      | ModelCheck_util.Safe(x) -> Safe(x)
      | ModelCheck_util.Unsafe(x,y) -> Unsafe(x,y)
  with
  | End_of_file -> (Format.printf "\nTRecS failed@."; assert false)

let check_aux_cps prog top_funs =
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
  let labels = (List.map Id.to_string top_funs) @ (List.map (fun n -> "r"^string_of_int n) [1;2;3;4]) in (* TODO: count the number of randnum *)
  let spec = make_spec labels in
  let arity_map = make_arity_map labels in
  try
    match model_check_aux (prog',arity_map,spec) with
      | ModelCheck_util.Safe(x) -> Safe(x)
      | ModelCheck_util.Unsafe(x,y) -> Unsafe(x,y)
  with End_of_file -> fatal "TRecS failed"

let check abst prog top_funs =
  let tmp = get_time () in
  if !Flag.print_progress
  then Color.printf Color.Green "(%d-2) Checking HORS ... @?" !Flag.cegar_loop;
  let result =
    match !Flag.model_check with
    | Flag.ModelCheckCPS ->
        if not @@ List.mem Flag.CPS !Flag.form then failwith "Program must be in CPS @ ModelCheckCPS";
        check_aux_cps abst top_funs
    | Flag.ModelCheck -> check_aux abst top_funs
  in
  add_time tmp Flag.time_mc;
  if !Flag.print_progress then Color.printf Color.Green "DONE!@.@.";
  result
