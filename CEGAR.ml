
open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type result = Safe of (var * CEGAR_ref_type.t) list | Unsafe of int list

let pre () =
  ()

let post () =
  incr Flag.cegar_loop;
  Fpat.Global.cegar_iterations := !Flag.cegar_loop





let print_non_CPS_abst abst prog =
  let result =
    try
      Some (ModelCheck.check abst prog)
    with _ -> None
  in
  if !Flag.just_print_non_CPS_abst then
    let s =
      match result with
      | None -> "Unknown"
      | Some (ModelCheck.Safe _) -> "Safe"
      | Some (ModelCheck.Unsafe _) -> "Unsafe"
    in
    Format.printf "@.ABST:@.%a@." CEGAR_print.prog abst;
    Format.printf "RESULT: %s@." s;
    exit 0

let rec loop prog0 is_cp info =
  pre ();
  let prog =
    if !Flag.relative_complete
    then
      let env,defs,main = FpatInterface.instantiate_param (prog0.env,prog0.defs,prog0.main) in
      {env; defs; main; attr=[]}
    else prog0
  in
  let pr =
    if !Flag.expand_nonrec
    then CEGAR_util.print_prog_typ' info.orig_fun_list info.inlined
    else CEGAR_print.prog_typ
  in
  if !Flag.print_progress
  then Format.printf "Program with abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop pr prog;
  if !Flag.print_abst_typ
  then Format.printf "Abstraction types (CEGAR-cycle %d)::@.%a@." !Flag.cegar_loop CEGAR_print.env prog.env;
  let labeled,abst = CEGAR_abst.abstract info.orig_fun_list info.inlined prog in
  print_non_CPS_abst abst prog;
  let result = ModelCheck.check abst prog in
  match result with
  | ModelCheck.Safe env ->
      if Flag.print_ref_typ_debug
      then
        begin
          Format.printf "Intersection types:@.";
          List.iter (fun (f,typ) -> Format.printf "  %s: %a@." f Inter_type.print typ) env;
          Format.printf "@."
        end;
      let aux (x,ityp) =
        try
          [x, Type_trans.ref_of_inter (List.assoc x prog.env) ityp]
        with Not_found -> []
      in
      let env' = List.rev_map_flatten aux env in
      post ();
      prog, Safe env'
  | ModelCheck.Unsafe ce ->
      let prog' = CEGAR_non_term.cegar prog0 labeled info is_cp ce prog in
      post ();
      loop prog' is_cp info



let cegar prog info =
  let add_fail_to_end ds =
    match !Flag.mode with
    | Flag.NonTermination ->
        List.map (fun (f, args, cond, e, t) -> if t=Const(CPS_result) then (f, args, cond, [Event "fail"], t) else (f, args, cond, e, t)) ds
    | _ -> ds
  in
  let prog = {prog with defs=add_fail_to_end prog.defs} in
  make_ID_map prog;
  try
    let is_cp = FpatInterface.is_cp prog in
    loop prog is_cp info
  with NoProgress | CEGAR_abst.NotRefined ->
    post ();
    raise NoProgress
