open Util
open Mochi_util
open Main_loop_util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

exception QuitWithUnsafe

let result_of i problems =
  let input,_,_,_ = List.assoc i problems in
  let file = Filename.change_extension input "json" in
  let of_json json =
    let open JSON.Util in
    let result =
      match json |> member "result" |> to_string with
      | "Safe" -> CEGAR.Safe []
      | "Unsafe" when !Flag.Encode.used_abst <> [] -> CEGAR.Unknown (Format.asprintf "because of abstraction options %a" Print.(list string) !Flag.Encode.used_abst)
      | "Unsafe" -> CEGAR.Unsafe([], ModelCheck.CESafety [])
      | r when !Flag.Parallel.continue -> CEGAR.Unknown r
      | r -> failwith r
    in
    let cycles = json |> member "cycles" |> to_int in
    let total = json |> member "total" |> to_float in
    let abst = json |> member "abst" |> to_float in
    let mc = json |> member "mc" |> to_float in
    let refine = json |> member "refine" |> to_float in
    Flag.Log.cegar_loop := cycles + !Flag.Log.cegar_loop;
    Flag.Log.Time.abstraction := abst +. !Flag.Log.Time.abstraction;
    Flag.Log.Time.mc := mc +. !Flag.Log.Time.mc;
    Flag.Log.Time.cegar := refine +. !Flag.Log.Time.cegar;
    result, Some {cycles; total; abst; mc; refine}
  in
  JSON.load file of_json

let print_status (i,(_,status,_,_)) =
  let s = BatPervasives.input_file status in
  let f,st =
    try
      let f,st = String.split s ~by:"," in
      float_of_string f, st
    with _ -> -1., s
  in
  if f < 0. then
    Verbose.printf "%d: %-40s@." i st
  else
    let len = 40 in
    let l = int_of_float (0.5 +. f *. float_of_int len) in
    let l' = min l len in
    let s1 = String.make l' '#' in
    let s2 =
      let finished = List.exists (String.starts_with st) ["Done: ";"Error: "] in
      String.make (len - l') (if finished then '#' else ' ')
    in
    Verbose.printf "%d: [%s%s]  %-40s@." i s1 s2 st

let rec make_wait isatty num problems finished =
  let rec wait running =
    let pid,st = Unix.(waitpid [WNOHANG] (-1)) in
    if isatty then List.iter print_status problems;
    if isatty then Verbose.printf "%a" Cursor.up_begin num;
    if pid = 0 then
      wait running
    else
      let i = List.assoc pid running in
      let r = result_of i problems in
      let is_safe = match r with CEGAR.Safe _, _ -> true | _ -> false in
      finished := (i,r) :: !finished;
      if not (is_safe || !Flag.Parallel.continue) then raise QuitWithUnsafe;
      pid, st
  in
  wait

let make_problem i preprocessed =
  let input = Filename.change_extension !!Flag.Input.main @@ Format.sprintf "%d.bin" i in
  let status = Filename.change_extension input "status" in
  let cmd = Format.sprintf "%s -s -limit %d %s" Sys.argv.(0) !Flag.Parallel.time input in
  i, (input, status, cmd, preprocessed)

let prepare (_,(input,status,_,preprocessed)) =
  let args = !Flag.Log.args in
  let bin = {args; preprocessed} in
  Marshal.to_file ~flag:[Marshal.Closures] input bin;
  IO.empty_file status

let check ?fun_list ?(exparam_sol=[]) spec pps =
  let isatty = Unix.isatty Unix.stdout in
  let finished = ref [] in
  let num = List.length pps in

  if !Flag.Print.progress
  then Color.printf Color.Green "Verifying sub-problems in parallel ... @?";
  if exparam_sol <> [] then unsupported "Parallel.check";
  if spec.Spec.abst_cegar_env <> [] then unsupported "Parallel.check";

  let problems = List.mapi make_problem pps in
  List.iter prepare problems;
  let wait = make_wait isatty num problems finished in
  if isatty then Verbose.printf "%t" Cursor.hide;
  Exception.finally
    (fun () -> if isatty then Verbose.printf "%t%a" Cursor.show Cursor.down num)
    (Unix.parallel ~wait !Flag.Parallel.num)
    (List.map (fun (_,(_,_,cmd,_)) -> cmd) problems);
  if !Flag.Print.progress then Color.printf Color.Green "DONE!@.@.";

  let result_of (i,(input,status,cmd,preprocessed)) =
    let result,stats = List.assoc_default (CEGAR.Unknown "", None) i !finished in
    let make_get_rtyp _ _ = unsupported "Parallel.check" in
    let set_main = None in
    let main = None in
    let {Problem.info} = Preprocess.last_problem preprocessed in
    {result; stats; make_get_rtyp; set_main; main; info}
  in
  List.map result_of problems
