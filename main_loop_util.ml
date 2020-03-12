open Util
open Mochi_util


(* let make_result *)
(*       ?(make_get_rtyp = fun _ _ -> unsupported "Main_loop_util.make_result") *)
(*       ?set_main *)
(*       ?main *)
(*       result *)
(*       stats *)
(*       preprocessed *)
(*   = *)
(*   let {Problem.info} = Preprocess.last_problem preprocessed in *)
(*   {result; stats; make_get_rtyp; set_main; main; info} *)
(*  *)
(* let add_to_log info = *)
(*   Flag.Log.cegar_loop := info.cycles + !Flag.Log.cegar_loop; *)
(*   Flag.Log.Time.abstraction := info.abst +. !Flag.Log.Time.abstraction; *)
(*   Flag.Log.Time.mc := info.mc +. !Flag.Log.Time.mc; *)
(*   Flag.Log.Time.cegar := info.refine +. !Flag.Log.Time.cegar; *)
