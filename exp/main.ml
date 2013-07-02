open Util
open Manager_util

module JSON = Yojson.Basic

(*
REQUIREMENTS:
 - Run in the directory of MoCHi
 - The directory of MoCHi is the parent directory "../"
 - The directory of MoCHi's wiki is "../wiki/"
 - Git is installed
*)


let () =
  if !Sys.interactive
  then ()
  else
    let cmd = ref (fun () -> ()) in
    let set_cmd f = fun x -> cmd := fun () -> f x in
    let usage = "Test manager for MoCHi\noptions are:" in
    let arg_spec =
      ["-exp", Arg.Int (set_cmd Exp.run),
         "<n>  Run MoCHi with option<n> for all programs";
       "-exp-all", Arg.Unit (set_cmd Exp.run_all),
         " Run MoCHi for all programs with each option";
       "-rm-exp", Arg.String (set_cmd Exp.delete), "<exp>  Remove <exp>";
       "-del-exp", Arg.String (set_cmd Exp.delete), "<exp>  Same as -rm-exp";
       "-list-exp", Arg.Unit (set_cmd Exp.list), " List all experiments";
       "-add", Arg.String (set_cmd Programs.add), "<program>  Add <program>";
       "-rm", Arg.String (set_cmd Programs.delete), "<program>  Remove <program>";
       "-del", Arg.String (set_cmd Programs.delete), "<program>  Same as -rm";
       "-update", Arg.String (set_cmd Programs.update), "<program>  Update <program>";
       "-list", Arg.Unit (set_cmd Programs.list), " List all programs";
       "-add-option", Arg.String (set_cmd Options.add), "<option>  Add <option> to the option list";
       "-list-option", Arg.Unit (set_cmd Options.list), " List all options";
       "-limit", Arg.Set_int Env.limit, "<n>  Set time limit";
       "-f", Arg.Set Env.run_force, " Run even if MoCHi is not committed";
       "-debug", Arg.Set Env.debug, " Debug mode";
       "-ignore-remote", Arg.Set Env.ignore_remote, " Do not push, fetch, and pull"]
    in
    Arg.parse (Arg.align arg_spec) ignore usage;
    pull ();
    !cmd ();
    Pages.update ()
