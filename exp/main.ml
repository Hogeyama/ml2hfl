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

(*
TODO:
 - To be able to write a comment on generated pages
*)


let update_home () =
  let open Markdown in
  let exps = Options.get ()
             |> mapi (fun i s -> if Exp.exists_option (i+1) then [i+1,s] else [])
             |> List.flatten
             |> List.map (fun (i,_) -> Exp.get_latest i)
  in
  let make_head exp =
    let i,commit = Exp.parse_name exp in
    let s = Options.assoc i in
    Link(Format.sprintf "option%d: \"%s\" @@ %s" i s commit, exp)
  in
  let head = List.map make_head exps in
  let head' = List.map (fun t -> [t]) @@ (Text "program" :: head) in
  let pos = Left :: List.map (fun _ -> Center) head in
  let make_cell p exp =
    let s =
      try
        Exp.assoc exp p |> List.assoc "result"
                        |> JSON.to_string
                        |> trunc_quote
      with Not_found -> "-"
    in
    [Text s]
  in
  let make_line program =
    [make_link program] :: List.map (make_cell program) exps
  in
  let body = List.map make_line @@ Programs.get () in
  let table = Table(head', pos, body) in
  let header1 = Header(1, [Text "Latest experimental results"]) in
  let list = UnorderedList (List.map (fun e -> [make_head e]) @@ Exp.get ()) in
  let header2 = Header(1, [Text "List of experimental results"]) in
  let ps = [dummy_header; header1; table; header2; list; dummy_footer] in
  update_page "Home" ps;
  Format.printf "Home page is updated.@."


let () =
  if !Sys.interactive
  then ()
  else
    let cmd = ref (fun () -> ()) in
    let set_cmd f = fun x -> cmd := fun () -> f x in
    let usage = "Test manager for MoCHi\noptions are:" in
    let arg_spec =
      ["-exp", Arg.Int (set_cmd @@ Exp.run update_home),
         "<n>  Run MoCHi with option<n> for all programs";
       "-exp-all", Arg.Unit (set_cmd @@ Exp.run_all update_home),
         " Run MoCHi for all programs with each option";
       "-add", Arg.String (set_cmd Programs.add), "<program>  Create wiki page for <program>";
       "-del", Arg.String (set_cmd Programs.delete), "<program>  Delete <program>";
       "-update", Arg.String (set_cmd Programs.update), "<program>  Update <program>";
       "-list", Arg.Unit (set_cmd Programs.list), " List all programs";
       "-add-option", Arg.String (set_cmd Options.add), "<option>  Add <option> to the option list";
       "-list-option", Arg.Unit (set_cmd Options.list), " List all options";
       "-limit", Arg.Set_int Env.limit, "<n>  Set time limit";
       "-f", Arg.Set Env.run_force, "";
       "-debug", Arg.Set Env.debug, " Debug mode"]
    in
    Arg.parse (Arg.align arg_spec) ignore usage;
    fetch ();
    if diff_origin () then pull ();
    !cmd ()
