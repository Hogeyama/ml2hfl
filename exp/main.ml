open Util
open Manager_util
open Markdown

module JSON = Yojson.Basic

let mochi_option = ref ""

(*
REQUIREMENTS:
 - Run in the directory of MoCHi
 - The directory of MoCHi is the parent directory "../"
 - The directory of MoCHi's wiki is "../wiki/"
 - Git is installed
*)


let make_home () =
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
  let ps = [header1; table; header2; list] in
  update_page "Home" ps;
  Format.printf "Home page is updated.@."


let () =
  if !Sys.interactive
  then ()
  else
    let usage = "Test manager for MoCHi\noptions are:" in
    let arg_spec =
      ["-exp", Arg.Int (Exp.run make_home),
         "<n>  Run MoCHi with option <n> for all programs and update exp page";
       "-add", Arg.String Programs.add, "<program>  Create wiki page for <program>";
       "-del", Arg.String Programs.delete, "<program>  Delete <program>";
       "-update", Arg.String Programs.update, "<program>  Update <program>";
       "-list", Arg.Unit Programs.list, " List all programs";
       "-add-option", Arg.String Options.add, "<option>  Add <option> to the option list";
       "-list-option", Arg.Unit Options.list, " List all options"]
    in
    Arg.parse arg_spec (fun _ -> Arg.usage arg_spec usage) usage
