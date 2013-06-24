open Util
open Manager_util
open Markdown

module JSON = Yojson.Basic

let make_option_table exps (item,p) =
  let head = [Text "program"] :: List.map (fun exp -> [Text (snd @@ Exp.parse_name exp)]) exps in
  let pos = Left :: List.map (fun _ -> p) exps in
  let make_cell program exp =
    let s =
      try
        Exp.assoc exp program
        |> List.assoc item
        |> JSON.to_string
        |> unescape
      with Not_found -> "-"
    in
    [Text s]
  in
  let make_line program =
    [make_link program] :: List.map (make_cell program) exps
  in
  let body = List.map make_line @@ Programs.get () in
  let header = Header(2, [Text item]) in
  [header; Table(head, pos, body)]

let update_option n =
  let header = Header(1, [Text (Format.sprintf "Option%d: %s" n @@ Options.assoc n)]) in
  let exps = List.filter (Exp.is_option_n n) @@ Exp.get () in
  let tables = List.flatten @@ List.map (make_option_table exps) @@ List.tl Env.items in
  let ps = TOC :: header :: tables in
  let name = "option" ^ string_of_int n in
  update_page name ps;
  Format.printf "Page %s is updated.@." name



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
    Link(Format.sprintf "option%d: %s @@ %s" i s commit, exp)
  in
  let head = List.map make_head exps in
  let head' = List.map (fun t -> [t]) @@ (Text "program" :: head) in
  let pos = List.map (fun _ -> Left) head' in
  let make_cell p exp =
    let s =
      try
        Exp.assoc exp p |> List.assoc "result"
                        |> JSON.to_string
                        |> unescape
      with Not_found -> "-"
    in
    [Text s]
  in
  let make_line program =
    [make_link program] :: List.map (make_cell program) exps
  in
  let body = List.map make_line @@ Programs.get () in
  let table = Table(head', pos, body) in
  let header_result = Header(1, [Text "Latest experimental results"]) in
  let make_opt_list i s =
    let name = Format.sprintf "option%d" (i+1) in
    [Link(Format.sprintf "%s: %s" name s, name)]
  in
  let opt_list = UnorderedList (mapi make_opt_list @@ Options.get ()) in
  let header_opt_list = Header(1, [Text "List of options"]) in
  let exp_list = UnorderedList (List.map (fun e -> [make_head e]) @@ Exp.get ()) in
  let header_exp_list = Header(1, [Text "List of experimental results"]) in
  let ps = [header_result; table;
            header_opt_list; opt_list;
            header_exp_list; exp_list]
  in
  update_page "Home" ps;
  Format.printf "Home page is updated.@."

let update () =
  if !Env.updated <> [] then update_home ();
  List.iter update_option !Env.updated;
  Env.updated := []
