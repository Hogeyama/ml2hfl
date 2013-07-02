open Util
open Manager_util
open Markdown

module JSON = Yojson.Basic

let get () =
  if file_exists ~prefix:Env.wiki_dir Env.exp_list
  then read_list_from_file Env.exp_list
  else []

let exists name =
  List.mem name @@ get ()

let commit_filename n =
  let commit = Manager_util.mochi_commit_hash_short () in
  Format.sprintf "option%d.%s" n commit

let commit_filename_json n =
  commit_filename n ^ ".json"

let item_filename n item =
  Format.sprintf "option%d.%s.json" n item

let parse_name s =
  let get_matched n =
    String.sub s (Str.group_beginning n) (Str.group_end n - Str.group_beginning n)
  in
  assert (Str.string_match (Str.regexp "option\\([0-9]+\\)\\.\\([^.]+\\)") s 0);
  int_of_string (get_matched 1), get_matched 2

let get_commit_date name =
  get_commit_date "./" @@ snd @@ parse_name name

let list () =
  Format.printf "Experiments:@.";
  List.iter (fun name -> Format.printf "  %s (%s)@." name (get_commit_date name)) (get ())

let is_option n name =
  n = fst @@ parse_name name

let get_latest n =
  let exps = List.filter (is_option n) @@ get () in
  if exps = []
  then raise Not_found
  else List.hd exps

let mochi_env () =
  let cmd = Format.sprintf "%s -version" Env.mochi in
  let cin = Unix.open_process_in cmd in
  let s_rev = ref [] in
  let rec read () : unit =
    s_rev := input_line cin :: !s_rev;
    read ()
  in
  let env =
    try
      read ();
      assert false
    with End_of_file -> List.rev !s_rev
  in
  ignore @@ Unix.close_process_in cin;
  let check s = Str.string_match (Str.regexp ".*Build\\|.*TRecS\\|.*OCaml") s 0 in
  List.filter check env

let parse_result filename : (string * JSON.json) list list =
  let result = read_list_from_file filename in
  List.iter (fun s -> assert (s.[0] = '{')) result;
  List.map (fun s -> JSON.Util.to_assoc (JSON.from_string s)) result

let assoc name program =
  if exists name
  then
    let results = parse_result @@ name ^ ".json" in
    let check dict =
      try
        `String program = List.assoc "filename" dict
      with Not_found -> assert false
    in
    List.find check results
  else raise Not_found

let run_mochi n programs =
  let filename = commit_filename_json n in
  let wiki_filename = Env.wiki_dir ^ filename in
  let option = Options.assoc n in
  Format.printf "Run: %s %s%s<program>@." Env.mochi option (if option="" then "" else " ");
  Format.printf "Limit: %d sec@." !Env.limit;
  Format.printf "Output to: %s@." wiki_filename;
  assert (command ("echo -n > " ^ wiki_filename) = 0);
  let rec iter programs =
    match programs with
      [] -> ()
    | p::programs' ->
        if not @@ file_exists p then fatal ("Not found: " ^ p);
        let result_num () = List.length @@ read_list_from_file filename in
        let n = result_num () in
        let cmd_limit = Format.sprintf "ulimit -t %d" !Env.limit in
        let cmd_mochi = Format.sprintf "%s -limit %d %s %s" Env.mochi !Env.limit option p in
        let cmd_timeout = Format.sprintf "echo '{\"filename\": %S, \"result\": \"TimeOut\"}'" p in
        let cmd = Format.sprintf "(%s; %s || %s) | tail -1 | tee -a %s"
          cmd_limit cmd_mochi cmd_timeout wiki_filename in
        assert (command cmd = 0);
        if n+1 <> result_num ()
        then (Format.printf "Rerun: %s@." p; iter programs)
        else iter programs'
  in
  iter programs;
  add filename;
  commit @@ Format.sprintf "Add %s" filename

let make_commit_table n assocs =
  let open Markdown in
  let head' = List.map (fun (h,_) -> [Text h]) Env.items in
  let assoc h xs =
    try
      let r = unescape @@ JSON.to_string @@ List.assoc h xs in
      if h = "filename"
      then [make_link r]
      else [Text r]
    with Not_found -> [Text "-"]
  in
  let body = List.map (fun xs -> List.map (fun (h,_) -> assoc h xs) Env.items) assocs in
  let pos = List.map snd Env.items in
  let table = Table(head',pos,body) in
  let env = mochi_env () in
  let option = Options.assoc n in
  let env = Format.sprintf "Option: %s ([option%d](option%d))" option n n :: env in
  let env = Format.sprintf "Limit: %d sec" !Env.limit :: env in
  let env = snoc env @@ Format.sprintf "FPAT version: %s" @@ fpat_commit_hash_short () in
  let env = snoc env @@ Format.sprintf "CSI-sat revision: %s" @@ csisat_revision () in
  let env' = UnorderedList (List.map (fun s -> [Text s]) env) in
  [env'; table]


let make_commit_page n programs =
  let filename = commit_filename n in
  let result = parse_result @@ commit_filename_json n in
  update_page filename @@ make_commit_table n result


let exists_option n =
  0 <> List.length @@ List.filter (is_option n) @@ get ()

let delete name =
  if not @@ exists name then fatal "Not exist.";
  let n,_ = parse_name name in
  write_list_to_file Env.exp_list @@ List.filter ((<>) name) @@ get ();
  Manager_util.add Env.exp_list;
  delete_page name;
  Format.printf "Experiment \"%s\" is removed.@." name;
  Env.updated := n :: !Env.updated

let run n =
  if not !Env.run_force && not @@ Options.exists n
  then
    Format.printf "Unregistered option: %d@." n
  else if not !Env.run_force && not @@ mochi_commited ()
  then
    Format.printf "Please, commit your changes of MoCHi or stash them.@."
  else if not !Env.run_force && Str.string_match (Str.regexp_string (mochi_commit_hash ())) (read_COMMIT ()) 0
  then
    Format.printf "Please, recompile MoCHi.@."
  else
    let name = commit_filename n in
    let programs = Programs.get () in
    run_mochi n programs;
    make_commit_page n programs;
    let cmp name1 name2 = compare (get_commit_date name2) (get_commit_date name1) in
    if not @@ exists name
    then write_list_to_file Env.exp_list @@ insert ~cmp name @@ get ();
    Manager_util.add Env.exp_list;
    Manager_util.commit ("Update " ^ Env.exp_list);
    Env.updated := n :: !Env.updated

let run_all () =
  List.iter run @@ mapi (fun i _ -> i+1) @@ Options.get ()
