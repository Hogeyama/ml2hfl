open Util

let command s =
  if !Env.debug then Format.printf "COMMAND: %s@." s;
  let r = Sys.command s in
  if !Env.debug then Format.printf "RETURN:  %d@." r;
  r

let command_assert s =
  if command s <> 0
  then fatal ("Failure: " ^ s)

let open_process_in s =
  if !Env.debug then Format.printf "OPEN_PROCESS: %s@." s;
  Unix.open_process_in s

let close_process_in cin =
  ignore @@ Unix.close_process_in cin

let encode_filename s =
  Str.global_replace (Str.regexp_string "/") "__" s

let dummy_header =
  Markdown.Normal [Markdown.Image("DO NOT EDIT BELOW THIS LINE", Env.dummy_image)]

let dummy_footer =
  Markdown.Normal [Markdown.Image("DO NOT EDIT ABOVE THIS LINE", Env.dummy_image)]

let make_link s =
  Markdown.Link(s, encode_filename s)

let file_exists ?(prefix=Env.wiki_dir) filename =
  Sys.file_exists @@ prefix ^ filename

let read_from_file ?(prefix=Env.wiki_dir) filename =
  let cin = open_in @@ prefix ^ filename in
  let s = ref "" in
  let rec read () : unit =
    s := !s ^ input_line cin ^ "\n";
    read ()
  in
  let r =
    try
      read ();
      assert false
    with End_of_file -> !s
  in
  close_in cin;
  r

let read_list_from_file ?(prefix=Env.wiki_dir) filename =
  let cin = open_in @@ prefix ^ filename in
  let ss_rev = ref [] in
  let rec read () : unit =
    ss_rev := input_line cin :: !ss_rev;
    read ()
  in
  let r =
    try
      read ();
      assert false
    with End_of_file -> List.rev !ss_rev
  in
  close_in cin;
  r

let write_to_file ?(prefix=Env.wiki_dir) filename s =
  let cout = open_out @@ prefix ^ filename in
  output_string cout s;
  close_out cout

let write_list_to_file ?(prefix=Env.wiki_dir) filename ss =
  let cout = open_out @@ prefix ^ filename in
  List.iter (fun s -> output_string cout (s ^ "\n")) ss;
  close_out cout

let append_to_file ?(prefix=Env.wiki_dir) filename s =
  let cout = open_out_gen [Open_creat;Open_append] 0o664 @@ prefix ^ filename in
  output_string cout (s ^ "\n");
  close_out cout

let add filename =
  command_assert @@ Format.sprintf "cd %s && git add %s" Env.wiki_dir filename

let diff_origin () =
  0 <> command (Format.sprintf "cd %s && test $(git diff HEAD origin/HEAD | wc -w) = 0" Env.wiki_dir)

let push () =
  if not !Env.ignore_remote && diff_origin ()
  then command_assert @@ Format.sprintf "cd %s && git push" Env.wiki_dir

let fetch () =
  if not !Env.ignore_remote
  then
    (Format.printf "Fetching ...@.";
     command_assert @@ Format.sprintf "cd %s && git fetch" Env.wiki_dir)

let pull () =
  if not !Env.ignore_remote
  then command_assert @@ Format.sprintf "cd %s && git pull" Env.wiki_dir

let commit msg =
  if 0 = command (Format.sprintf "cd %s && test $(git status -s --untracked-files=no | wc -w) = 0" Env.wiki_dir)
  then ()
  else
    command_assert @@ Format.sprintf "cd %s && git commit -m \"%s\"" Env.wiki_dir msg;
    push ()

let parse_generated lines =
  let s_header = Markdown.string_of_paragraph dummy_header in
  let s_footer = Markdown.string_of_paragraph dummy_footer in
  let aux line (state,(header,contents,footer)) =
    match state with
      `Footer ->
        if line = s_footer
        then `Contents, (header, contents, footer)
        else `Footer, (header, contents, line::footer)
    | `Contents ->
        if line = s_header
        then `Header, (header, contents, footer)
        else `Contents, (header, line::contents, footer)
    | `Header ->
        `Header, (line::header, contents, footer)
  in
  let _,(header,contents,footer) =  List.fold_right aux lines (`Footer, ([],[],[])) in
  let rec trunc_blank ss =
    match ss with
      ""::""::ss' -> trunc_blank @@ ""::ss'
    | _ -> ss
  in
  let header' = header |> trunc_blank |> List.rev |> trunc_blank |> List.rev in
  let contents' = contents |> trunc_blank |> List.rev |> trunc_blank |> List.rev in
  let footer' = footer |> trunc_blank |> List.rev |> trunc_blank |> List.rev in
  header', contents', footer'

let read_page name =
  if file_exists name
  then
    let filename = encode_filename name ^ ".md" in
    let lines = read_list_from_file filename in
    parse_generated lines
  else [],[],[]

let update_page name body =
  let filename = encode_filename name ^ ".md" in
  let header,_,footer = read_page name in
  let to_text s = Markdown.Text (s ^ "\n") in
  let header' = Markdown.Normal (List.map to_text header) in
  let footer' = Markdown.Normal (List.map to_text footer) in
  let body' = header' :: body @ [footer'] in
  let s = Markdown.string_of_paragraphs body' in
  write_to_file filename s;
  command_assert @@ Format.sprintf "cd %s && git add %s" Env.wiki_dir filename;
  commit ("Update " ^ name)

let delete_page name =
  let filename = encode_filename name ^ ".md" in
  command_assert @@ Format.sprintf "cd %s && git rm %s" Env.wiki_dir filename;
  commit ("Remove " ^ name)

let get_commit_hash_aux dir option =
  let cmd = Format.sprintf "cd %s && git rev-parse %s HEAD" dir option in
  let cin = open_process_in cmd in
  let s = input_line cin in
  close_process_in cin;
  s

let get_commit_date dir hash =
  let cmd = Format.sprintf "cd %s && git log --date=iso --pretty=format:\"%%ad\" %s -1" dir hash in
  let cin = open_process_in cmd in
  let s = input_line cin in
  close_process_in cin;
  s

let wiki_commit_hash () =
  get_commit_hash_aux Env.wiki_dir ""

let wiki_commit_hash_short () =
  get_commit_hash_aux Env.wiki_dir "--short"

let mochi_commit_hash () =
  get_commit_hash_aux "./" ""

let mochi_commit_hash_short () =
  get_commit_hash_aux "./" "--short"

let fpat_commit_hash () =
  get_commit_hash_aux Env.fpat_dir ""

let fpat_commit_hash_short () =
  get_commit_hash_aux Env.fpat_dir "--short"

let fpat_version () =
  let hash = fpat_commit_hash_short () in
  Format.sprintf "%s (%s)" hash @@ get_commit_date Env.fpat_dir hash

let csisat_revision () =
  let cmd = Format.sprintf "cd %s && svnversion" Env.csisat_dir in
  let cin = open_process_in cmd in
  let s = input_line cin in
  close_process_in cin;
  s

let mochi_commited () =
  command "test $(git diff | wc -w) = 0" = 0

let read_COMMIT () =
  List.hd @@ read_list_from_file ~prefix:"" "COMMIT"
