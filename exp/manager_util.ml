open Util

let debug = false

let command s =
  if debug then Format.printf "COMMAND: %s@." s;
  let r = Sys.command s in
  if debug then Format.printf "RETURN:  %d@." r;
  r

let encode_filename s =
  Str.global_replace (Str.regexp_string "/") "__" s

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
  flush cout;
  close_out cout

let write_list_to_file ?(prefix=Env.wiki_dir) filename ss =
  let cout = open_out @@ prefix ^ filename in
  List.iter (fun s -> output_string cout (s ^ "\n")) ss;
  flush cout;
  close_out cout

let append_to_file ?(prefix=Env.wiki_dir) filename s =
  let cout = open_out_gen [Open_creat;Open_append] 0o664 @@ prefix ^ filename in
  output_string cout (s ^ "\n");
  flush cout;
  close_out cout

let add filename =
  let cmd = Format.sprintf "cd %s && git add %s" Env.wiki_dir filename in
  let r = command cmd in
  if r <> 0 then fatal ("Failure: " ^ cmd)

let push () =
  if 0 = command (Format.sprintf "cd %s && test $(git diff HEAD origin/HEAD | wc -w) = 0" Env.wiki_dir)
  then ()
  else
    let cmd = Format.sprintf "cd %s && git push" Env.wiki_dir in
    let r = command cmd in
    if r <> 0 then fatal ("Failure: " ^ cmd)

let commit msg =
  if 0 = command (Format.sprintf "cd %s && test $(git status -s | wc -w) = 0" Env.wiki_dir)
  then ()
  else
    let cmd = Format.sprintf "cd %s && git commit -m \"%s\"" Env.wiki_dir msg in
    let r = command cmd in
    if r <> 0 then fatal ("Failure: " ^ cmd);
    push ()

let update_page name body =
  let name' = encode_filename name in
  let filename = name' ^ ".md" in
  let s = Markdown.string_of_paragraphs body in
  write_to_file filename s;
  let cmd = Format.sprintf "cd %s && git add %s" Env.wiki_dir filename in
  let r = command cmd in
  if r <> 0 then fatal ("Failure: " ^ cmd);
  commit ("Update " ^ name)

let delete_page name =
  let name' = encode_filename name in
  let filename = name' ^ ".md" in
  let cmd = Format.sprintf "cd %s && git rm %s" Env.wiki_dir filename in
  let r = command cmd in
  if r <> 0 then fatal ("Failure: " ^ cmd);
  commit ("Remove " ^ name)

let get_commit_hash_aux cd option =
  let cmd = Format.sprintf "%s git rev-parse %s HEAD" cd option in
  let cin = Unix.open_process_in cmd in
  let s = input_line cin in
  ignore @@ Unix.close_process_in cin;
  s

let wiki_commit_hash () =
  get_commit_hash_aux (Format.sprintf "cd %s &&" Env.wiki_dir) ""

let wiki_commit_hash_short () =
  get_commit_hash_aux (Format.sprintf "cd %s &&" Env.wiki_dir) "--short"

let mochi_commit_hash () =
  get_commit_hash_aux "" ""

let mochi_commit_hash_short () =
  get_commit_hash_aux "" "--short"

let trunc_quote s =
  let n = String.length s in
  if n < 2 || s.[0] <> '"' || s.[n-1] <> '"' then raise (Invalid_argument "trunc_quote");
  String.sub s 1 (n-2)

let mochi_commited () =
  command "test $(git diff | wc -w) = 0" = 0

let read_COMMIT () =
  List.hd @@ read_list_from_file ~prefix:"" "COMMIT"

let make_link s =
  Markdown.Link(s, encode_filename s)
