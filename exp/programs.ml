open Util
open Manager_util
open Markdown

let get () =
  if file_exists ~prefix:Env.wiki_dir Env.program_list
  then read_list_from_file Env.program_list
  else []

let list () =
  Format.printf "Programs:@.";
  List.iter (Format.printf "  %s@.") (get ())

let exists filename =
  List.mem filename @@ get ()

let delete filename =
  if not @@ exists filename then fatal "Not exist.";
  write_list_to_file Env.program_list @@ List.filter ((<>) filename) @@ get ();
  Manager_util.add Env.program_list;
  delete_page filename;
  Format.printf "Program \"%s\" is removed.@." filename

let make_contents_from_file filename =
  let s = read_from_file ~prefix:"" filename in
  let body = Code(Some "ocaml", s) in
  let header = Header(1, [Text filename]) in
  [header; body]

let add filename =
  if exists filename then fatal "Already registered.";
  if not @@ file_exists filename then fatal ("Not found: " ^ filename);
  write_list_to_file Env.program_list @@ insert filename @@ get ();
  Manager_util.add Env.program_list;
  update_page (encode_filename filename) @@ make_contents_from_file filename;
  Format.printf "Program \"%s\" is added.@." filename

let update filename =
  if not @@ exists filename then fatal "Not registered.";
  if not @@ file_exists filename then fatal ("Not found: " ^ filename);
  update_page (encode_filename filename) @@ make_contents_from_file filename;
  Format.printf "Program \"%s\" is updated.@." filename
