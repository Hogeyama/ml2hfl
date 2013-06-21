open Util
open Manager_util
open Markdown

let get () =
  if file_exists Env.program_list
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
  [dummy_header; header; body; dummy_footer]

let add filename =
  if exists filename then fatal "Already registered.";
  if not @@ file_exists ~prefix:"" filename then fatal ("Not found: " ^ filename);
  let ps = make_contents_from_file filename in
  write_list_to_file Env.program_list @@ insert filename @@ get ();
  Manager_util.add Env.program_list;
  update_page (encode_filename filename) ps;
  Format.printf "Program \"%s\" is added.@." filename

let update filename =
  if not @@ exists filename then fatal "Not registered.";
  if not @@ file_exists ~prefix:"" filename then fatal ("Not found: " ^ filename);
  let ps = make_contents_from_file filename in
  let header,_,footer = read_page filename in
  let to_text s = Text (s ^ "\n") in
  let header' = Normal (List.map to_text header) in
  let footer' = Normal (List.map to_text footer) in
  let ps' = header' :: ps @ [footer'] in
  update_page (encode_filename filename) ps';
  Format.printf "Program \"%s\" is updated.@." filename
