open Util
open Manager_util
open Markdown

let get () =
  if file_exists Env.option_list
  then read_list_from_file Env.option_list
  else []

let num () = List.length @@ get ()
let exists n = num () >= n
let assoc n = List.nth (get ()) (n-1)

let add option =
  append_to_file Env.option_list option;
  Manager_util.add Env.option_list;
  commit (Format.sprintf "Add option \"%s\"" option);
  Format.printf "Option \"%s\" is added.@." option

let delete n =
  if not @@ exists n then fatal (Format.sprintf "Option %d does not exist" n);
  let options = get () in
  let options1,options2 = take2 options (n-1) in
  let options' = options1 @ List.tl options2 in
  let option = List.hd options2 in
  write_list_to_file Env.option_list options';
  Manager_util.add Env.option_list;
  commit (Format.sprintf "Remove option \"%s\"" option);
  Format.printf "Option %d is added.@." n

let list () =
  let options = get () in
  Format.printf "Options:@.";
  iteri (fun i s -> Format.printf "  %d: %s@." (i+1) s) options
