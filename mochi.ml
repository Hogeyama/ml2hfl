open Util
open Mochi_util

let make_temp_file () =
  let dir = "/tmp/mochi" in
  let template = Format.asprintf "%s/%a_XXXXXXXX.ml" dir Time.print_simple !!Unix.time in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
  Unix.CPS.open_process_in ("mktemp " ^ template) input_line
  |@> Verbose.printf "Temporary file \"%s\" is created@.@."

let copy_input_file file =
  let temp_file = !!make_temp_file in
  IO.copy_file ~src:file ~dest:temp_file;
  temp_file

let save_input_to_file filenames =
  match filenames with
  | []
  | ["-"] ->
      let filename = if !Flag.use_temp then !!make_temp_file else "stdin.ml" in
      Flag.Input.filenames := [filename];
      IO.output_file filename (IO.input_all stdin)
  | _ ->
      if !Flag.use_temp then
        filenames
        |> List.map copy_input_file
        |> Ref.set Flag.Input.filenames

let main filenames =
  (* origにはdirectiveと型定義が入っている．今回は無視してよい *)
  let _, parsed = Parser_wrapper.parse_files !Flag.Input.filenames in
  let problem = Problem.safety parsed in
  Main_loop.run Spec.init problem

let () =
  Cmd.parse_arg ();
  save_input_to_file !Flag.Input.filenames;
  main !Flag.Input.filenames

