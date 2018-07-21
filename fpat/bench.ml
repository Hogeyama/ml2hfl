open Combinator
open Util

(** Utility functions for experiments *)

let result_filename = ref "exp.csv"
let additional_msg = ref ""

let files path =
  Format.printf "path: %s@." path;
  Sys.readdir path
  |> Array.to_list
  |> List.filter
    (fun x -> Str.first_chars x 1 <> ".") (* filter out dot files *)
  |> List.map ((^) path)


(** execute the given command for the file path with timeout constraint,
    and export its result to result_filename *)
let bench_file ?(timeout=100) command path =
  Global.target_filename := path;
  SMTProver.initialize ();
  let oc = open_out_gen [Open_append; Open_creat] 0o666 !result_filename in
  Global.log_fileformatter :=
    Format.make_formatter
      (output oc)
      (fun () -> flush oc);
  let name =
    Filename.chop_extension (Filename.basename !Global.target_filename)
  in
  Timer.start_interval ();
  let _ =
    try
      Timer.block
        ~timeout:timeout
        (fun _ -> ())
        (fun _ -> ())
        (fun () -> command path)
      |> fst
      |> (fun time ->
          Format.fprintf
            !Global.log_fileformatter
            "%s,%f,%s,%s@."
            name time "true" !additional_msg)
    with
    | Timer.Timeout ->
      Format.fprintf
        !Global.log_fileformatter
        "%s,%s,%s,%s@."
        name "TimeOut" "false" !additional_msg
    | e ->
      Format.fprintf
        !Global.log_fileformatter
        "%s,%s,false, %s@."
        name
        "Failure"
        (Printexc.to_string e)
  in
  close_out oc;
  SMTProver.finalize ()

(*
(* @todo we have to reset some mutable variables
   such as type environments *)
let bench ?(recursive=true) command path =
  try
    if Sys.is_directory path then
      begin
        path
        |> files
        |> List.iter (bench_file command)
      end
    else
      begin
        bench_file command path
      end
  with
  | e ->
     SMTProver.finalize ();
     Format.printf "@]@.";
     raise e
*)
