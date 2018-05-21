open Util

(** Interface to OCaml *)

let ocamlin = ref stdin
let ocamlout = ref stdout

let print_string _ = ()
let print_newline _ = ()
let print_char _ = ()

let open_ocaml () =
  let cin, cout = Unix.open_process "ocaml" in
  ocamlin := cin;
  ocamlout := cout;
  print_string (input_line !ocamlin);
  print_newline ();
  print_string (input_line !ocamlin);
  print_newline ()

let send_cmd cmd =
  let command = cmd ^ ";;\n" in
  print_char (input_char !ocamlin);
  print_char (input_char !ocamlin);
  print_string command;
  flush stdout;

  output_string !ocamlout command;
  flush !ocamlout

let recv_cmd () =
  input_line !ocamlin
  (*let buffer_size = 250
  let buf = String.create buffer_size in
  let len = input !ocamlin buf 0 buffer_size in
  let ret = String.sub buf 0 len in
  ret*)

let eval exp =
  send_cmd exp;
  recv_cmd ()

let close_ocaml () =
  send_cmd "#quit";
  match Unix.close_process (!ocamlin, !ocamlout) with
  | Unix.WEXITED(_) | Unix.WSIGNALED(_) | Unix.WSTOPPED(_) -> ()


(** @test simple *)
let test () =
  open_ocaml ();
  let ret = eval "23 + 18" in
  Format.printf "%s@." ret;
  let ret = eval "(fun x -> x) 41" in
  Format.printf "%s@." ret;
  close_ocaml ()
