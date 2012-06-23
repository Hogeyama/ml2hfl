
open Utilities
open Syntax

type spec = {abst_env : (id * typ) list; inlined : id list}

let init = {abst_env=[]; inlined=[]}

let print {abst_env=aenv; inlined=inlined} =
  if aenv <> []
  then
    begin
      Format.printf "spec (abstraction type environment)::@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Syntax.print_id x Syntax.print_typ typ) aenv;
      Format.printf "@."
    end;
  if inlined <> []
  then
    begin
      Format.printf "spec (inlined functions)::@. @[";
      List.iter (Format.printf "@[%a@]@\n" Syntax.print_id) inlined;
      Format.printf "@."
    end

let parse parser lexer filename =
  if filename = ""
  then init
  else
    let lb = Lexing.from_channel (open_in filename) in
      lb.Lexing.lex_curr_p <-
        {Lexing.pos_fname = Filename.basename filename;
         Lexing.pos_lnum = 1;
         Lexing.pos_cnum = 0;
         Lexing.pos_bol = 0};
      parser lexer lb


let rename {abst_env=aenv; inlined=inlined} t =
  let funs = get_top_funs t in
  let rename_id f =
    try
      List.find (fun f' -> Id.name f = Id.name f') funs
    with Not_found -> raise (Fatal ("SPEC: " ^ Id.name f ^ " not found"))
  in
  let aenv' = List.map (fun (f,typ) -> rename_id f, typ) aenv in
  let inlined' = List.map rename_id inlined in
    {abst_env=aenv'; inlined=inlined'}
