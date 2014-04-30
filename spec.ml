
open Util
open Syntax
open Term_util

type spec = {abst_env: (id * typ) list; abst_cps_env: (id * typ) list; inlined: id list; inlined_f: id list}

let init = {abst_env=[]; abst_cps_env=[]; inlined=[]; inlined_f=[]}

let print {abst_env=aenv; abst_cps_env=cpsenv; inlined=inlined; inlined_f=inlined_f} =
  if aenv <> []
  then
    begin
      Color.printf Color.Red "spec (abstraction type environment):@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Syntax.print_id x (Color.blue Syntax.print_typ) typ) aenv;
      Format.printf "@."
    end;
  if cpsenv <> []
  then
    begin
      Color.printf Color.Red "spec (abstraction type environment for CPS transformed program):@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Syntax.print_id x (Color.blue Syntax.print_typ) typ) cpsenv;
      Format.printf "@."
    end;
  if inlined <> []
  then
    begin
      Color.printf Color.Red "spec (inlined functions):@. @[";
      List.iter (Format.printf "@[%a@]@\n" Syntax.print_id) inlined;
      Format.printf "@."
    end;
  if inlined_f <> []
  then
    begin
      Color.printf Color.Red "spec (force inlined functions):@. @[";
      List.iter (Format.printf "@[%a@]@\n" Syntax.print_id) inlined_f;
      Format.printf "@."
    end

let parse parser lexer filename =
  if filename = ""
  then init
  else
    let lb = Lexing.from_channel @@ open_in filename in
    lb.Lexing.lex_curr_p <-
      {Lexing.pos_fname = Filename.basename filename;
       Lexing.pos_lnum = 1;
       Lexing.pos_cnum = 0;
       Lexing.pos_bol = 0};
    parser lexer lb


let parse_comment parser lexer filename =
  let cin = open_in filename in
  let rec loop flag str =
    let s = try Some (input_line cin) with End_of_file -> None in
    match s with
      None -> str
    | Some s when Str.string_match (Str.regexp "(\\*{SPEC}") s 0 -> loop true str
    | Some s when Str.string_match (Str.regexp "{SPEC}\\*)") s 0 -> loop false str
    | Some s when flag -> loop true (str ^ "\n" ^ s)
    | _ -> loop false str
  in
  parser lexer @@ Lexing.from_string @@ loop false ""


let merge spec1 spec2 =
  {abst_env = spec1.abst_env @ spec2.abst_env;
   abst_cps_env = spec1.abst_cps_env @ spec2.abst_cps_env;
   inlined = spec1.inlined @ spec2.inlined;
   inlined_f = spec1.inlined_f @ spec2.inlined_f}


let rename {abst_env=aenv; abst_cps_env=cpsenv; inlined=inlined; inlined_f=inlined_f} t =
  let funs = get_top_funs t in
  let rename_id f = (* temporal implementation *)
    List.find (fun f' -> Id.name f = Id.name f') funs
  in
  let aenv' = flatten_map (fun (f,typ) -> try [rename_id f, typ] with Not_found -> []) aenv in
  let cpsenv' = flatten_map (fun (f,typ) -> try [rename_id f, typ] with Not_found -> []) cpsenv in
  let inlined' = flatten_map (fun f -> try [rename_id f] with Not_found -> []) inlined in
  let inlined_f' = flatten_map (fun f -> try [rename_id f] with Not_found -> []) inlined_f in
    {abst_env=aenv'; abst_cps_env=cpsenv'; inlined=inlined'; inlined_f=inlined_f'}
