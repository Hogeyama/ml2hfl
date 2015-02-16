
open Util
open Syntax
open Term_util

type spec =
    {ref_env: (id * Ref_type.t) list;
     abst_env: (id * typ) list;
     abst_cps_env: (id * typ) list;
     abst_cegar_env: (id * typ) list;
     inlined: id list;
     inlined_f: id list}

let init =
  {ref_env = [];
   abst_env = [];
   abst_cps_env = [];
   abst_cegar_env = [];
   inlined = [];
   inlined_f = []}

let print {abst_env=aenv; abst_cps_env=cpsenv; abst_cegar_env=cegarenv; inlined=inlined; inlined_f=inlined_f} =
  if aenv <> []
  then
    begin
      Color.printf Color.Red "spec (abstraction type environment):@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Print.id x (Color.blue Print.typ) typ) aenv;
      Format.printf "@."
    end;
  if cpsenv <> []
  then
    begin
      Color.printf Color.Red "spec (abstraction type environment for CPS transformed program):@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Print.id x (Color.blue Print.typ) typ) cpsenv;
      Format.printf "@."
    end;
  if cegarenv <> []
  then
    begin
      Color.printf Color.Red "spec (abstraction type environment for CEGAR-loop):@. @[";
      List.iter (fun (x,typ) -> Format.printf "@[%a: %a@]@\n" Print.id x (Color.blue Print.typ) typ) cegarenv;
      Format.printf "@."
    end;
  if inlined <> []
  then
    begin
      Color.printf Color.Red "spec (inlined functions):@. @[";
      List.iter (Format.printf "@[%a@]@\n" Print.id) inlined;
      Format.printf "@."
    end;
  if inlined_f <> []
  then
    begin
      Color.printf Color.Red "spec (force inlined functions):@. @[";
      List.iter (Format.printf "@[%a@]@\n" Print.id) inlined_f;
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
    | Some s when ExtString.String.starts_with s "(*{SPEC}" -> loop true str
    | Some s when ExtString.String.ends_with s "{SPEC}*)" -> loop false str
    | Some s when flag -> loop true (str ^ "\n" ^ s)
    | _ -> loop false str
  in
  let lb = Lexing.from_string @@ loop false "" in
  parser lexer lb


let merge spec1 spec2 =
  {ref_env = spec1.ref_env @ spec2.ref_env;
   abst_env = spec1.abst_env @ spec2.abst_env;
   abst_cps_env = spec1.abst_cps_env @ spec2.abst_cps_env;
   abst_cegar_env = spec1.abst_cegar_env @ spec2.abst_cegar_env;
   inlined = spec1.inlined @ spec2.inlined;
   inlined_f = spec1.inlined_f @ spec2.inlined_f}


let get_def_vars = make_col2 [] (@@@)

let get_def_vars_term vars t =
  match t.desc with
  | Let(flag, defs, t2) ->
      let xs = List.map fst3 defs in
      let xs' = List.filter (fun x -> not @@ List.exists (fun y -> Id.name x = Id.name y) vars) xs in
      let vars1 = List.rev_flatten_map (fun (_,ys,t) -> get_def_vars.col2_term (ys@@@vars) t) defs in
      let vars2 = get_def_vars.col2_term (xs'@@@vars) t2 in
      xs'@@@vars1@@@vars2
  | _ -> get_def_vars.col2_term_rec vars t

let () = get_def_vars.col2_term <- get_def_vars_term
let get_def_vars = get_def_vars.col2_term []


exception My_not_found of id

let rename {ref_env; abst_env; abst_cps_env; abst_cegar_env; inlined; inlined_f} t =
  let vars = get_def_vars t in
  let rename_id f = (* temporal implementation *)
    List.find (fun f' -> Id.name f = Id.name f') vars
  in
  let aux1 (f,typ) = try [rename_id f, typ] with Not_found -> Format.printf "%a@." Id.print f; [] in
  let aux2 f = try [rename_id f] with Not_found -> [] in
  let ref_env' = List.flatten_map aux1 ref_env in
  let abst_env' = List.flatten_map aux1 abst_env in
  let abst_cps_env' = List.flatten_map aux1 abst_cps_env in
  let abst_cegar_env' = List.flatten_map aux1 abst_cegar_env in
  let inlined' = List.flatten_map aux2 inlined in
  let inlined_f' = List.flatten_map aux2 inlined_f in
  {ref_env = ref_env';
   abst_env = abst_env';
   abst_cps_env = abst_cps_env';
   abst_cegar_env = abst_cegar_env';
   inlined = inlined';
   inlined_f = inlined_f'}


let read parser lexer =
  let spec1 =
    begin
      if !Flag.use_spec && !Flag.spec_file = ""
      then
        try
          let spec = Filename.chop_extension !Flag.filename ^ ".spec" in
          if Sys.file_exists spec then Flag.spec_file := spec
        with Invalid_argument "Filename.chop_extension" -> ()
    end;
    parse parser lexer !Flag.spec_file
  in
  let spec2 =
    if !Flag.comment_spec && Sys.file_exists !Flag.filename
    then parse_comment parser lexer !Flag.filename
    else init
  in
  if spec2 <> init then Flag.use_filter := true;
  merge spec1 spec2
