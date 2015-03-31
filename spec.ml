
open Util
open Syntax
open Term_util

type spec =
    {ref_env: Ref_type.env;
     ext_ref_env: Ref_type.env;
     abst_env: env;
     abst_cps_env: env;
     abst_cegar_env: env;
     inlined: id list;
     inlined_f: id list}

let init =
  {ref_env = [];
   ext_ref_env = [];
   abst_env = [];
   abst_cps_env = [];
   abst_cegar_env = [];
   inlined = [];
   inlined_f = []}

let pr_ref_env ppf (x,typ) = Format.fprintf ppf "%a: %a" Print.id x (Color.blue Ref_type.print) typ
let pr_env ppf (x,typ) = Format.fprintf ppf "%a: %a" Print.id x (Color.blue Print.typ) typ

let print_aux ppf s pr env =
  if env <> [] then
    begin
      Color.fprintf ppf Color.Red "@[spec (%s):@\n @[" s;
      List.iter (Format.fprintf ppf "@[%a@]@\n" pr) env;
      Format.fprintf ppf "@]@]@\n"
    end

let print_ref_env ppf renv = print_aux ppf "refinement type assertions" pr_ref_env renv
let print_ext_ref_env ppf eenv = print_aux ppf "refinement type assumptions" pr_ref_env eenv
let print_abst_env ppf aenv = print_aux ppf "abstraction type environment" pr_env aenv
let print_abst_cps_env ppf cpsenv = print_aux ppf "abstraction type environment for CPS transformed program" pr_env cpsenv
let print_abst_cegar_env ppf cegarenv = print_aux ppf "abstraction type environment for CEGAR-loop" pr_env cegarenv
let print_inlined ppf inlined = print_aux ppf "inlined functions" Print.id inlined
let print_inlined_f ppf inlined_f = print_aux ppf "force inlined functions" Print.id inlined_f

let print ppf {ref_env=renv; ext_ref_env=eenv; abst_env=aenv; abst_cps_env=cpsenv; abst_cegar_env=cegarenv; inlined; inlined_f} =
  print_ref_env ppf renv;
  print_ext_ref_env ppf eenv;
  print_abst_env ppf aenv;
  print_abst_cps_env ppf cpsenv;
  print_abst_cegar_env ppf cegarenv;
  print_inlined ppf inlined;
  print_inlined_f ppf inlined_f

let parse parser lexer filename =
  if filename = ""
  then init
  else
    let lb = Lexing.from_channel @@ open_in filename in
    lb.Lexing.lex_curr_p <-
      {Lexing.pos_fname = Format.sprintf "File \"%s\"" @@ Filename.basename filename;
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
    lb.Lexing.lex_curr_p <-
      {Lexing.pos_fname = Format.sprintf "Spec in file \"%s\"" @@ Filename.basename filename;
       Lexing.pos_lnum = 0;
       Lexing.pos_cnum = 0;
       Lexing.pos_bol = 0};
  parser lexer lb


let merge spec1 spec2 =
  {ref_env = spec1.ref_env @ spec2.ref_env;
   ext_ref_env = spec1.ext_ref_env @ spec2.ext_ref_env;
   abst_env = spec1.abst_env @ spec2.abst_env;
   abst_cps_env = spec1.abst_cps_env @ spec2.abst_cps_env;
   abst_cegar_env = spec1.abst_cegar_env @ spec2.abst_cegar_env;
   inlined = spec1.inlined @ spec2.inlined;
   inlined_f = spec1.inlined_f @ spec2.inlined_f}


let get_def_vars = make_col2 [] (@@@)

let get_def_vars_term vars t =
  match t.desc with
  | Let(flag, defs, t2) ->
      let xs = List.map Triple.fst defs in
      let xs' = List.filter_out (fun x -> List.exists (fun y -> Id.name x = Id.name y) vars) xs in
      let vars1 = List.rev_flatten_map (fun (_,ys,t) -> get_def_vars.col2_term (ys@@@vars) t) defs in
      let vars2 = get_def_vars.col2_term (xs'@@@vars) t2 in
      xs'@@@vars1@@@vars2
  | _ -> get_def_vars.col2_term_rec vars t

let () = get_def_vars.col2_term <- get_def_vars_term
let get_def_vars = get_def_vars.col2_term []


exception My_not_found of id

type kind =
  | Ref_env
  | Ext_ref_env
  | Abst_env
  | Abst_cps_env
  | Abst_cegar_env
  | Inlined
  | Inlined_f

let rename ks {ref_env; ext_ref_env; abst_env; abst_cps_env; abst_cegar_env; inlined; inlined_f} t =
  let vars = get_def_vars t @ get_fv t in
  let rename_id f = (* temporal implementation *)
    List.find_eq_on Id.name f vars
  in
  let aux_ref (f,typ) =
    try
      let f' = rename_id f in
      if not @@ Type.same_shape (Id.typ f') (Ref_type.to_simple typ) then
        begin
          Format.printf "VAR: %a@." Id.print f;
          Format.printf "  Prog: %a@." Print.typ @@ Id.typ f';
          Format.printf "  Spec: %a@." Ref_type.print typ;
          fatal @@ Format.sprintf "The simple type of %s in the specification is wrong?" @@ Id.name f'
        end;
      Some (f', typ)
    with Not_found -> None
  in
  let aux_typ (f,typ) = Option.try_with (fun () -> rename_id f, typ) Not_found in
  let aux_id f = Option.try_with (fun () -> rename_id f) Not_found in
  {ref_env = if List.mem Ref_env ks then List.filter_map aux_ref ref_env else ref_env;
   ext_ref_env = if List.mem Ext_ref_env ks then List.filter_map aux_ref ext_ref_env else ext_ref_env;
   abst_env = if List.mem Abst_env ks then List.filter_map aux_typ abst_env else abst_env;
   abst_cps_env = if List.mem Abst_cps_env ks then List.filter_map aux_typ abst_cps_env else abst_cps_env;
   abst_cegar_env = if List.mem Abst_cegar_env ks then List.filter_map aux_typ abst_cegar_env else abst_cegar_env;
   inlined = if List.mem Inlined ks then List.filter_map aux_id inlined else inlined;
   inlined_f = if List.mem Inlined_f ks then List.filter_map aux_id inlined_f else inlined_f}

let get_ref_env spec t = (rename [Ref_env] spec t).ref_env
let get_ext_ref_env spec t = (rename [Ext_ref_env] spec t).ext_ref_env
let get_abst_env spec t = (rename [Abst_env] spec t).abst_env
let get_abst_cps_env spec t = (rename [Abst_cps_env] spec t).abst_cps_env
let get_abst_cegar_env spec t = (rename [Abst_cegar_env] spec t).abst_cegar_env
let get_inlined spec t = (rename [Inlined] spec t).inlined
let get_inlined_f spec t = (rename [Inlined_f] spec t).inlined_f




let read parser lexer =
  let spec1 =
    begin
      if !Flag.use_spec && !Flag.spec_file = ""
      then
        let spec = Filename.change_extension !Flag.filename "spec" in
        if Sys.file_exists spec then Flag.spec_file := spec
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
