open Location
open Stypes
open Parsetree

let make loc typ =
  if loc <> Location.none then [loc, typ] else []

let rec process_pat env (p, e) =
  process_expression env e
and process_expopt env = function
    None -> []
  | Some(e) -> process_expression env e
and process_expression env e =
  let loc = e.pexp_loc in
  match e.pexp_desc with
    Pexp_ident(Longident.Lident(x)) ->
				  (try
        make loc (List.assoc x env)
						with Not_found ->
						  [])
  | Pexp_constant _ ->
		    []
  | Pexp_let(_, pats, e) ->
		    Util.concat_map (process_pat env) pats @
		    process_expression env e
  | Pexp_function(_, eopt, pats) ->
		    process_expopt env eopt @
		    Util.concat_map (process_pat env) pats
  | Pexp_apply(e, les) ->
		    process_expression env e @
		    Util.concat_map (fun (_, e) -> process_expression env e) les
  | Pexp_match(e ,pats) ->
		    process_expression env e @
		    Util.concat_map (process_pat env) pats
  | Pexp_try(e, pats) ->
		    process_expression env e @
		    Util.concat_map (process_pat env) pats
  | Pexp_tuple es ->
		    Util.concat_map (process_expression env) es
  | Pexp_construct(_, eopt, _) ->
		    process_expopt env eopt
  | Pexp_variant(_, eopt) ->
		    process_expopt env eopt
  | Pexp_record(fields, eopt) ->
		    Util.concat_map (fun (_, e) -> process_expression env e) fields @
		    process_expopt env eopt
  | Pexp_field(e, _) ->
		    process_expression env e
  | Pexp_setfield(e1, _, e2) ->
		    process_expression env e1 @
		    process_expression env e2
  | Pexp_array(es) ->
		    Util.concat_map (process_expression env) es
  | Pexp_ifthenelse(e1, e2, eopt) ->
		    process_expression env e1 @
		    process_expression env e2 @
		    process_expopt env eopt
  | Pexp_sequence(e1, e2) ->
		    process_expression env e1 @
		    process_expression env e2
  | Pexp_while(e1, e2) ->
		    process_expression env e1 @
		    process_expression env e2
  | Pexp_for(_, e1, e2, _, e3) ->
		    process_expression env e1 @
		    process_expression env e2 @
		    process_expression env e3
  | Pexp_constraint(e, _, _) ->
		    process_expression env e
  | Pexp_when(e1, e2) ->
		    process_expression env e1 @
		    process_expression env e2
  | Pexp_send(e, _) ->
		    process_expression env e
  | Pexp_new(_) ->
		    []
  | Pexp_setinstvar(_, e) ->
		    process_expression env e
  | Pexp_override(ies) ->
		    Util.concat_map (fun (_, e) -> process_expression env e) ies
  | Pexp_letmodule(_, _, e) ->
      raise (Util.NotImplemented "writeAnnot")
  | Pexp_assert(e) ->
		    process_expression env e
  | Pexp_assertfalse ->
		    []
  | Pexp_lazy(e) ->
		    process_expression env e
  | Pexp_poly(e, _) ->
		    process_expression env e
  | Pexp_object(_) ->
		    raise (Util.NotImplemented "writeAnnot")
  | Pexp_newtype(_, e) ->
		    process_expression env e
  | Pexp_pack(_) ->
		    raise (Util.NotImplemented "writeAnnot")
  | Pexp_open(_, e) ->
		    process_expression env e

let process_top_level_phrase env = function
  Ptop_dir _ ->
    []
| Ptop_def struc ->
    let aux si =
      match si.pstr_desc with
		      Pstr_eval e ->
		        process_expression env e
		    | Pstr_value(_, pats) ->
		        Util.concat_map (process_pat env) pats
		    | Pstr_primitive _ -> []
		    | Pstr_type decls -> []
		    | Pstr_exception(x,exc_decl) -> []
		    | Pstr_exn_rebind _ -> []
		    | Pstr_module _ -> []
		    | Pstr_recmodule _ -> []
		    | Pstr_modtype _ -> []
		    | Pstr_open _ -> []
		    | Pstr_class _ -> []
		    | Pstr_class_type _ -> []
		    | Pstr_include _ -> []
    in
    Util.concat_map aux struc

let f filename orig env =
  let filename = Misc.chop_extension_if_any filename ^ ".annot" in
  let oc = Format.formatter_of_out_channel (open_out filename) in
  List.iter
		  (fun (loc, typ) ->
      if loc.loc_start <> Lexing.dummy_pos && loc.loc_end <> Lexing.dummy_pos then
						   let _ = print_position oc loc.loc_start in
									let _ = Format.fprintf oc " " in
									let _ = print_position oc loc.loc_end in
									let _ = Format.fprintf oc "@.type(@.  " in
									let _ = Format.fprintf oc "%a" Ref_type.print typ in
									Format.fprintf oc "@.)@.")
    (Util.concat_map (process_top_level_phrase env) orig)
