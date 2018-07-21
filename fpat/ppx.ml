open Ast_mapper
open Asttypes
open Parsetree
open Longident

let unknown_int_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({txt=s; loc}, pstr)} when s = "q" ->
        begin
          match pstr with
          | PStr [] -> (* no payload *)
            (* Replace [%q] with RCamlConst.unknonw_int "q" *)
            Ast_helper.Exp.apply
              {expr with pexp_desc=Pexp_ident {txt=Ldot(Lident"RCamlConst","unknown_int");loc}}
              [(Nolabel, {expr with pexp_desc=Pexp_constant(Pconst_string(s,None))})]
          | _ ->
            let msg = "Extension nodes for unknown coefficients sholud be without a payload (e.g. [%q])" in
            raise (Location.Error (Location.error ~loc msg))
        end
      | x -> default_mapper.expr mapper x;
  }

let set_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({txt=s; loc}, pstr)} when s = "set" ->
        begin
          match pstr with
          | Parsetree.PStr([{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant Pconst_string(str,None)},_)}]) ->
            (* Replace [%set str] with RCamlConst.set_str str *)
            Ast_helper.Exp.apply
              {expr with pexp_desc=Pexp_ident {txt=Ldot(Lident"RCamlConst","set_str");loc}}
              [(Nolabel, {expr with pexp_desc=Pexp_constant(Pconst_string(str,None))})]
          | _ -> assert false
        end
      | x -> default_mapper.expr mapper x;
  }
