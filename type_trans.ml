open Util
module RT = CEGAR_ref_type
module IT = Inter_type
module AT = CEGAR_type
module CS = CEGAR_syntax

let ref_base_of_abs_base = function
  | AT.TUnit -> RT.Unit
  | AT.TInt -> RT.Int
  | AT.TBool -> RT.Bool
  | AT.TList -> assert false
  | AT.TTuple -> assert false
  | AT.TAbst s -> RT.Abst s


let rec ref_of_inter env cond atyp ityp =
  match atyp,ityp with
  | _, IT.Inter ityps ->
      let rtyps = List.map (ref_of_inter env cond atyp) ityps in
      RT.Inter(atyp, rtyps)
  | AT.TFun(AT.TBase(b,ps), atyp2), _ ->
      let x = CS.new_id "x" in
      let ps' = ps (CS.Var x) in
      let ityps,ityp' = IT.decomp_fun (List.length ps') ityp in
      let env' = (x,AT.TBase(b,fun _ -> []))::env in
      let b' = ref_base_of_abs_base b in
      let aux p ityp =
        match ityp with
        | IT.Base IT.True -> [p]
        | IT.Base IT.False -> [CS.make_not p]
        | IT.Inter [] -> []
        | _ -> assert false
      in
      let ts = List.rev_flatten @@ List.map2 aux ps' ityps in
      let p = List.fold_left CS.make_and (CS.Const CS.True) ts in
      let cond' = p::cond in
      let p' = CEGAR_util.normalize_bool_term p in
      let rtyp = ref_of_inter env' cond' (atyp2 (CS.Var x)) ityp' in
      RT.Fun(x, RT.Base(b',x,p'), rtyp)
  | AT.TFun(atyp1,atyp2), IT.Fun(ityp1,ityp2) ->
      let x = CS.new_id "x" in
      let rtyp1 = ref_of_inter env cond atyp1 ityp1 in
      let rtyp2 = ref_of_inter env cond (atyp2 (CS.Var x)) ityp2 in
      RT.Fun(x, rtyp1, rtyp2)
  | AT.TBase(AT.TUnit, _), IT.Base (IT.State _) ->
      RT.Base(RT.Unit, "", CS.Const CS.True)
  | AT.TBase(base, _), IT.Base (IT.State _) when base = AT.typ_result_base ->
      RT.Base(RT.Unit, "", CS.Const CS.True)
  | _ -> Format.printf "atyp:%a@.ityp:%a@." CEGAR_print.typ atyp IT.print ityp; assert false

let ref_of_inter atyp ityp = ref_of_inter [] [] atyp ityp
