


open Utilities
open CEGAR_syntax
open CEGAR_type


exception CannotRefute

let rec subst_typ x t = function
    TBase(b,ps) ->
      let y = new_id "x" in
      let ps t' =
        let aux t'' = subst y t' (subst x t t'') in
          List.map aux (ps (Var y))
      in
        TBase(b, ps)
  | TFun typ ->
      let y = new_id "x" in
      let typ1,typ2 = typ (Var y) in
      let typ' t' =
        let aux typ = subst_typ y t' (subst_typ x t typ) in
          aux typ1, aux typ2
      in
        TFun typ'
  | TAbs _ -> assert false
  | TApp _ -> assert false
      
  

let rec merge_typ typ typ' =
    match typ,typ' with
        TBase(b1,ps1),TBase(b2,ps2) ->
          assert (b1 = b2);
          let x = new_id "x" in
          let ps t =
            let aux = subst x t in
             List.map aux (ps1 (Var x) @ ps2 (Var x))
          in
            TBase(b1, ps)
      | TFun typ1, TFun typ2 ->
          let x = new_id "x" in
          let typ11,typ12 = typ1 (Var x) in
          let typ21,typ22 = typ2 (Var x) in
          let typ' t = subst_typ x t (merge_typ typ11 typ21), subst_typ x t (merge_typ typ12 typ22) in
            TFun typ'
      | TAbs _, _ -> assert false
      | TApp _, _ -> assert false
let add_pred map env =
  let aux (f,typ) = f, merge_typ typ (List.assoc f map) in
    List.map aux env


let refine ces prog =
  prog
