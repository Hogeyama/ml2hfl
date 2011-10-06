
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util


exception CannotRefute
      
  

let rec merge_typ typ typ' =
    match typ,typ' with
        TBase(b1,ps1),TBase(b2,ps2) ->
          assert (b1 = b2);
          let x = new_id "x" in
          let ps = uniq compare (ps1 (Var x) @ ps2 (Var x)) in
          let ps t = List.map (subst x t) ps in
            TBase(b1, ps)
      | TFun typ1, TFun typ2 ->
          let x = new_id "x" in
          let typ11,typ12 = typ1 (Var x) in
          let typ21,typ22 = typ2 (Var x) in
          let typ1 = merge_typ typ11 typ21 in
          let typ2 = merge_typ typ12 typ22 in
          let typ' t = subst_typ x t typ1, subst_typ x t typ2 in
            TFun typ'
      | TAbs _, _ -> assert false
      | TApp _, _ -> assert false
let add_pred map env =
  let aux (f,typ) =
    try
      f, merge_typ typ (List.assoc f map)
    with Not_found -> f, typ
  in
    List.map aux env


let refine ces (env,defs,main) =
  let map =
    match !Flag.refine with
        Flag.RefineSizedType ->
          let rec aux = function
              (LineNode _)::ce -> 0 :: aux ce
            | (BrNode b)::(LineNode _)::ce -> (if b then 0 else 1) :: aux ce
            | (EventNode _)::ce -> aux ce
            | [] -> []
            | _ -> assert false
          in
            LazyInterface.infer (List.map aux ces) (env,defs,main)
      | Flag.RefineDependentType ->
          try
            RefineDepTyp.infer ces (env,defs,main)
          with RefineDepTyp.Untypable -> raise CannotRefute
  in
  let env' = add_pred map env in
    env', defs, main

