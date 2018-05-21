open Util
open Combinator

(** Call-by-value evalutor *)

(* @require input does not use De Bruijn index *)
let reduce e =
  MLExp.para
    (object
      method fvar _ = fun () -> assert false
      method fcon _ = fun () -> raise Not_found
      method fif ty e1 r1 e2 r2 e3 r3 = fun () ->
        try MLExp.mk_if ty (r1 ()) e2 e3
        with Not_found ->
          if e1 |> Formula.of_term |> Formula.is_true then e2
          else if e1 |> Formula.of_term |> Formula.is_false then e3
          else assert false
      method flet ty_bd (Pattern.V(x), ty) e1 r1 e2 r2 = fun () ->
        try MLExp.mk_let ty_bd (x, ty) (r1 ()) e2
        with Not_found ->MLExp.subst [x, e1] e2
      method fletrec _ _ _ = fun () -> assert false
      method fevent _ _ _ = fun () -> assert false
      method fapp e1 r1 es rs = fun () ->
        try
          let i, e' =
            List.find_mapi (fun _ r -> Option.of_nf r ()) (r1 :: rs)
          in
          List.replace_at i e' (e1 :: es)
          |> List.hd_tl
          |> uncurry2 Term.mk_app
        with Not_found ->
          if CunTerm.is_fun e1 then
            CunTerm.let_fun e1
              (fun (x, _) e ->
                 MLExp.mk_app (Term.subst [x, List.hd es] e) (List.tl es))
          else if MLExp.is_const e1 then
            MLExp.let_const e1
              (fun c ->
                 let cs = List.map (fun e1 -> MLExp.let_const e1 id) es in
                 match Const.eval (c :: cs) with
                 | [c'] -> MLExp.mk_const c'
                 | _ -> raise Not_found)
          else assert false
      method ffun _ _ _ = fun () -> raise Not_found
      method ffix (x, ty) e1 _ = fun () ->
        Term.subst [x, MLExp.mk_fix (x, ty) e1] e1
      method fcls _ _ _ _ = fun () -> assert false
      method ftuple _ _ _  = fun () -> assert false
      method fkon _ _ _ = fun () -> assert false
      method farray _ rs = fun () -> assert false
      method faget _ a _ n  = fun () -> assert false
      method faset _ a _ n _ m _ e  = fun () -> assert false
    end)
    e
    ()

(* @require t does not use De Bruijn index *)
let normalize e =
  let rec aux i e =
    Logger.printf2
      "@[<v>reduction step %a:@,  %a@]@,"
      Integer.pr i
      Term.pr e;
    try e |> reduce |> aux (i + 1) with Not_found -> e
  in
  aux 1 e
let normalize = Logger.log_block1 "CBVEvaluator.normalize" normalize
