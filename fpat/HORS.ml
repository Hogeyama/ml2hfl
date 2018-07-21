open Combinator
open Util

(** Higher-Order Recursion Schemes *)

let rec pr_exp mode ppf =
  MLExp.visit
    (object
        method fvar x =
          Idnt.pr ppf x
        method fcon c =
          assert false
        method fif e1 e2 e3 =
          assert false
        method flet _ _ _ =
          assert false
        method fletrec fds e1 =
          failwith "pr_exp"
        method fevent id (Term.Const(Const.Unit)) =
          String.pr ppf id
        method fapp e es =
          let e1, e2 = MLExp.mk_app e (List.initial es), List.last es in
          if mode then
            Format.fprintf ppf "%a %a" (pr_exp true) e1 (pr_exp false) e2
          else
            Format.fprintf ppf "(%a %a)" (pr_exp true) e1 (pr_exp false) e2
        method ffun _ _ = failwith "pr_exp"
        method ftuple _ _ = assert false
        method fkon _ _ = assert false
        method ffix _ _ = assert false
        method fcls _ _ _ = assert false
        method farray _ = assert false
        method faget _ _  = assert false
        method faset _ _ _ _  = assert false
      end)

let gen_nt =
  let cnt = ref 0 in
  fun () ->
  cnt := !cnt + 1;
  Idnt.make @@ "N" ^ string_of_int !cnt

let pr ppf e =
  MLExp.let_letrec
    e
    (fun fds e1 [] ->
     let vmap_nts = List.map (fun ((x, _), _, _) -> x, gen_nt ()) fds in
     Format.fprintf ppf "@[<v>%%BEGING@,";
     Format.fprintf ppf "S -> %a.@," (pr_exp true) (Term.rename vmap_nts e1);
     List.iter
       (fun ((x, _), xtys, e) ->
        let vmap_args = 
          List.mapi
            (fun i (x, _) -> x, Idnt.make @@ "x" ^ string_of_int (i + 1))
            xtys
        in
        Format.fprintf
          ppf
          "%a -> %a.@,"
          (List.pr Idnt.pr " ") (List.assoc x vmap_nts :: List.map snd vmap_args)
          (pr_exp true) (Term.rename (vmap_nts @ vmap_args) e))
       fds;
     Format.fprintf ppf "%%ENDG@,@,";
     Format.fprintf ppf "%%BEGINA@,";
     Format.fprintf ppf "q0 end -> .@,";
     Format.fprintf ppf "%%ENDA@,@]")
