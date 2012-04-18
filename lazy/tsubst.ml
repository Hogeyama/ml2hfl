open ExtList

(** Typed substitutions *)

type t = (Var.t * Term.t * SimType.t) list

let pr_elem ppf (x, t, _) =
  Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t

(** ignore equalities of functions *)
let formula_of_elem (x, t, ty) =
  Formula.eq_ty ty (Term.make_var x) t

let fvs_elem (x, t, _) = x :: Term.fvs t

let pr ppf sub =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem " && ") sub

let fvs sub = Util.concat_map fvs_elem sub

let formula_of sub = Formula.band (List.map formula_of_elem sub)

let fun_of xttys =
  let sub = List.map (fun (x, t, _) -> x, t) xttys in
  fun x -> List.assoc x sub

let dom xttys = List.map Util.fst3 xttys

let elim_duplicate xttys =
  let xttyss, tss =
    List.split
		    (List.map
		      (fun (((x, t1, ty) as xtty)::xttys) ->
          let xttys, ts =
            Util.partition_map
              (fun (_, t2, _) ->
                (*sound?
                match t1, t2 with
                  Term.Var(_, x1), Term.Var(_, x2) ->
                    if List.mem x1 xs then
                      `L(x1, t2, ty)
                    else
                      `L(x2, t1, ty)
                | Term.Var(_, x1), _ ->
                    `L(x1, t2, ty)
                | _, Term.Var(_, x2) ->
                    `L(x2, t1, ty)
                | _ ->*)
                  `R(Formula.eq_ty ty t1 t2))
              xttys
          in
          xtty::xttys, ts)
		      (Util.classify (fun (x1, _, _) (x2, _, _) -> x1 = x2) xttys))
  in
  List.flatten xttyss, List.flatten tss
