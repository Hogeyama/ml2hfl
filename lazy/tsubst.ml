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


(** @param pids specifies the priority *)
let extract_from pids p t =
  let ts = Formula.conjuncts t in
		let eqcs, ts =
		  Util.partition_map
		    (fun t ->
		      match Term.fun_args t with
		        Term.Const(_, Const.EqUnit), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Unit)
		      | Term.Const(_, Const.EqBool), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Bool)
		      | Term.Const(_, Const.EqInt), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Int)
		      | _ -> `R(t))
		    ts
		in
		let eqcs =
				let rec aux eqcs1 eqcs2 =
				  match eqcs2 with
				    [] -> eqcs1
				  | eqc2::eqcs2' ->
				      let eqcs1' =
				  						let flag = ref false in
		          let eqcs =
              List.map
                (fun eqc1 ->
                  if Util.inter (fst eqc2) (fst eqc1) <> [] then
                    let _ = assert (snd eqc1 = snd eqc2) in
                    let _ = flag := true in
                    List.unique (fst eqc1 @ fst eqc2), snd eqc1
                  else
                    eqc1)
                eqcs1
            in
				        if !flag then eqcs else eqc2 :: eqcs
				      in
				      aux eqcs1' eqcs2'
				in
    Util.fixed_point (aux []) (fun eqcs1 eqcs2 -> List.length eqcs1 = List.length eqcs2) eqcs
  in
  let ts', sub =
    Util.flatten_unzip
		    (List.map
		      (fun (eqc, ty) ->
		        let xs1, xs2 = List.partition p eqc in
		        match xs1 with
		          [] ->
		            [], List.map (fun x -> x, Term.make_var (List.hd xs2), ty) (List.tl xs2)
		        | x::xs ->
		            let x, xs =
                try
                  let pid = List.find (fun pid -> List.mem pid xs1) pids in
                  pid , Util.diff xs1 [pid]
                with Not_found -> x, xs
              in
		            List.map (fun x' -> Formula.eq_ty ty (Term.make_var x) (Term.make_var x')) xs,
		            List.map (fun x' -> x', Term.make_var x, ty) xs2)
		      eqcs)
  in
  fun_of sub, Formula.simplify (Formula.band (ts @ ts'))

(** may return a substitution of the form {x -> y, y -> z}
    unsound for non linear expressions? maybe not *)
let extract_from2 pvs p ts =
  let nlfvs = LinArith.nlfvs (Formula.band ts) in
  let rec aux ts xttys0 ts0 =
    match ts with
      [] -> xttys0, ts0
    | t::ts' ->
        let xttys0, ts0 =
          try
		          let dom = List.map Util.fst3 xttys0 in
		          let xtty = Formula.xtty_of p dom t in
		          let xtty =
              (*Format.printf "xtty: %a@,nlfvs: %a@,pvs: %a@," pr_elem xtty (Util.pr_list Var.pr ",") nlfvs (Util.pr_list Var.pr ",") pvs;*)
				          if List.mem (Util.fst3 xtty) nlfvs && not (Formula.is_linear (Util.snd3 xtty)) ||
                 List.mem (Util.fst3 xtty) pvs && Term.coeffs (Util.snd3 xtty) <> [] (*|| t is constant*) then
				            raise Not_found
				          else
				            xtty
		          in
		          xtty::xttys0, ts0
          with Not_found ->
            xttys0, t::ts0
        in
        aux ts' xttys0 ts0
  in
  let xttys0, ts0 = aux ts [] [] in
  let xttys1, ts1 = elim_duplicate xttys0 in
  xttys1, Formula.band (ts0 @ ts1)


(** ensure: the result does not use =b *)
let elim_boolean ts =
  let bvs = List.unique (Util.concat_map (fun t -> Term.fvs_ty SimType.Bool t SimType.Bool) ts) in
  if bvs = [] then
    [ts],
    function [t] -> t | _ -> assert false
  else
    let subs = Util.multiply_list_list (@) (List.map (fun b -> [[b, Formula.ttrue, SimType.Bool]; [b, Formula.tfalse, SimType.Bool]]) bvs) in
    List.map
      (fun sub ->
         List.map (fun t -> Formula.simplify (Term.subst (fun_of sub) t)) ts)
      subs,
    fun ts ->
      Formula.bor
						  (List.map2
								  (fun t sub ->
										  Formula.band [t; formula_of sub])
										ts
										subs)
