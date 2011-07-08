open ExtList
open ExtString

type t =
  Sub of (Var.t * Term.t) list
| Guard of Term.t
| Cnode of bool * (Var.t * int) * t list
| Nop

let of_error_path p =
  let rec f x g xts trs p =
		  match p with
		    [] ->
        [], Cnode(true, x, Guard(g)::Sub(xts)::trs)
	   | s::p ->
        (match s with
				      Ctree.Call(y, g') ->
				        let p', tr = f y g' [] [] p in
            f x g xts (tr::trs) p'
				    | Ctree.Arg(xts') ->
				        f x g (xts' @ xts) trs p
				    | Ctree.Ret(y, t) -> 
				        p, Cnode(false, x, Guard(g)::Sub((y, t)::xts)::trs)
				    | Ctree.Error ->
            let _ = assert (p = []) in
            p, Cnode(true, x, Guard(g)::Sub(xts)::trs))
  in
  let Ctree.Call(x, t)::p = p in
  snd (f x t [] [] p)

let rec pr ppf tr =
  match tr with
    Sub(xts) ->
      Format.fprintf ppf "%a" Term.pr (Term.band (Ctree.eq_xts xts))
  | Guard(t) ->
      Format.fprintf ppf "%a" Term.pr t
  | Cnode(b, _, trs) ->
      if b then
        Format.fprintf ppf "{@[<v>%a@]}" (Util.pr_list pr ", @,") trs
      else
        Format.fprintf ppf "[@[<v>%a@]]" (Util.pr_list pr ", @,") trs
  | Nop ->
      Format.fprintf ppf "nop"

let rec leaf_of op tr =
		let rec f ctx1 ctx2 tr =
		  match tr with
		    Sub(_)
		  | Guard(_)
		  | Nop ->
		      raise Not_found
		  | Cnode(op', xuid, trs) ->
		      if op = op' && List.for_all (function Sub(_) | Guard(_) -> true | _ -> false) trs then
          ctx1, ctx2, tr
		      else
          Util.find_map
		          (fun (ctx', tr) ->
              f (fun tr -> ctx1 (ctx2 tr))
                (fun tr -> Cnode(op', xuid, ctx' tr))
                tr)
		          (Util.ctx_elem trs)
  in
  f (fun tr -> tr) (fun tr -> tr) tr

let rec callers_of uid tr =
		let rec f ctx1 ctx2 tr =
		  match tr with
		    Sub(_)
		  | Guard(_)
    | Nop ->
		      raise Not_found
		  | Cnode(op, (x, uid'), trs) ->
		      if uid = uid' then
		        []
		      else
		        ((fun tr -> ctx1 (ctx2 tr)), tr)(*???implicit args*)::
		        (Util.find_map
		          (fun (ctx', tr) ->
              f (fun tr -> ctx1 (ctx2 tr))
                (fun tr -> Cnode(op, (x, uid'), ctx' tr))
                tr)
		          (Util.ctx_elem trs))
  in
  f (fun tr -> tr) (fun tr -> tr) tr

let rec_callers_of eptr = function
  Cnode(_, (y, uid), _) ->
		  List.filter
			   (fun (_, Cnode(_, (x, _), _)) -> x = y)
			   (callers_of uid eptr)
| _ -> invalid_arg "Trace.rec_callers_of"

let rec term_of (x, uid) tr =
  let rec f tr =
		  match tr with
		    Sub(xts) ->
		      xts, []
		  | Guard(t) ->
		      [], [t]
		  | Cnode(_, _, trs) ->
		      let xtss, tss = List.split (List.map f trs) in
        List.concat xtss, List.concat tss
		  | Nop ->
		      [], []
  in
  let xts, ts = f tr in
  let xts1, xts2 = List.partition
    (function (Var.T(x', uid', _), _) ->
				  let rec g x uid =
				    (x = x' && uid = uid') ||
				    (match x with
				      Var.T(x, uid, _) -> g x uid
				    | _ -> false)
				  in
      g x uid
    | _ -> false) xts in
  let t = Term.band (Ctree.eq_xts xts1 @ ts) in
  let sub x = List.assoc x xts2 in
  Util.fixed_point (Term.subst sub) (fun t1 t2 -> Term.equiv t1 t2) t

let summary_of pre ctx1 ctx2 tr rec_callers =
		let sub y uid x =
		  match x with
		    Var.V(_) ->
		      Term.make_var2 x
		  | Var.T(y', uid', arg) ->
		      if y = y' && uid = uid' then
		        Term.make_var2 (Var.T(y', 0(*???*), arg))
		      else
		        Term.make_var2 x
		in
		let sub_inv y uid x =
		  match x with
		    Var.V(_) ->
		      Term.make_var2 x
		  | Var.T(y', uid', arg) ->
		      if y = y' && uid' = 0 then
		        Term.make_var2 (Var.T(y', uid, arg))
		      else
		        Term.make_var2 x
		in
		
		let y, uid =
    match tr with
      Cnode(true, (y, uid), _) ->
        let _ = assert (pre = true) in
        let _ = Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Var.pr y uid in
        y, uid
    | Cnode(false, (y, uid), _) ->
        let _ = assert (pre = false) in
        let _ = Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Var.pr y uid in
        y, uid
  in
		let tfun, tctx =
		  match y with
      Var.V(_) ->
        term_of (y, uid) tr,
        term_of (y, uid) (ctx1 (ctx2 Nop))
    | Var.T(_, _, _) ->
(*
        let _ = Format.printf "%a@." pr (ctx1 tr) in
*)
        term_of (y, uid) (ctx1 tr),
        term_of (y, uid) (ctx2 Nop)
  in
		let tts =
		  (Term.subst (sub y uid) tfun, Term.subst (sub y uid) tctx)::
		  List.rev
		    (List.map
		      (fun (ctx, (Cnode(_, (y, uid), _) as tr)) ->
		        Term.subst (sub y uid) (term_of (y, uid) tr),
		        Term.subst (sub y uid) (term_of (y, uid) (ctx Nop)))
		      rec_callers)
		in
		let _, _, tfunss, tctxss = List.fold_left
		  (fun (tfuns, tctxs, tfunss, tctxss) (tfun, tctx) ->
		    tfun::tfuns, tctx::tctxs, tfunss @ [tfun::tfuns], tctxss @ [tctx::tctxs])
		  ([], [], [], [])
		  tts in
		let tfun' = Term.subst (sub_inv y uid) (ApronInterface.widen (List.map Term.bor tfunss)) in
		let tctx' = Term.subst (sub_inv y uid) (ApronInterface.widen (List.map Term.bor tctxss)) in
		
  let t1, t2, t1', t2' =
    if pre then
      tctx, tfun, tctx', tfun'
    else
      tfun, tctx, tfun', tctx'
  in
		let interp =
		  try
		    (if Flag.enable_widening then
		      try
		  						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1' Term.pr t2' in
		        CsisatInterface.interpolate t1' t2'
		      with CsisatInterface.No_interpolant ->
          if pre then
				        if Term.equiv t1 t1' then
				          raise CsisatInterface.No_interpolant
				        else
				    						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2' in
				          CsisatInterface.interpolate t1 t2'
          else
				        if Term.equiv t2 t2' then
				          raise CsisatInterface.No_interpolant
				        else
				    						let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1' Term.pr t2 in
				          CsisatInterface.interpolate t1' t2
		    else
		      raise CsisatInterface.No_interpolant)
		  with CsisatInterface.No_interpolant ->
      (try
				    if Flag.enable_widening && Term.equiv t1 t1' && Term.equiv t2 t2' then
				      raise CsisatInterface.No_interpolant
				    else
										let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
				      CsisatInterface.interpolate t1 t2
      with CsisatInterface.No_interpolant ->
        assert false)
		in
		let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
  if pre then
    `Pre((y, uid), interp), ctx1 (ctx2 (Guard(Term.bnot (interp))))
  else
    `Post((y, uid), interp), ctx1 (ctx2 (Guard(interp)))

let rec summaries_of eptr =
  let rec summaries_of_aux sums eptr =
(**)
		  let _ = Format.printf "error trace:@.  %a@." pr eptr in
(**)
		  try
						let ctx1, ctx2, tr = leaf_of true eptr in
      let sum, eptr = summary_of true ctx1 ctx2 tr (rec_callers_of eptr tr) in
				  summaries_of_aux (sum::sums) eptr
		  with Not_found ->
		    try
								let ctx1, ctx2, tr = leaf_of false eptr in
        let sum, eptr = summary_of false ctx1 ctx2 tr (rec_callers_of eptr tr) in
						  summaries_of_aux (sum::sums) eptr
		    with Not_found ->
		      sums
  in
  summaries_of_aux [] eptr
