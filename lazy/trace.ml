open ExtList
open ExtString

type t =
  Sub of (Var.t * Term.t) list
| Guard of Term.t
| Cnode of bool * (Var.t * int) * t list
| Grp of t list

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
  | Cnode(op, (x, uid), trs) ->
      if op then
        Format.fprintf ppf "@[<v><%a:%d>@,  @[<v>%a@]@]"
          Var.pr x
          uid
          (Util.pr_list pr ", @,") trs
      else
        Format.fprintf ppf "@[<v><%a:%d>@,  @[<v>%a@]</%a:%d>@]"
          Var.pr x
          uid
          (Util.pr_list pr ", @,") trs
          Var.pr x
          uid
  | Grp(trs) ->
      Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr ", @,") trs

let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    Var.V(_) -> false
  | Var.T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

let rec sl_ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    Var.V(_)
  | Var.T(Var.V(_), _, _) -> false
  | Var.T(Var.T(x', uid', _), _, _) -> sl_ancestor_of (x, uid) (x', uid'))

let rec is_pos x =
  match x with
    Var.V(_) -> true
  | Var.T(x', _, _) -> not (is_pos x')

(* return value satisfies p and does not contain a function call that satisfies p *)
let call_of p tr =
		let rec f ctx tr =
		  match tr with
		    Sub(_)
		  | Guard(_) ->
		      raise Not_found
		  | Cnode(op, (x, uid), trs) ->
        (try
		        Util.find_map
				        (fun (ctx', tr) ->
		            f (fun trs -> ctx [Cnode(op, (x, uid), ctx' trs)])
		              tr)
				        (Util.ctx_elem trs)
        with Not_found ->
          if p op (x, uid) then ctx, tr else raise Not_found)
    | Grp(trs) ->
		      Util.find_map
				      (fun (ctx', tr) ->
		          f (fun trs -> ctx [Grp(ctx' trs)])
		            tr)
				      (Util.ctx_elem trs)
  in
  f (fun trs -> if List.length trs = 1 then List.hd trs else Grp(trs)) tr

(*
let rec callers_of uid tr =
		let rec f ctx1 ctx2 tr =
		  match tr with
		    Sub(_)
		  | Guard(_) -> raise Not_found
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
*)

let rec term_of (x, uid) tr =
  let rec f tr =
		  match tr with
		    Sub(xts) ->
		      xts, []
		  | Guard(t) ->
		      [], [t]
		  | Cnode(_, _, trs)
    | Grp(trs) ->
		      let xtss, tss = List.split (List.map f trs) in
        List.concat xtss, List.concat tss
  in
  let xts, ts = f tr in
  let xts1, xts2 = List.partition
    (function (Var.T(x', uid', _), _) ->
      true
      (*ancestor_of (x, uid) (x', uid')*)
    | (Var.V(_), _) -> assert false) xts in
  let t = Term.band (Ctree.eq_xts xts1 @ ts) in
  let sub x = List.assoc x xts2 in
  Util.fixed_point
    (fun t ->
      (*Format.printf "%a@." Term.pr t;*)
      Term.subst sub t)
    (fun t1 t2 -> Term.equiv t1 t2) t


(* tr does not call top-level function *)
let extract (x, uid) tr =
  let rec f tr =
		  match tr with
		    Sub(_)
		  | Guard(_) ->
		      [tr], []
		  | Cnode(op, (x', uid'), trs) ->
        let trsctx, trsfun =
          List.partition
            (function
              Sub(_)
            | Guard(_) -> false
            | Cnode(_, _, _) -> true
            | Grp(_) -> assert false)
            trs
        in
        let trssctx, trssfun = List.split (List.map f trsctx) in
        if sl_ancestor_of (x, uid) (x', uid') then
          List.concat trssctx, trsfun @ List.concat trssfun
        else
          [Cnode(op, (x', uid'), trsfun @ List.concat trssctx)], List.concat trssfun
    | Grp(_) -> assert false
  in
  f tr

(*
let x_uid_of (Cnode(_, (x, uid), _)) = (x, uid)
let pre (Cnode(op, (_, _), _)) = op
*)

let summary_of ctx (Cnode(pre, (x, uid), trs)) rec_callers =		
  let _ =
				if pre then
				  Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Var.pr x uid
				else
				  Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Var.pr x uid
  in
		let trsctx, trsfun =
    List.partition
      (function
        Sub(_)
      | Guard(_) -> false
      | Cnode(_, _, _) -> true
      | Grp(_) -> assert false)
      trs
  in
  (* no need to split trsctx? *)
		let tfun, tctx =
    match x with
      Var.V(_) ->
								term_of (x, uid) (Grp(trsfun)),
								term_of (x, uid) (ctx trsctx)
    | Var.T(x', uid', _) ->
								(* OK? *)
								let p op (x, uid) = x = x' && uid = uid' in
        let ctx', tr' = call_of p (ctx []) in
								term_of (x', uid') (ctx'(trsfun)),
								term_of (x', uid') tr'
		in
		let interp =
				let t1, t2 =
						if pre then
								tctx, tfun
						else
								tfun, tctx
				in
				let interp =
						try
								let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
								CsisatInterface.interpolate t1 t2
						with CsisatInterface.No_interpolant ->
						  assert false
				in
				let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
				interp
		in
		if pre then
				`Pre((x, uid), interp), ctx (Guard(Term.bnot (interp))::trsctx)
		else
				`Post((x, uid), interp), ctx (Guard(interp)::trsctx)
(*
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
		let tts =
		  (Term.subst (sub y uid) tfun, Term.subst (sub y uid) tctx)::
		  List.rev
		    (List.map
		      (fun (ctx, (Cnode(_, (y, uid), _) as tr)) ->
		        Term.subst (sub y uid) (term_of (y, uid) tr),
		        Term.subst (sub y uid) (term_of (y, uid) (ctx [])))
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
*)

let rec summaries_of eptr =
  let rec summaries_of_aux sums eptr =
(**)
		  let _ = Format.printf "error trace:@.  %a@." pr eptr in
(**)
		  try
						(* top level or open function call *)
						let p op (x, uid) =
								match x with
						    Var.V(_) -> true
								| Var.T(_, _, _) -> op
      in
						let ctx, tr = call_of p eptr in
      let sum, eptr = summary_of ctx tr [](*(rec_callers_of eptr tr)*) in
				  summaries_of_aux (sum::sums) eptr
		  with Not_found ->
      sums
  in
  summaries_of_aux [] eptr
