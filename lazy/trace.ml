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
        Format.fprintf ppf "@[<v><%a:%d>@,  @[<v>%a@]@,</%a:%d>@]"
          Var.pr x
          uid
          (Util.pr_list pr ", @,") trs
          Var.pr x
          uid
  | Grp(trs) ->
      Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr ", @,") trs

let rec tlc_of (x, uid) =
  match x with
    Var.V(_) -> (x, uid)
  | Var.T(x', uid', _) -> tlc_of (x', uid')


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

let rec dl_ancestor_of (x, uid) (x', _) =
  match x' with
    Var.V(_) -> false
  | Var.T(Var.V(_) as x', uid', _) -> x = x' && uid = uid'
  | Var.T(Var.T(x', uid', _), _, _) -> dl_ancestor_of (x, uid) (x', uid')

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
          if p op (x, uid) trs then ctx, tr else raise Not_found)
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
      (*true*)
      ancestor_of (x, uid) (x', uid')
    | (Var.V(_), _) -> assert false) xts in
  let t = Term.band (Ctree.eq_xts xts1 @ ts) in
  let sub x = List.assoc x xts2 in
  Util.fixed_point
    (fun t ->
      (*Format.printf "%a@." Term.pr t;*)
      Term.subst sub t)
    (fun t1 t2 -> Term.equiv t1 t2) t

let flatten p tr =
  let rec f tr =
		  match tr with
		    Sub(_)
		  | Guard(_) ->
		      [tr], []
		  | Cnode(op, (x, uid), trs) ->
        let trs', sub =
          let trss, subs = List.split (List.map f trs) in
          List.concat trss, List.concat subs
        in
        (match x with
          Var.V(_) ->
            let trss1, sub =
              Util.partition_map
                (fun (x_uid, trs) ->
                  if sl_ancestor_of (x, uid) x_uid then
                    `L(trs)
                  else
                    `R(x_uid, trs))
                sub
            in
            let trss2, sub =
              let rec f trs =
                Util.concat_map (function Cnode(_, (x_uid), trs) -> [x_uid](*::(f trs)*) | _ -> []) trs
              in
              let callee = f trs' in
              Util.partition_map
		              (fun (x_uid, trs) ->
		                if List.exists (fun (x_uid') -> dl_ancestor_of x_uid' x_uid) callee then
                    let _ = let (x, uid) = x_uid in assert true(*the length of the path of x is odd*) in
		                  `L(trs)
		                else
		                  `R(x_uid, trs))
		              sub
            in
            [Cnode(op, (x, uid), trs' @ List.concat trss1 @ List.concat trss2)], sub
        | Var.T(_, _, _) ->
            if p op then
              [], ((x, uid), trs')::sub
            else
(*
              if is_pos x then
*)
		              [Cnode(op, (x, uid), trs')], sub)
(*
              else
		              let trs1, trs2 = List.partition (function Sub(_) | Guard(_) -> true | Cnode(_, _, _) -> false | Grp(_) -> assert false) trs' in
		              [Cnode(op, (x, uid), trs2)], ((x, uid), trs1)::sub
*)
    | Grp(_) -> assert false
  in
  let [tr], sub = f tr in
(**)
  let _ = assert (sub = []) in
(**)
  tr

(*
let x_uid_of (Cnode(_, (x, uid), _)) = (x, uid)
let pre (Cnode(op, (_, _), _)) = op
*)

let summary_of ctx (Cnode(pre, (x, uid), trs)) rec_callers =		
  let emp, x', uid' =
    match List.filter
      (function
        Sub(_)
      | Guard(_) -> false
      | Cnode(_, _, _) -> true
      | Grp(_) -> assert false)
      trs with
      [] -> true, x, uid
    | [Cnode(_, (x', uid'), _)] -> false, x', uid'
    | _ -> assert false
  in
  let _ =
				if pre then
				  Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Var.pr x' uid'
				else
				  Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Var.pr x' uid'
  in
		let tfun, tctx, trs' =
    let (x, uid) = tlc_of (x, uid) in
    if emp then
						term_of (x, uid) (Grp(trs)),
						term_of (x, uid) (ctx []),
      []
    else
      let [Cnode(_, _, trs')], trs = List.partition (function Cnode(_, _, _) -> true | _ -> false) trs in
      let _ = assert pre in
						term_of (x, uid) (ctx trs'),
						term_of (x, uid) (Grp(trs)),
      trs'
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
    if emp then
  				[`Pre((x', uid'), interp)], ctx [Guard(Term.bnot (interp))]
    else
      [`Pre((x, uid), Term.make_true); `Pre((x', uid'), interp)], ctx (Guard(interp)::trs')
		else
				[`Post((x', uid'), interp)], ctx [Guard(interp)]
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
						let p op (x, uid) trs =
								(match x with
						    Var.V(_) -> List.for_all (function Cnode(_, _, _) -> false | _ -> true) trs
								| Var.T(_, _, _) -> false) ||
        (op && is_pos x &&
        (match List.filter (function Cnode(_, _, _) -> true | _ -> false) trs with
          [] -> true
        | [Cnode(_, (y, _), _)] -> not (is_pos y)
        | _ -> false))
      in
						let ctx, tr = call_of p eptr in
      let sums', eptr = summary_of ctx tr [](*(rec_callers_of eptr tr)*) in
				  summaries_of_aux (sums' @ sums) eptr
		  with Not_found ->
      sums
  in
(*
		let _ = Format.printf "error trace:@.  %a@." pr eptr in
*)
  summaries_of_aux [] (flatten (fun op -> not op) eptr)
