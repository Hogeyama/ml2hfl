open ExtList
open ExtString

type t = { name: Var.t * int; closed: bool; constr: Term.t; subst: (Var.t * Term.t * SimType.t) list }
type tree = Node of t * tree list
type path = Top | Path of path * t * tree list
type location = Loc of tree * path

let make name closed constr subst =
  Node({ name = name; closed = closed; constr = constr; subst = subst }, [])
let get (Node(nd, _)) = nd
let set (Node(_, trs)) nd = Node(nd, trs)

let zipper tr = Loc(tr, Top)
let up (Loc(tr, p)) =
  match p with
    Top -> raise Not_found
  | Path(up, nd, trs) -> Loc(Node(nd, tr::trs), up)
let down (Loc(tr, p)) x =
  match tr with
    Node(nd, trs) ->
      let [tr'], trs' = List.partition (fun tr -> (get tr).name = x) trs in
      Loc(tr', Path(p, nd, trs'))
let rec root (Loc(tr, p) as l) =
  match p with
    Top -> tr
  | _ -> root (up l)
let insert_down (Loc(Node(nd, trs), p)) tr' =
  Loc(tr', Path(p, nd, trs))

let find_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, nd, trs')))
  in
  aux (zipper tr)

let rec pr ppf tr =
  match tr with
    Node(nd, trs) ->
				  let _ =
						  Format.fprintf ppf "@[<v><%a:%d>"
						    Var.pr (fst nd.name)
						    (snd nd.name)
				  in
				  let _ =
				    let ts = nd.constr::List.rev (List.map Ctree.eq_xtty nd.subst) in
				    if ts <> [] then
						    Format.fprintf ppf "@,  @[<v>%a@]"
						      (Util.pr_list Term.pr ", @,") ts
				  in
				  let _ =
				    if trs <> [] then
						    Format.fprintf ppf "@,  @[<v>%a@]"
						      (Util.pr_list pr ", @,") (List.rev trs)
				  in
				  if nd.closed then
						  Format.fprintf ppf "@,</%a:%d>@]"
						    Var.pr (fst nd.name)
						    (snd nd.name)

let rec function_calls_of ep =
  match ep with
    [] ->
      []
  | s::ep' ->
      (match s with
        Ctree.Call(x, _) ->
          x::function_calls_of ep'
      | _ -> function_calls_of ep')

let of_error_path ep =
		let rec path_set_open p =
		  match p with
		    Top -> Top
		  | Path(up, nd, trs) ->
        Path(path_set_open up, { nd with closed = false }, trs)
  in
  let rec aux (Loc(tr, p) as loc) ep0 =
    match ep0 with
      [] ->
        assert false
    | s::ep ->
        (match s with
          Ctree.Call(y, g) ->
            if Var.is_top (fst y) then
              aux (insert_down loc (make y true g [])) ep
            else if Var.is_pos (fst y) then
              let _ = assert (g = Term.ttrue) in
              aux (down loc (Var.fc_of (fst y))) ep
            else if Var.is_neg (fst y) then
              let _ = assert (g = Term.ttrue) in
		            aux (up loc) ep
            else assert false
        | Ctree.Arg(xttys) ->
            let nd = get tr in
            aux (Loc(set tr { nd with subst = xttys @ nd.subst }, p)) ep
        | Ctree.Ret(x, t, ty) ->
            let nd = get tr in
            let nd' = { nd with subst = (x, t, ty)::nd.subst } in
            let Var.T(f, _, _) = x in
            if Var.is_pos f then
              aux (up (Loc(set tr nd', p))) ep
            else if Var.is_neg f then
              aux (down (Loc(set tr nd', p)) (Var.fc_of x)) ep
            else assert false
        | Ctree.Nop ->
            aux loc ep
        | Ctree.Error ->
            let _ = assert (ep = []) in
            let nd = get tr in
            root (Loc(set tr { nd with closed = false }, path_set_open p)))
  in
  match ep with
    Ctree.Call(x, g)::ep -> aux (zipper (make x true g [])) ep
  | _ -> assert false

exception FeasibleErrorTrace of tree

let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    Var.V(_) -> false
  | Var.T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

let term_of_nodes (x, uid) nds =
  let xttys = Util.concat_map (fun nd -> nd.subst) nds in
  let ts = List.map (fun nd -> nd.constr) nds in
  let xttys1, xttys2 =
    List.partition
		    (function (Var.T(x', uid', _), _, _) ->
		      (*true*)
		      ancestor_of (x, uid) (x', uid')
		    | (Var.V(_), _, _) -> assert false)
      xttys
  in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
  Util.fixed_point
    (fun t ->
      (*Format.printf "%a@." Term.pr t;*)
      Term.subst sub t)
    (fun t1 t2 -> Term.equiv t1 t2)
    (Term.band (List.map Ctree.eq_xtty xttys1 @ ts))

let rec nodes_of_tree (Node(nd, trs)) =
  nd::Util.concat_map nodes_of_tree trs
let rec nodes_of_path p =
  match p with
    Top -> []
  | Path(up, nd, trs) ->
      nodes_of_path up @ [nd] @ Util.concat_map nodes_of_tree trs

let rec rec_calls_of x (Loc(tr, p) as loc) =
  let trs, ps = 
		  try
				  rec_calls_of x (up loc)
		  with Not_found ->
		    [], []
  in
		if fst (get tr).name = x then
		  tr::trs, p::ps
		else
		  trs, ps

let summary_of (Loc(Node(nd, []), p) as loc) =  
  let x, uid = fst nd.name, snd nd.name in
		let _ =
		  if nd.closed then
		    Format.printf "computing a postcondition of <%a:%d>:@.  @[<v>" Var.pr x uid
		  else
		    Format.printf "computing a precondition of <%a:%d>:@.  @[<v>" Var.pr x uid
		in
  let trs, ps = rec_calls_of (fst nd.name) loc in

  let sub (y, uid) x =
    match x with
      Var.V(_) ->
        Term.make_var2 x
    | Var.T(y', uid', arg) ->
        if y = y' && uid = uid' then
          Term.make_var2 (Var.T(y', 0(*???*), arg))
        else
          Term.make_var2 x
  in
  let tts, tps = List.split
    (List.map2
      (fun tr p ->
        Term.subst (sub nd.name) (term_of_nodes nd.name (nodes_of_tree tr)),
        Term.subst (sub nd.name) (term_of_nodes nd.name (nodes_of_path p)))
      trs ps)
  in
  let sub_inv (y, uid) x =
    match x with
      Var.V(_) ->
        Term.make_var2 x
    | Var.T(y', uid', arg) ->
        if y = y' && uid' = 0 then
          Term.make_var2 (Var.T(y', uid, arg))
        else
          Term.make_var2 x
  in
  let hoge ts =
		  let _, tss = List.fold_left
		    (fun (ts, tss) t ->
		      t::ts, tss @ [t::ts])
		    ([], [])
		    ts
    in
    tss
  in
  let tt = Term.subst (sub_inv nd.name) (List.hd tts) in
  let tp = Term.subst (sub_inv nd.name) (List.hd tps) in
  let ttw = Term.subst (sub_inv nd.name) (ApronInterface.widen (List.map Term.bor (hoge tts))) in
  let tpw = Term.subst (sub_inv nd.name) (ApronInterface.widen (List.map Term.bor (hoge tps))) in
  let interp =
    let t1, t2, tw1, tw2 =
      if nd.closed then
        tt, tp, ttw, tpw
      else
        tp, tt, tpw, ttw
    in

		  let interp =
		    try
		      (if LazyFlag.enable_widening then
		        try
		          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
		          CsisatInterface.interpolate tw1 tw2
		        with CsisatInterface.No_interpolant ->
		          if nd.closed then
		            if Term.equiv t2 tw2 then
		              raise CsisatInterface.No_interpolant
		            else
		              let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
		              CsisatInterface.interpolate tw1 t2
		          else
		            if Term.equiv t1 tw1 then
		              raise CsisatInterface.No_interpolant
		            else
		              let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
		              CsisatInterface.interpolate t1 tw2
		      else
		        raise CsisatInterface.No_interpolant)
		    with CsisatInterface.No_interpolant ->
		      (try
		        if LazyFlag.enable_widening && Term.equiv t1 tw1 && Term.equiv t2 tw2 then
		          raise CsisatInterface.No_interpolant
		        else
		          let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
		          CsisatInterface.interpolate t1 t2
		      with CsisatInterface.No_interpolant ->
		        raise CsisatInterface.No_interpolant)
		  in
    let _ = Format.printf "interp_out: %a@]@." Term.pr interp in
    interp
  in
  if nd.closed then
    [`Post((x, uid), interp)],
    match p with
      Top -> assert false
    | Path(up, nd, trs) -> Some(root (Loc(Node({ nd with constr = Term.band [nd.constr; interp] }, trs), up)))
  else
    [`Pre((x, uid), interp)],
    match p with
      Top -> let _ = assert (interp = Term.ttrue) in None
    | Path(up, nd, trs) -> Some(root (Loc(Node({ nd with constr = Term.band [nd.constr; Term.bnot (interp)] }, trs), up)))

let rec summaries_of eptr0 =
  let rec summaries_of_aux sums eptr =
(**)
    let _ = Format.printf "error trace:@.  %a@." pr eptr in
(**)
    let sums', eptr_opt =
      try
        summary_of (find_leaf eptr)
      with CsisatInterface.No_interpolant ->
        raise (FeasibleErrorTrace(eptr0))
    in
    match eptr_opt with
      None -> sums' @ sums
    | Some(eptr) ->
        summaries_of_aux (sums' @ sums) eptr
  in
  summaries_of_aux [] eptr0

let infer_env prog eptrs fcs =
(**)
  let _ = Format.printf "error traces:@.  @[<v>%a@]@." (Util.pr_list pr "@,") eptrs in
(**)
  let sums = Util.concat_map
    (fun eptr ->
      Format.printf "@.";
      summaries_of eptr)
    eptrs
  in
(*
  let _ = List.iter (function `Pre((x, uid), pre) ->
    Format.printf "Pre(%a,%d): %a@." Var.pr x uid Term.pr pre
  | `Post((x, uid), post) ->
    Format.printf "Post(%a,%d): %a@." Var.pr x uid Term.pr post) sums
  in
*)
  let senv = SizType.shape_env_of (Prog.type_of prog) fcs in
  let env = SizType.of_summaries senv sums in
  let env' =
    List.map
      (fun (f, sty) ->
        f, SizType.of_simple_type sty)
      (List.find_all
        (fun (f, sty) -> not (List.mem_assoc f env))
        (List.map
          (fun (f, sty) -> Var.make f, sty)
          prog.Prog.types))
  in
  env @ env'
