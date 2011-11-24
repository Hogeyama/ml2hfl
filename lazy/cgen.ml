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
				    let ts = nd.constr::List.rev (List.map CompTree.eq_xtty nd.subst) in
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

exception FeasibleErrorTrace of tree

let rec path_set_open p =
		match p with
		  Top -> Top
		| Path(up, nd, trs) ->
      Path(path_set_open up, { nd with closed = false }, trs)





let rec ancestor_of (x, uid) (x', uid') =
  (x = x' && uid = uid') ||
  (match x' with
    Var.V(_) -> false
  | Var.T(x', uid', _) -> ancestor_of (x, uid) (x', uid'))

let rec visible y (Var.T(x', uid', arg') as z) =
  match y with
    Var.V(_) -> false
  | Var.T(x, uid, arg) ->
      (x = x' && uid = uid' && arg' <= arg) ||
      (visible x z)

(* for interaction type inference, (x, uid) is always a top level function call *)
let term_of_nodes (x, uid) nds =
  let xttys = Util.concat_map (fun nd -> nd.subst) nds in
  let ts = List.map (fun nd -> nd.constr) nds in
  let xttys1, xttys2 =
    List.partition
		    (function (Var.T(x', uid', _) as y, _, _) ->
		      visible x y (* only for refinement type inference *)
        || ancestor_of (x, uid) (x', uid')
		    | (Var.V(_), _, _) -> assert false)
      xttys
  in
  let sub x = List.assoc x (List.map (fun (x, t, _) -> x, t) xttys2) in
  Util.fixed_point
    (fun t ->
      (*Format.printf "%a@." Term.pr t;*)
      Term.subst sub t)
    (fun t1 t2 -> Term.equiv t1 t2)
    (Term.band (List.map CompTree.eq_xtty xttys1 @ ts))

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
        (* ToDo: no need to be recursive??? *)
        if y = y' && uid = uid' then
          Term.make_var2 (Var.T(y', 0(*???*), arg))
        else
          Term.make_var2 x
  in
  let tts, tps = List.split
    (List.map2
      (fun tr p ->
        Term.simplify (Term.subst (sub (get tr).name) (term_of_nodes (get tr).name (nodes_of_tree tr))),
        Term.simplify (Term.subst (sub (get tr).name) (term_of_nodes (get tr).name (nodes_of_path p))))
      trs ps)
  in
  let sub_inv (y, uid) x =
    match x with
      Var.V(_) ->
        Term.make_var2 x
    | Var.T(y', uid', arg) ->
        (* ToDo: no need to be recursive??? *)
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
  let ttw = if Flags.enable_widening then Term.subst (sub_inv nd.name) (ApronInterface.widen (List.map Term.bor (hoge tts))) else Term.tunit(*dummy*) in
  let tpw = if Flags.enable_widening then Term.subst (sub_inv nd.name) (ApronInterface.widen (List.map Term.bor (hoge tps))) else Term.tunit(*dummy*)in
  let interp =
    let t1, t2, tw1, tw2 =
      if nd.closed then
        tt, tp, ttw, tpw
      else
        tp, tt, tpw, ttw
    in

		  let interp =
      if Term.equiv t2 Term.ttrue then
        (* this is necessary for finding refinement types that witness the infeasibility of error trace *)
        Term.ttrue
      else
		    try
		      (if Flags.enable_widening then
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
		        if Flags.enable_widening && Term.equiv t1 tw1 && Term.equiv t2 tw2 then
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

let rec summaries_of constrs0 =
  let rec summaries_of_aux sums constrs =
(**)
    let _ = Format.printf "constraints:@.  %a@." pr constrs in
(**)
    let sums', constrs_opt =
      try
        summary_of (find_leaf constrs)
      with CsisatInterface.No_interpolant ->
        raise (FeasibleErrorTrace(constrs0))
    in
    match constrs_opt with
      None -> sums' @ sums
    | Some(constrs) ->
        summaries_of_aux (sums' @ sums) constrs
  in
  summaries_of_aux [] constrs0
