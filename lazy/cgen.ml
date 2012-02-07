open ExtList
open ExtString

(** Constraint generation *)

(** invariant: length constr = length subst + 1 and closed then length subst = length trs + 1
    ToDo: does the condition holds for interaction types too?
    @param ret for refinement type inference only *)
type t = { name: Var.t * int; closed: bool; constr: Term.t list; subst: (Var.t * Term.t * SimType.t) list list; ret: (Var.t * int) option }
type tree = Node of t * tree list
type path = Top | Path of path * tree list * t * tree list
type location = Loc of tree * path

let make name closed constr subst =
  Node({ name = name; closed = closed; constr = constr; subst = subst; ret = None }, [])
let get (Node(nd, _)) = nd
let set (Node(_, trs)) nd = Node(nd, trs)

let zipper tr = Loc(tr, Top)
let up (Loc(tr, p)) =
  match p with
    Top -> raise Not_found
  | Path(up, trs1, nd, trs2) -> Loc(Node(nd, trs1 @ tr::trs2), up)
let down (Loc(tr, p)) x =
  match tr with
    Node(nd, trs) ->
      let trs1, tr', trs2 =
        try
          Util.find_split (fun tr -> (get tr).name = x) trs
        with Not_found ->
          assert false
      in
      Loc(tr', Path(p, trs1, nd, trs2))
let rec root (Loc(tr, p) as l) =
  match p with
    Top -> tr
  | _ -> root (up l)
let insert_down (Loc(Node(nd, trs), p)) tr =
  Loc(tr, Path(p, trs, nd, []))

let find_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match List.rev trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, List.rev trs', nd, [])))
  in
  aux (zipper tr)

let find_leaves tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match trs with
      [] ->
        [loc]
    | _ ->
        List.concat
		        (List.init
            (List.length trs)
		          (fun i ->
		            let trs1, tr::trs2 = List.split_nth i trs in
		            aux (Loc(tr, Path(p, trs1, nd, trs2)))))
  in
  aux (zipper tr)

let find_rev_leaf tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    match trs with
      [] ->
        loc
    | tr'::trs' ->
        aux (Loc(tr', Path(p, [], nd, trs')))
  in
  aux (zipper tr)

(*
let rec up_until x_uid (Loc(tr, p)) =
  match p with
    Top -> assert false
  | Path(up, trs1, nd, trs2) ->
      if nd.name = x_uid then
        Loc(Node(nd, trs1 @ tr::trs2), up)
      else
        up_until x_uid (Loc(Node(nd, trs1 @ tr::trs2), up))
*)

let tree_of_path p =
  root (Loc(make (Var.make (Idnt.make "hole"), -1) true [Formula.ttrue] [], p))

let rec pr ppf tr =
  match tr with
    Node(nd, trs) ->
				  let _ =
						  Format.fprintf ppf "@[<v>%a" Var.pr_x_uid nd.name
				  in
				  let _ =
								let _ = Format.fprintf ppf "@,  @[<v>" in
        let _ = let g = List.hd nd.constr in (*if g <> Formula.ttrue then*) Format.fprintf ppf "%a" Term.pr g in
        let _ =
						    if nd.subst <> [] then
            let _ =
				          Util.iter3
				            (fun t xttys tr ->
														    let _ = Format.fprintf ppf ", @,{%a}" (Util.pr_list Term.pr ", @,") (List.map Formula.eq_xtty xttys) in
                  let _ = if t <> Formula.ttrue then Format.fprintf ppf ", @,%a" Term.pr t in
                  Format.fprintf ppf ", @,%a" pr tr)
				            (if List.length trs = List.length nd.constr - 1 then List.tl nd.constr else Util.init (List.tl nd.constr))
				            (if List.length trs = List.length nd.subst then nd.subst else Util.init nd.subst)
				            trs
            in
            if List.length trs + 1 = List.length nd.subst then
              let _ = Format.fprintf ppf ", @,{%a}" (Util.pr_list Term.pr ", @,") (List.map Formula.eq_xtty (List.last nd.subst)) in
		            if List.last nd.constr <> Formula.ttrue then
		              Format.fprintf ppf ", @,%a" Term.pr (List.last nd.constr)
        in
        Format.fprintf ppf "@]"
				  in
				  if nd.closed then
        let x, id =
								  match nd.ret with
		          None -> nd.name
		        | Some(x, id) -> x, id
        in
						  Format.fprintf ppf "@,</%a@@%d>@]"
								  Var.pr x
								  id

let pr_path ppf p =
		Format.fprintf ppf "%a" pr (tree_of_path p)

exception FeasibleErrorTrace of tree

let rec path_set_open p =
		match p with
		  Top -> Top
		| Path(up, trs1, nd, trs2) ->
      Path(path_set_open up, trs1, { nd with ret = None(**ToDo*); closed = false }, trs2)





let term_of_nodes nds =
  Util.concat_map (fun nd -> nd.constr) nds,
  Util.concat_map (fun nd -> List.concat nd.subst) nds

let rec nodes_of_tree (Node(nd, trs)) =
  nd::Util.concat_map nodes_of_tree trs
let rec nodes_of_path p =
  match p with
    Top -> []
  | Path(up, trs1, nd, trs2) ->
      nodes_of_path up @ Util.concat_map nodes_of_tree trs1 @ [nd] @ Util.concat_map nodes_of_tree trs2

let rec left_of_path p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, _) ->
      let ts1, _ = List.split_nth (List.length trs1 + 2) nd.constr in
      let xttyss1, _ = List.split_nth (List.length trs1 + 1) nd.subst in
      Path(left_of_path up, trs1, { nd with constr = ts1; subst = xttyss1}, [])
let rec right_of_path p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, trs2) ->
      let _, ts2 = List.split_nth (List.length trs1 + 2) nd.constr in
      let _, xttyss2 = List.split_nth (List.length trs1 + 1) nd.subst in
      Path(right_of_path up, [], { nd with constr = ts2; subst = xttyss2}, trs2)

let find_all cond tr =
  let rec aux (Loc(Node(nd, trs), p) as loc) =
    if cond nd then [loc] else [] @
    Util.concat_map
      (fun tr -> aux (down loc (get tr).name))
      trs
  in
  aux (zipper tr)

let rec is_recursive (x, uid) (Loc(tr, p) as loc) =
  (try
		  is_recursive (x, uid) (up loc)
  with Not_found ->
    false) ||
		(let x', uid' = (get tr).name in Var.equiv x' x && uid' <> uid)

let rec rec_calls_of x (Loc(tr, p) as loc) =
  let trs, ps = 
		  try
				  rec_calls_of x (up loc)
		  with Not_found ->
		    [], []
  in
		if Var.equiv (fst (get tr).name) x then
		  tr::trs, p::ps
		else
		  trs, ps

let get_unsat_prefix tr =
  let rec aux ts0 xttys0 tr =
		  match tr with
		    Node(nd, trs) ->
        let g = List.hd nd.constr in
        let ts0 = g::ts0 in
(*
        let _ = Format.printf "%a: %a@." Var.pr_x_uid nd.name Term.pr (Formula.bnot (Formula.formula_of_fes (ts0, xttys0))) in
*)
        if Cvc3Interface.is_valid (Formula.bnot (Formula.eqelim (fun _ -> false) (ts0, xttys0))) then
          ts0, xttys0, make nd.name false [g] [], true
        else
		        let rec aux_aux ts0 xttys0 ts xttyss trs =
		          match ts, xttyss, trs with
		            [], [], [] ->
                ts0, xttys0, [], false
            | [t], [xttys], [] ->
                t::ts0, xttys @ xttys0, [], false
		          | t::ts, xttys::xttyss, tr::trs ->
						          let ts0, xttys0, tr, b = aux (t::ts0) (xttys @ xttys0) tr in
				            if b then
				              ts0, xttys0, [tr], true
				            else
						            let ts0, xttys0, trs, b = aux_aux ts0 xttys0 ts xttyss trs in
						            ts0, xttys0, tr::trs, b
            | _ -> assert false
		        in
		        let ts0, xttys0, trs, b = aux_aux ts0 xttys0 (List.tl nd.constr) nd.subst trs in
          ts0, xttys0,
          (if b then
            Node({ nd with constr = List.take (List.length trs + 1) nd.constr;
                           subst = List.take (List.length trs) nd.subst;
                           closed = false;
                           ret = None }, trs)
          else
            Node(nd, trs)),
          b
  in
  let _, _, tr, true = aux [Formula.ttrue] [] tr in
  tr

let interpolate_chk t1 t2 =
  try
    Formula.simplify (CsisatInterface.interpolate t1 t2)
  with CsisatInterface.No_interpolant ->
				if Flags.debug && Cvc3Interface.implies t1 (Formula.bnot t2) then
				  let _ = Format.printf "an error has occurred because of CSIsat@." in
				  assert false
				else
						raise CsisatInterface.No_interpolant

let interpolate_widen closed t1 t2 tw1 tw2 =
  let interp =
				try
				  (try
				    let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr tw2 in
				    interpolate_chk tw1 tw2
				  with CsisatInterface.No_interpolant ->
						  if closed then
						    if Term.equiv t2 tw2 then
						      raise CsisatInterface.No_interpolant
						    else
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr tw1 Term.pr t2 in
						      interpolate_chk tw1 t2
						  else
						    if Term.equiv t1 tw1 then
						      raise CsisatInterface.No_interpolant
						    else
						      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr tw2 in
						      interpolate_chk t1 tw2)
				with CsisatInterface.No_interpolant ->
				  (try
				    if (closed && Term.equiv t1 tw1) || (not closed && Term.equiv t2 tw2) then
				      raise CsisatInterface.No_interpolant
				    else
				      let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
				      interpolate_chk t1 t2
				  with CsisatInterface.No_interpolant ->
				    raise CsisatInterface.No_interpolant)
  in
		let _ = Format.printf "interp_out: %a@ " Term.pr interp in
  interp

(** require: t1 and t2 share only variables that satisfy p *)
let interpolate_widen_bvs p closed t1 t2 tw1 tw2 =
		let t1 = Term.rename_fresh p t1 in
		let t2 = Term.rename_fresh p t2 in
		let tw1 = Term.rename_fresh p tw1 in
		let tw2 = Term.rename_fresh p tw2 in
  interpolate_widen closed t1 t2 tw1 tw2

let interpolate t1 t2 =
		let _ = Format.printf "interp_in1: %a@ interp_in2: %a@ " Term.pr t1 Term.pr t2 in
		let interp = interpolate_chk t1 t2 in
		let _ = Format.printf "interp_out: %a@ " Term.pr interp in
  interp

(* require: t1 and t2 share only variables that satisfy p *)
let interpolate_bvs p t1 t2 =
		let t1 = Term.rename_fresh p t1 in
		let t2 = Term.rename_fresh p t2 in
  interpolate t1 t2


let widen xss ts =
  match ts with
    [t] -> t
  | _ ->
			  	(*
				  List.iter (fun xs -> Format.printf "%a@ " (Util.pr_list Var.pr ", ") xs) xss;
	  			*)
				  let xs = List.hd xss in
				  let ts =
				    List.map2
				      (fun ys t ->
				        let sub = List.combine ys xs in
				        Term.subst (fun x -> Term.make_var (List.assoc x sub)) t)
				      xss
				      ts
				  in
				  ApronInterface.widen (List.map Formula.bor (Util.nonemp_prefixes ts))
