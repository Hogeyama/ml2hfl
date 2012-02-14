open ExtList
open ExtString
open Zipper

(** Trace constraint generation *)

(** {6 Type} *)

(** element of trace constraint
    invariant: length constr = length subst
    invariant: length subst = length trs or length subst = length trs + 1
               (closed or not is not relevant)
    ToDo: does the condition holds for interaction types too?
    @param ret for refinement type inference only *)
type t = { name: Var.t * int;
           closed: bool;
           ret: (Var.t * int) option;
           guard: Term.t;
           constr: Term.t list;
           subst: (Var.t * Term.t * SimType.t) list list }

(** {6 Exception} *)

exception FeasibleErrorTrace of t tree

(** {6 Functions on trace trees} *)

let make name closed guard constr subst =
  Zipper.make { name = name; closed = closed; ret = None;
                guard = guard; constr = constr; subst = subst } []

let rec pr ppf tr =
  match tr with
    Node(nd, trs) ->
				  let _ =
						  Format.fprintf ppf "@[<v>%a" Var.pr_x_uid nd.name
				  in
				  let _ =
								let _ = Format.fprintf ppf "@,  @[<v>" in
        let _ = (*if nd.guard <> Formula.ttrue then*) Format.fprintf ppf "%a" Term.pr nd.guard in
        let _ =
						    if nd.subst <> [] then
            let _ =
				          Util.iter3
				            (fun t xttys tr ->
														    let _ = Format.fprintf ppf ", @,{%a}" (Util.pr_list Term.pr ", @,") (List.map Formula.eq_xtty xttys) in
                  let _ = if t <> Formula.ttrue then Format.fprintf ppf ", @,%a" Term.pr t in
                  Format.fprintf ppf ", @,%a" pr tr)
				            (List.take (List.length trs) nd.constr)
				            (List.take (List.length trs) nd.subst)
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

let get_min_unsat_prefix tr =
  let rec aux (ts0, xttys0) tr =
		  match tr with
		    Node(nd, trs) ->
        let fes = nd.guard::ts0, xttys0 in
(*
        let _ = Format.printf "%a: %a@." Var.pr_x_uid nd.name Term.pr (Formula.bnot (Formula.formula_of_fes fes)) in
*)
        if Cvc3Interface.is_valid (Formula.bnot (Formula.eqelim_fes (fun _ -> false) fes)) then
          fes, make nd.name false nd.guard [] [], true
        else
		        let rec aux_aux (ts0, xttys0) ts xttyss trs =
		          match ts, xttyss, trs with
		            [], [], [] ->
                (ts0, xttys0), [], false
            | [t], [xttys], [] ->
                (t::ts0, xttys @ xttys0), [], false
		          | t::ts, xttys::xttyss, tr::trs ->
						          let fes, tr, fail = aux (t::ts0, xttys @ xttys0) tr in
				            if fail then
				              fes, [tr], true
				            else
						            let fes, trs, fail = aux_aux fes ts xttyss trs in
						            fes, tr::trs, fail
            | _ -> assert false
		        in
		        let fes, trs, fail = aux_aux fes nd.constr nd.subst trs in
          fes,
          (if fail then
            Node({ nd with constr = List.take (List.length trs) nd.constr;
                           subst = List.take (List.length trs) nd.subst;
                           closed = false;
                           ret = None }, trs)
          else
            Node(nd, trs)),
          fail
  in
  let _, tr, true = aux ([Formula.ttrue(*???*)], []) tr in
  tr

(** {6 Functions on trace zippers} *)

let down loc x = Zipper.down loc (fun nd -> nd.name = x)

let rec is_recursive (x, uid) (Loc(tr, _) as loc) =
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

(** @deprecated use ??? *)
let rec up_until x_uid (Loc(tr, p)) =
  match p with
    Top -> assert false
  | Path(up, trs1, nd, trs2) ->
      if nd.name = x_uid then
        Loc(Node(nd, trs1 @ tr::trs2), up)
      else
        up_until x_uid (Loc(Node(nd, trs1 @ tr::trs2), up))

(** {6 Functions on trace paths} *)

let tree_of_path p =
  root (Loc(make (Var.make (Idnt.make "hole"), -1) true Formula.ttrue [] [], p))

let pr_path ppf p =
		Format.fprintf ppf "%a" pr (tree_of_path p)

let rec path_set_open p =
		match p with
		  Top -> Top
		| Path(up, trs1, nd, trs2) ->
      Path(path_set_open up, trs1, { nd with ret = None(**ToDo*); closed = false }, trs2)

let rec left_of_path p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, _) ->
      let ts1, _ = List.split_nth (List.length trs1 + 1) nd.constr in
      let xttyss1, _ = List.split_nth (List.length trs1 + 1) nd.subst in
      Path(left_of_path up, trs1, { nd with constr = ts1; subst = xttyss1}, [])
let rec right_of_path p =
  match p with
    Top -> Top
  | Path(up, trs1, nd, trs2) ->
      let _, ts2 = List.split_nth (List.length trs1 + 1) nd.constr in
      let _, xttyss2 = List.split_nth (List.length trs1 + 1) nd.subst in
      Path(right_of_path up, [], { nd with constr = ts2; subst = xttyss2}, trs2)

(** {6 Functions on trace nodes} *)

let fes_of_nodes nds =
  Util.concat_map (fun nd -> nd.guard :: nd.constr) nds,
  Util.concat_map (fun nd -> List.concat nd.subst) nds
