open Util
open Combinator
open NumAtom

(** A constraint solver based on relational analysis *)
type args = (Idnt.t * Type.t) list
type r = R of (Idnt.t * args * args) list
(* R(P1(~x1, ~y1),...,Pn(~xn, ~yn)) *)

(* phi => R(P1(~x1, ~y1),...,Pn(~xn, ~yn)) *)
type hcr = Hcr of r * r list * Formula.t
type hcrs = hcr list

(* One step: Hcr(R(~x), [], phi)
   Transitive: Hcr(R(~x), [R(~a); R(~b)], []) *)

(** {6 Inspectors of R} *)
let find key (R(r)) =
  let (_, oldr, newr) = List.find (fun (pv, _old, _new) -> pv = key) r in
  (oldr, newr)

(** {6 Auxiliary constructors} *)

(* R(P1(~x1,~y1),...,Pn(~xn,~yn)) => P?(~x1,~y1,...,~xn,~yn) *)
let pvs_of args = List.map (fun (pv, _) -> pv) args
let args_of (R(r)) = r
let pvar_of (R(r)) =
  r
  |> List.fold_left
    (fun xtys (_,args1,args2) ->  
       (args1) @ (args2) @ xtys
       (*|> List.map (fun (x,ty) -> (Term.mk_var x), ty)*))
    []
  |> PredVar.make (Idnt.make "R")

(* hcr to HornClause *)
let hc_of (Hcr(head, body, phi)) =
  HornClause.mk_def
    (pvar_of head)
    (List.map (pvar_of >> Pva.of_pvar) body)
    phi

let make_hcr r body phi = Hcr(r, body, phi)

(* make PredVar R *)
let make_R theta =
  let fresh args = List.map (fun (x, ty) -> Idnt.new_var (), ty) args in
  let args =
    List.fold_left
      (fun args (pv, (xtys, t)) -> (pv, xtys, fresh xtys)::args) [] theta
  in
  R(args)

let onestep (R(r_args) as r) hc =
  let h = HornClause.head_of hc in
  let pvas = HornClause.bpvas_of hc in
  let phi = HornClause.bphi_of hc in
  let make_eq args1 args2 =
    List.map2
      (fun (v1,_) (v2,_) ->
         Atom.eq Type.mk_int (* need to check the type? *)
           (Term.mk_var v1) (Term.mk_var v2)
         |> Formula.of_atom) args1 args2
  in
  let rhs_phi (pvar:PredVar.t) =
    let (pv, xtys) = PredVar.idnt_of pvar, PredVar.args_of pvar in
    let newr = snd (find pv r) in
    let new_phi = make_eq newr xtys in
    let same_phi = (* @TODO bug *)
      let rest_args = List.filter (fun (id,_,_) -> pv <> id) r_args in
      rest_args
      |> List.map (fun (_, oldr, newr) -> make_eq oldr newr)
      |> List.concat
    in
    (*same_phi @*) new_phi
  in
  let lhs_phi : Formula.t list =
    if (hc |> HornClause.pvsH |> List.unique |> List.length) = List.length pvas
    then (* no duplication of pvar *)
      List.map
        (fun x ->
           let (pv, xtys) = PredVar.idnt_of x, PredVar.args_of x in
           let oldr = fst (find pv r) in
           make_eq oldr xtys)
        (pvas |> List.map (FwWidenHCCSSolver.pvar_of)) |> List.concat
    else raise (Global.NotImplemented "NegaHcSolver.make_hcr")
  in
  match HornClause.pv_of_head h with
  | Some(p) -> make_hcr r [] (phi::(lhs_phi @ rhs_phi p) |> Formula.band)
  | _ -> Logger.debug_assert_false ()
let onesteps r hcs = List.map (onestep r) hcs

let transitive (R(xtys):r) =
  let fresh args = List.map (fun (_, ty) -> (Idnt.new_var(), ty)) args in
  let xtys2, xtys_tr =
    List.map (fun (pv, bef, aft) ->
        let aft2 = fresh aft in
        (pv, aft, aft2), (pv, bef, aft2)) xtys
    |> List.unzip
  in
  make_hcr (R(xtys_tr)) [R(xtys); R(xtys2)] Formula.mk_true

(* input : negative & specification hornclause set *)
let solve hcs =
  let theta = hcs |> HCCS.tenv |> PredSubst.top_of_tenv in
  let r = make_R theta in
  let (spec, nega) = List.partition HornClause.is_root hcs in
  (* for debugging *) Format.printf "@[spec = %a\n@]@?" HCCS.pr spec;
  let hcrs = (transitive r)::(onesteps r nega) in
  hcrs
  |> List.map (hc_of >> HornClause.simplify)
  |> (* for debugging *) Format.printf "@[R = %a\n@]@?" HCCS.pr;
  let r_theta =
    hcrs |> List.map (hc_of >> HornClause.simplify) |> FwWidenHCCSSolver.solve
  in
  let r_formula = r_theta |> List.hd |> (fun (_,(_,phi)) -> phi) in
  let npsi =
    spec
    |> List.map HornClause.bphi_of
    |> Formula.band
  in (* @TODO rename to old *)
  let (old_args, new_args) =
    r |> args_of |> List.map (fun (_,o,n) ->  (o,n)) |> List.unzip
  in
  let dist_apply f (x,y) = (f x, f y) in
  let subst_npsi = (* !psi[new/old] *) (* @TODO not works *)
    let (old_pvs, new_pvs) =
      List.map2 (pvs_of |> dist_apply |> curry2) old_args new_args
      |> List.unzip
    in
    (*
    let _old = npsi |> Formula.fvs_int |> List.map (fun x -> x, Type.mk_int) in
    let _new : (Idnt.t * Type.t) list =
      r |> (fun (R(r):r) -> r) |> List.filter (fun (_,o,n) -> List.mem o _old) |> List.map (fun (_,o,n) -> n)
    in
    *)
    Term.rename
      (List.zip (List.concat old_pvs) (List.concat new_pvs))
      (Formula.term_of npsi) |> Formula.of_term
  in
  let formula = (* @TODO something is wrong *)
    Formula.band
      [npsi;
       Formula.forall
         (List.concat new_args) (Formula.imply r_formula subst_npsi)]
    |> Qelim.integer_qelim_dyn
  in
  List.map (fun (pv, (tenv, phi)) -> pv, (tenv, subst_npsi)) r_theta (* formula print test *)

(* P(x) -> !psi /\ \forall y.(R(x,y) -> !psi[y/x]) *)
(* distribute *)
