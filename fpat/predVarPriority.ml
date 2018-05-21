open Util
open Combinator

(** priority constraints between predicate variables *)
(* @todo to Idnt.t list and move to MultiObjectiveHCCSSolver *)
type t = (Idnt.t * Idnt.t) list (* (high, low)*)

(** {6 Printers} *)

let pr_elem ppf elem =
  Format.fprintf
    ppf
    "@[(%a, %a)@]"
    Idnt.pr (fst elem)
    Idnt.pr (snd elem)

let pr ppf li =
  Format.fprintf
    ppf
    "@[<v>%a@]"
    (List.pr pr_elem ", ")
    li

(** {6 Operators} *)

(** get next nodes list *)
let nexts set node =
  set
  |> List.filter (fun x -> fst x = node)
  |> List.map snd

(* higher to lower *)
let rec of_list = function
  | x::[] -> []
  | x1::x2::xs -> (x1,x2)::(of_list (x2::xs))
  | _ -> assert false
let of_list li = if li = [] then [] else of_list li

(** head is the highest priority idnt *)
let rec list_of acc = function
  | (x1,x2)::[] -> List.rev (x2::x1::acc)
  | (x1,x2)::xs -> list_of (x1::acc) xs
  | _ -> assert false
let list_of li = if li = [] then [] else list_of [] li

(** perform topological sort
    @return total ordered list (HIGH to LOW) *)
let topological_sort set prior =
  let visited = ref [] in
  let sorted = ref [] in
  let rec visit n =
    if not @@ List.mem n !visited then
      (visited := n :: !visited;
       List.iter visit (nexts prior n);
       sorted := n :: !sorted)
  in
  List.iter visit set;
  Logger.printf "sort: %a@," (List.pr Idnt.pr "; ") !sorted; (*dbg*)
  assert ((List.length set) = (List.length !sorted));
  !sorted

(** substitute a template and the previous solution to pvar then return (new * old) *)
let mk_new_old_phis _new old pvar =
  pvar
  |> Pva.of_pvar
  |> (fun x -> PredSubst.lookup _new x,
               PredSubst.lookup old x)

(** make a constraint which represents improvement for a given idnt.t *)
let imply (pvar_new, pvar_old) pid pvpole =
  (if PredVarPoles.pole_of pid pvpole
   then (pvar_new, pvar_old) (* new => old : minimize *)
   else (pvar_old, pvar_new))(* old => new : Maximize*)
  |> uncurry2 Formula.imply
  |> FormulaSimplifier.simplify

(** make a constraint which represents it is not decline for a given idnt.t *)
let e_not_rev_imply (pvar_new, pvar_old) pid pvpole pred =
  (if PredVarPoles.pole_of pid pvpole
   then (pvar_old, pvar_new)
   else (pvar_new, pvar_old))
  |> uncurry2 Formula.imply
  |> Logger.pprintf "a: %a@," Formula.pr
  |> Formula.subst
    (pred
     |> fst
     |> List.map (fun (id, _) -> id, Term.new_coeff ())) (* existentially quantifying by replacing variables with coefficients *)
  |> Logger.pprintf "b: %a@," Formula.pr
  |> Formula.bnot
  |> Logger.pprintf "c: %a@," Formula.pr
let e_not_rev_imply =
  Logger.log_block4
    "PredVarPriority.e_not_rev_imply"
    e_not_rev_imply

let eq (pvar_new, pvar_old) =
  Formula.mk_iff pvar_new pvar_old
  |> FormulaSimplifier.simplify

(** eliminate universal quantifiers by applying Farkas' lemma *)
let approximate phi =
  phi
  |> Formula.bnot
  |> FormulaSimplifier.simplify
  |> DNF.of_formula
  |> DNF.formula_of
  |> PolyConstrSolver.gen_coeff_constr
    ~pos:false ~linear:!Template.linear_farkas

(** @require total priority relation list (head is the highest priority) *)
let whole_constr prev tmpl pvpole priors =
  let priors = List.filter (fun id -> List.mem_assoc id pvpole) priors |> List.rev in
  let make_tup pid =
    let pred = List.assoc pid prev in
    let pvar = PredVar.make pid (fst pred) in
    let (pvar_new, pvar_old) = mk_new_old_phis tmpl prev pvar in
    let imp = imply (pvar_new, pvar_old) pid pvpole |> approximate in
    let not_rev_imp = e_not_rev_imply (pvar_new, pvar_old) pid pvpole pred in
    let gt = Formula.mk_and imp not_rev_imp in
    let eq = eq (pvar_new, pvar_old) |> approximate in
    (gt, eq)
  in
  let equals = ref [] in
  let rec aux li acc =
    try
      match li with
      | [] -> acc
	      |> List.rev
	      |> Formula.bor
      | x::xs ->
	let (gt,_) = make_tup x in
	(if !equals = [] then
	   List.iter
	     (fun pid -> equals := (pid |> make_tup |> snd)::!equals) xs
	 else
	   equals := List.tl !equals);
	aux xs ((gt::(!equals)
		 |> Formula.band)::acc)
    with
      Not_found -> Format.printf "raised Not_found@."; Formula.mk_true
  in
  aux priors [Formula.mk_false]
let whole_constr =
  Logger.log_block4
    "PredVarPriority.whole_constr"
    whole_constr

(** @require total priority relation list (head is the highest priority) *)
let whole_constr_wo_approximation prev tmpl pvpole priors =
  let priors = List.filter (fun id -> List.mem_assoc id pvpole) priors |> List.rev in
  let make_tup pid =
    let pred = List.assoc pid prev in
    let pvar = PredVar.make pid (fst pred) in
    let (pvar_new, pvar_old) = mk_new_old_phis tmpl prev pvar in
    let imp = imply (pvar_new, pvar_old) pid pvpole in
    let not_rev_imp = e_not_rev_imply (pvar_new, pvar_old) pid pvpole pred in
    let gt = Formula.mk_and imp not_rev_imp in
    let eq = eq (pvar_new, pvar_old) in
    (gt, eq)
  in
  let equals = ref [] in
  let rec aux li acc =
    try
      match li with
      | [] -> acc
	      |> List.rev
	      |> Formula.bor
      | x::xs ->
	let (gt,_) = make_tup x in
	(if !equals = [] then
	   List.iter
	     (fun pid -> equals := (pid |> make_tup |> snd)::!equals) xs
	 else
	   equals := List.tl !equals);
	aux xs ((gt::(!equals)
		 |> Formula.band)::acc)
    with
      Not_found -> Format.printf "raised Not_found@."; Formula.mk_true
  in
  aux priors [Formula.mk_false]
let whole_constr_wo_approximation prev tmpl pvpole priors =
  Logger.log_block4
    "PredVarPriority.whole_constr_wo_approximation"
    whole_constr_wo_approximation prev tmpl pvpole priors

(*
(imply1) /\ ((not rev-imply1)
             \/ ((rev-imply1) /\
                    ((imply2) /\ ((not rev-imply2))))

Formula.mk_and imp (Formula.mk_or not_rev_imp
  (Formula.mk_and rev_imp (Formula.mk_and imp2 (Fomula.mk_or not_rev_imp2 false)))) *)
let whole_constr_opt prev tmpl pvpole priors =
  let priors = List.filter (fun id -> List.mem_assoc id prev) priors |> List.rev in
  if priors = [] then Formula.mk_true
  else
    begin
      List.fold_left
	(fun acc pid ->
           let pred = List.assoc pid prev in
           (* let pva = PredVar.make pid (fst pred) |> Pva.of_pvar in *)
           (* let (pvar_new, pvar_old) = (Pva.to_formula pva, PredSubst.lookup prev pva) in *)
           let pvar = PredVar.make pid (fst pred) in
           let (pvar_new, pvar_old) = mk_new_old_phis tmpl prev pvar in
           let imp = imply (pvar_new, pvar_old) pid pvpole |> approximate in
           let rev_imp = imply (pvar_old, pvar_new) pid pvpole |> approximate in
           let not_rev_imp = e_not_rev_imply (pvar_new, pvar_old) pid pvpole pred in
           let rev = Formula.mk_or not_rev_imp (Formula.mk_and rev_imp acc) in
           (* part of ((not rev-imply2) \/ (rev-imply2 /\ false)) *)
	   (* Format.printf *)
           (*   "id: %a@.old: %a@.@.new: %a@.@.imply: %a@.@.rev_imply: %a@.@.not_rev_imply: %a@.@." *)
           (*   Idnt.pr pid Formula.pr pvar_old Formula.pr pvar_new Formula.pr imp Formula.pr rev_imp Formula.pr not_rev_imp;  *)
           Formula.mk_and imp rev) (* part of (imply2 /\ ((not rev...)) *)
        Formula.mk_false priors
    end

(* for debugging *)
let not_same prev tmpl priors =
  let priors = List.filter (fun id -> List.mem_assoc id prev) priors in
  if priors = [] then Formula.mk_true
  else
    begin
      List.fold_left
        (fun acc pid ->
           let pred = List.assoc pid prev in
           (* let pva = PredVar.make pid (fst pred) |> Pva.of_pvar in *)
           (* let (pvar_new, pvar_old) = (Pva.to_formula pva, PredSubst.lookup prev pva) in *)
           let pvar = PredVar.make pid (fst pred) in
           let (pvar_new, pvar_old) = mk_new_old_phis tmpl prev pvar in
           let phi = eq (pvar_new, pvar_old) |> Formula.bnot |> approximate in
           Formula.mk_and acc phi)
        Formula.mk_true priors
    end

(* optimize:
   (imply1) /\ ((not rev-imply1)
             \/ ((rev-imply1) /\
                    ((imply2) /\ ((not rev-imply2) \/ (rev-imply2 /\ false)))

   Formula.mk_and imp (Formula.mk_or not_rev_imp (Formula.mk_and rev_imp (Formula.mk_and imp2 not_rev_imp2) *)
let whole_constr_wo_approximation_opt prev tmpl pvpole priors =
  let priors = List.filter (fun id -> List.mem_assoc id prev) priors in
  if priors = [] then Formula.mk_true
  else
    begin
      List.fold_left
	(fun acc pid ->
           let pred = List.assoc pid prev in
           (* let pva = PredVar.make pid (fst pred) |> Pva.of_pvar in *)
           (* let (pvar_new, pvar_old) = (Pva.to_formula pva, PredSubst.lookup prev pva) in *)
           let pvar = PredVar.make pid (fst pred) in
           let (pvar_new, pvar_old) = mk_new_old_phis tmpl prev pvar in
           let imp = imply (pvar_new, pvar_old) pid pvpole in
           let rev_imp = imply (pvar_old, pvar_new) pid pvpole in
           let not_rev_imp = e_not_rev_imply (pvar_new, pvar_old) pid pvpole pred in
           let rev = Formula.mk_or not_rev_imp (Formula.mk_and rev_imp acc) in
           (* part of ((not rev-imply2) \/ (rev-imply2 /\ false)) *)
	   (* Format.printf *)
           (*   "id: %a@.old: %a@.@.new: %a@.@.imply: %a@.@.rev_imply: %a@.@.not_rev_imply: %a@.@." *)
           (*   Idnt.pr pid Formula.pr pvar_old Formula.pr pvar_new Formula.pr imp Formula.pr rev_imp Formula.pr not_rev_imp;  *)
           Formula.mk_and imp rev) (* part of (imply2 /\ ((not rev...)) *)
        Formula.mk_false priors
    end
let whole_constr_wo_approximation_opt prev tmpl pvpole priors =
  Logger.log_block4
    "PredVarPriority.whole_constr_wo_approximation_opt"
    whole_constr_wo_approximation_opt prev tmpl pvpole priors

(** make HCCS to obtain a strictly weaker solution for x *)
let weaken sol x =
  let pred = List.assoc x sol in
  let tenv = fst pred in
  let pva = Pva.of_tenv x tenv in
  let pvar = PredVar.make x tenv in
  let phi = PredSubst.lookup sol pva in
  let hphi = Formula.bnot phi in
  let rev = HornClause.mk_def ~tenv ~hphi pvar [] Formula.mk_true in
  let imp = HornClause.mk_def pvar [] phi in
  [imp; rev]

