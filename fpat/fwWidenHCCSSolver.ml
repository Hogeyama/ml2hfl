open Util
open Combinator
open CunAtom

(** A dag HCCS solver based on forward abstract interpretation with widening *)

let threshold = ref 3

(* transform Horn clause set into Hyper graph *)
type hg = { nodes : TypEnv.t;
            edges : (PredVar.t list * PredVar.t list * Formula.t) list }
(* edges means (PVar (<-) PVars (with) phi) *)

exception Not_canonical

let pr_edge ppf (goals, atms, phi) =
  if goals <> [] then
    Format.fprintf ppf "(%a <- " (List.pr PredVar.pr ",@ ") goals;
  if atms <> [] then
    Format.fprintf ppf "(%a)" (List.pr PredVar.pr ",@ ") atms;
  Format.fprintf ppf ")@,@[<hov2>func:@[ %a@]@]." Formula.pr phi

let pr ppf hg =
  Format.fprintf ppf "@[<hov2>hg@ =@ @[<hov>";
  if hg.nodes <> [] then
    Format.fprintf ppf "nodes:@[ %a@]@," TypEnv.pr hg.nodes;
  if hg.edges <> [] then
    Format.fprintf ppf "edges:@[ %a@]" (List.pr pr_edge "@,") hg.edges;
  Format.fprintf ppf "@]@]@,"

let pvar_of atm =
  let var_of_term = function
    | Term.Var(x) -> x
    | _ -> raise Not_canonical in
  List.map (Pair.map_fst var_of_term) (Pva.args_of atm)
  |> PredVar.make (Pva.idnt_of atm)

(* Horn clause set to Directed hyper graph *)
let hg_of edges =
  HornClause.fold
    (fun atms phi ->
       let pvars : PredVar.t list = List.map (fun atm -> pvar_of atm) atms in
       ([], pvars, phi) :: edges)
    (fun pvar atms phi ->
       let pvars : PredVar.t list = List.map (fun atm -> pvar_of atm) atms in
       ([pvar], pvars, phi) :: edges)

let hg_of hcs =
  let nodes = HCCS.tenv hcs in
  let edges =
    hcs
    |> HCCS.normalize2
    |> List.fold_left (fun hg hc -> hg_of hg hc) []
  in
  { nodes=nodes; edges=edges }

(** Solver *)

let make_initial nodes =
  let make_elem node =
    let pid, simty = node in
    let args_ty, _ = Type.args_ret simty in (* get types of arguments *)
    let xtys = List.map (fun ty -> Idnt.new_var (), ty) args_ty in 
    PredSubst.mk_elem pid xtys Formula.mk_false
  in
  List.map make_elem nodes

type trfun = (Idnt.t list * Formula.t) list

(* make trfun *)
let make_trfuns edges theta =
  let make_elem (pvar_goals, body, phi) =
    let app formula pvar =
      let atm = Pva.of_pvar pvar in
      let tr_phi = PredSubst.lookup_fresh theta atm in
      Formula.band (tr_phi::[formula])
    in
    let formula = List.fold_left app phi body in
    (* rename *)
    let subst_args = (* (old_args = new_args) *)
      let f pvar =
        let pid = PredVar.idnt_of pvar in
        let old_args = PredVar.args_of pvar in
        let new_args = PredSubst.args_of pid theta in
        List.map2
          (fun (old,_) (_new,_) ->
             Atom.eq Type.mk_int (Term.mk_var old) (Term.mk_var _new)
             |> Formula.of_atom)
          old_args new_args
        |> Formula.band 
      in
      List.map f pvar_goals
    in
    let renamed_formula =
      Formula.band (formula::subst_args)
      |> FormulaSimplifier.simplify
    in
    let pids = List.map (fun pvar -> PredVar.idnt_of pvar) pvar_goals in
    pids, renamed_formula
  in
  List.map make_elem edges

(* update function. return new theta *)
let update theta edges =
  let trfuns = make_trfuns edges theta in
  let gather goal = (* gather edges having same goal from trfuns *)
    List.filter (fun (pids, _) -> pids = goal)
  in (* return filtered frfuns *)
  let new_phi xtys pids =
    List.fold_left
      (fun x (_, y) ->
         let simplified_y = QelimBody.elim_int_vars_full (List.map fst xtys) y in
         let x = QelimBody.elim_int_vars_full (List.map fst xtys) x in
         Formula.bor (simplified_y::[x]))
      Formula.mk_false
      (gather pids trfuns)
    |> FormulaSimplifier.simplify
  in
  let make_new_elem (pid, (xtys, phi)) =
    let simplified_new_phi =
      new_phi xtys [pid] |> QelimBody.elim_int_vars_full (List.map fst xtys)
    in
    PredSubst.mk_elem pid xtys simplified_new_phi
    (*@! upto one pvar to one goal *)
  in
  List.map make_new_elem theta

(* solve : hcs -> PredSubst.t *)
let solve ?(wide = true) hcs =
  let hg = hg_of hcs in
  Logger.printf "%a@," pr hg;
  let initial_theta = make_initial hg.nodes in
  Logger.printf "initial: %a@," PredSubst.pr initial_theta;
  let rec loop count theta =
    let eq old_theta new_theta =
      (* if new -> old, new <-> old (because new <- old is trivial) *)
      let eq_elem t1 t2 =
        let (pid1, (xtys1, phi1)) = t1 in
        let (pid2, (xtys2, phi2)) = t2 in
        SMTProver.implies_dyn [phi2] [phi1]
      in
      List.for_all2 eq_elem old_theta new_theta
    in
    let widening old_theta new_theta =
      let widen =
        List.map2
          (fun (pid1, (xtys1, phi1)) (_, (_, phi2)) ->
             (pid1, (xtys1, Polyhedron.widen_list_dyn [phi1; phi2])))
          old_theta
          new_theta
      in
      Logger.printf2 "widening(%a): %a@," Integer.pr count PredSubst.pr widen;
      widen
    in
    let validity theta =
      if HCCS.is_solution hcs theta then
        (Logger.printf "solution: %a@," PredSubst.pr theta;
         Logger.printf0 "\x1b[32m\x1b[1msat\x1b[0m@,";
         theta)
      else
        (Logger.printf0 "Specification is not satisfied.@,";
         Logger.printf0 "\x1b[1m\x1b[31munknown\x1b[0m@,";
         raise HCCSSolver.Unknown)
    in
    let new_theta = update theta hg.edges in
    Logger.printf2 "updated(%a): %a@," Integer.pr count PredSubst.pr new_theta;
    let new_theta =
      if count > !threshold && wide (* threshold *)
      then widening theta new_theta
      else new_theta
    in 
    if count <> 1 && eq theta new_theta
    then validity new_theta
    else loop (count+1) new_theta
  in
  loop 1 initial_theta
