open Util
open Combinator

let pset_opt = ref 1

let remove_None xs =
  List.map Option.list_of xs
  |> List.concat

let nnf = NNF.of_formula >> NNF.formula_of

let rec or_simplify = function
  | [] -> []
  | x::xs -> 
    if List.exists (Formula.imply x >> SMTProver.is_valid_dyn) xs then
      or_simplify xs
    else x :: or_simplify xs
let or_simplify = or_simplify >> List.rev >> or_simplify

let bor2 phis =
  try
    phis
    |> or_simplify
    |> Formula.bor
  with Not_found ->
    Formula.mk_true

(* rename variables in HornClause *)
let rename p_predset_list hc =
  match HornClause.hpv_of hc with
  | None -> hc
  | Some pv ->
    let idnt = PredVar.idnt_of pv in
    let args = PredVar.args_of pv in
    let ids =
      List.assoc idnt p_predset_list
      |> List.hd
      |> fst
      |> List.map fst
    in
    let pv' = PredVar.make idnt (List.map2 (fun (_, ty) i -> i, ty) args ids) in
    let tsub = List.combine (List.map fst args) (List.map Term.mk_var ids) in
    HornClause.map_head
      (fun _ -> HornClause.mk_head (Some pv'))
      (HornClause.subst_varsB tsub hc)

let polarity fmla =
  let rec disj_loop fs =
    let fs' = List.map nnf fs
              |> List.map Formula.disjuncts_of
              |> List.concat in
    if fs = fs' then fs
    else disj_loop fs'
  in
  let rec conj_loop fs =
    let fs' = List.map nnf fs
              |> List.map Formula.conjuncts_of
              |> List.concat in
    if fs = fs' then fs
    else conj_loop fs'
  in
  let rec loop fs =
    let fs' = (disj_loop >> conj_loop) fs in
    if fs = fs' then fs
    else loop fs'
  in
  let unk = Formula.fvs_bool fmla (*todo?*)
            |> List.map Term.mk_var
            |> List.map Formula.of_term
  in
  let elimf = Formula.elim_imply_iff fmla in
  (*  Format.printf "@.elim:@, %a @," Formula.pr elimf; *)
  let fs = loop [elimf] in
  (*  Format.printf "@.fs:@, %a @," Formula.pr_list fs; *)
  List.fold_right (fun u ts -> (List.mem u fs, u)::ts) unk [] 
  |> List.partition fst
  |> Pair.lift (List.map (snd >> Formula.fvs) >> List.concat)


(* Optimal Negative Solution *)
(* requirement: all unknown are negative in phi
   function _Q returns candiddates considering sigma_t *)
let breadth_search _Q tsub =
  let unks = List.map fst tsub in
  let unk_qs_s = List.map (fun u -> u, _Q u) unks in
  (* search next level *)
  let rec loop pre = function
    | [] -> []
    | (i, t)::post ->
      let qs = List.assoc i unk_qs_s in
      let tf = Formula.of_term t in
      let f1 q =
        let t' = Formula.mk_and tf q
                 |> Formula.term_of in
        pre @ (i, t')::post      
      in
      List.map f1 qs @ loop (pre@[i, t]) post
  in
  loop [] tsub

let opt_neg_sol (phi:Formula.t) (_Q:Idnt.t -> Formula.t list) =
  Format.printf "@.opt_neg_sol:%a@." Formula.pr phi;
  let unks = Formula.fvs_bool phi (*todo?*)in
  let tsub0 = List.map (fun u -> u, Formula.term_of Formula.mk_true) unks in
  (*Format.printf "@. %a@." TermSubst.pr tsub0;*)
  let phi' = Formula.subst tsub0 phi in
  (*Format.printf "@.is_valid1:%a@." Formula.pr phi';*)
  if SMTProver.is_valid_dyn phi' then [tsub0]
  else
    let rec loop depth tsubs =
      let tsub's_set = List.map (breadth_search _Q) tsubs
                       |> List.concat in
      let f2 tsub' =
        if Formula.subst tsub' phi
           |> (fun f -> (*Format.printf "@.is_valid2(%d):%a@." depth Formula.pr f;*)
               SMTProver.is_valid_dyn f)
        then Some tsub'
        else None
      in
      Format.printf "@. len: %d" (List.length tsub's_set);
      let checklist = List.map f2 tsub's_set in
      if depth < 4 then
        remove_None checklist
        @ loop (depth + 1) tsub's_set
      else []
    in loop 1 [tsub0]

(*
(unknown * (idnt * int)) list
  template: 0 => 1
*)
let unk_assoc = ref []
let b_assoc = ref []

let pick unk p_psets =
  List.assoc unk !unk_assoc
  |> fst
  |> flip List.assoc p_psets

let gen_temp idnt tmp =
  let i = ref 0 in
  Formula.fold
    (object
      method fatom atm =
        Idnt.new_var ()
        |> (fun v ->
            unk_assoc := (v, (idnt, !i))::!unk_assoc;
            incr i;
            Formula.mk_var v [])
      method ftrue () =
        raise (Global.NotImplemented "CFPPAHCCSSolver.gen_temp")
      method ffalse () =
        raise (Global.NotImplemented "CFPPAHCCSSolver.gen_temp")
      method fnot r = Formula.bnot r
      method fand r1 r2 = Formula.mk_and r1 r2
      method for_ r1 r2 = Formula.mk_or r1 r2
      method fimply r1 r2 = Formula.imply r1 r2
      method fiff r1 r2 = Formula.mk_iff r1 r2
      method fforall _ _ =
        raise (Global.NotImplemented "CFPPAHCCSSolver.gen_temp")
      method fexists _ _ =
        raise (Global.NotImplemented "CFPPAHCCSSolver.gen_temp")
      method fbox idx r1 = assert false
      method fdiamond idx r1 = assert false
      method fmu x r1 = assert false
      method fnu x r1 = assert false
    end)
    tmp

let mk_sol p_tmps uselist =
  let f p tmp =
    let ul =
      List.find_all (fst >> (=) p) uselist
      |> List.map snd
    in
    let i = ref 0 in
    Formula.fold
      (object
        method fatom atm =
          List.find_all (fun ((j, b), _) -> j = !i && b) ul
          |> (fun xs -> incr i; xs)
          |> List.map snd
          |> Formula.band
        method ftrue () =
          raise (Global.NotImplemented "CFPPAHCCSSolver.mk_sol")
        method ffalse () =
          raise (Global.NotImplemented "CFPPAHCCSSolver.mk_sol")
        method fnot r = Formula.bnot r
        method fand r1 r2 = Formula.mk_and r1 r2
        method for_ r1 r2 = Formula.mk_or r1 r2
        method fimply r1 r2 = Formula.imply r1 r2
        method fiff r1 r2 = Formula.mk_iff r1 r2
        method fforall _ _ =
          raise (Global.NotImplemented "CFPPAHCCSSolver.mk_sol")
        method fexists _ _ =
          raise (Global.NotImplemented "CFPPAHCCSSolver.mk_sol")
        method fbox idx r1 = assert false
        method fdiamond idx r1 = assert false
        method fmu x r1 = assert false
        method fnu x r1 = assert false
      end)
      tmp
  in
  List.map (fun (p, tmp) -> p, f p tmp) p_tmps

(* make sigma_t and formula with template *)
let mk_temp_fmla p_newids p_tmp hc :(Idnt.t * TermSubst.t) list * Formula.t =
  let f body =
    let bpvas = HornClause.pvas_of_body body in
    let phi = HornClause.phi_of_body body in
    let rec loop un_tsub phi = function
      | [] -> (un_tsub, phi)
      | pva::rest ->
        let idnt = Pva.idnt_of pva in
        let args = Pva.args_of pva |> List.map fst in
        let temp = List.assoc idnt p_tmp |> gen_temp idnt in
        let unks = Formula.fvs temp in
        let newids = List.assoc idnt p_newids in
        let un_tsub' =
          List.map (fun fv -> (fv, List.combine newids args)) unks
          @ un_tsub
        in
        loop un_tsub' (Formula.mk_and phi temp) rest
    in loop [] phi bpvas
  in
  let (un_tsub, fbody) = f (HornClause.body_of hc) in
  match HornClause.hpv_of hc with
  | None -> un_tsub, Formula.imply fbody Formula.mk_false
  | Some pv ->
    let idnt = PredVar.idnt_of pv in
    let args = Pva.of_pvar pv
               |> Pva.args_of
               |> List.map fst in
    let temp = List.assoc idnt p_tmp |> gen_temp idnt in
    let unks = Formula.fvs temp in
    let newids = List.assoc idnt p_newids in
    let un_tsub' =
      List.map (fun fv -> (fv, List.combine newids args)) unks
      @ un_tsub
    in
    un_tsub', Formula.imply fbody temp

let parse_atom str = 
  HCCSParser.atom
    HCCSLexer.token
    (Lexing.from_string str)

let parse_temp str = 
  MLRefparser.temp
    MLReflexer.token
    (Lexing.from_string str)

(* make pridicate sets *)
(* (Idnt.t * Formula.t) list *)
let mk_pset p_predset_list p_gfs auto =
  let pvs =
    List.map
      (fun (pid, preds) ->
         let tenv = List.hd preds |> fst in
         PredVar.make pid tenv)
      p_predset_list
  in
  let pred_to_formulas pred =
    match !pset_opt with
    | 0 ->
      snd pred
      |> Formula.atoms
      |> List.map Formula.of_atom
      |> List.map FormulaSimplifier.simplify
    | 1 ->
      snd pred
      |> FormulaSimplifier.simplify
      |> (fun x -> [x])
    | _ -> failwith "pset error"
  in
  let p_psets =
    List.map
      (Pair.map_snd
         (List.map pred_to_formulas
          >> List.concat
          >> List.unique
            ~cmp:(fun f1 f2 -> Formula.mk_iff f1 f2 |> SMTProver.is_valid_dyn)
          >> flip List.remove Formula.mk_true
          >> flip List.remove Formula.mk_false))
      p_predset_list
  in
  let rec loop pv (p, ps) =
    Format.printf "@.predicate set(%a):@." Idnt.pr p;
    List.iter
      (Format.printf "%a@.  -> %a, @." PredVar.pr pv Formula.pr)
      ps;
    if auto then p, ps
    else
      begin
        Format.printf "@.Add?(y/n/goal)@.";
        let ans = read_line () in
        if ans = "y" then
          begin
            Format.printf "@.add predicate(%a):@.%a -> @." Idnt.pr p PredVar.pr pv;
            let a = parse_atom (read_line ()) in
            loop pv (p, a::ps)
          end
        else if ans = "goal" then
          loop pv (p, ps @ List.assoc p p_gfs)
        else p, ps
      end
  in List.map2 loop pvs p_psets

let mk_Q' sigma_t p_psets = fun idnt ->
  let pset = pick idnt p_psets in
  let tsub =
    try List.assoc idnt sigma_t with Not_found -> []
  in
  List.map (Formula.subst tsub) pset

let mk_sigma_ij (rho: Idnt.t) (q: Formula.t) = fun idnt ->
  if idnt = rho then q
  else Formula.mk_true

let mk_sigma = fun idnt ->
  Formula.mk_true

(* except sigma_t ! *)
let app_sigma fmla sigma =
  let rhos, _ = polarity fmla in
  let tsub = List.map (fun rho -> rho, sigma rho |> Formula.term_of) rhos in
  Formula.subst tsub fmla

let s_ij p_psets sigma_t fmla rho q =
  let sigma_ij = mk_sigma_ij rho q in
  let vc_sig_ij = app_sigma fmla sigma_ij in
  let _Q' = mk_Q' sigma_t p_psets in
  opt_neg_sol vc_sig_ij _Q'

let s p_psets sigma_t fmla =
  let sigma = mk_sigma in
  let vc_sig = app_sigma fmla sigma in
  let _Q' = mk_Q' sigma_t p_psets in
  opt_neg_sol vc_sig _Q'

let orig (eta:Idnt.t) :(Idnt.t * int) =
  List.assoc eta !unk_assoc

let mk_sigma_t_inv (sigma_t: (Idnt.t * TermSubst.t) list ) p_psets =
  let mapf (v, tsub) =
    let pset = pick v p_psets in
    let pset_sig = List.map (Formula.subst tsub) pset in
    let pos fmla =
      Format.printf "@.fmla:@, %a @," Formula.pr fmla;
      Format.printf "@.pset_sig:@, %a @," Formula.pr_list pset_sig;
      (*      let conjs =
              match !pset_opt with
              | 0 -> Formula.conjuncts_of fmla
              | 1 -> [fmla]
              | _ -> failwith "pset error"
              in
              let f fmla' =
              if Formula.is_true fmla' then -1
              else
              try
              List.findi
                (fun i p ->
              (*Format.printf "@.%a <=> %a@." Formula.pr p Formula.pr fmla';*)
              SMTProver.is_valid_dyn (Formula.mk_iff p fmla'))
                pset_sig
                  |> fst
              with Not_found -> -200000
              in
              List.map f conjs
      *)
      match !pset_opt with
      | 0 ->
        let conjs = Formula.conjuncts_of fmla in
        let f fmla' =
          if Formula.is_true fmla' then -1
          else
            try
              List.findi
                (fun i p ->
                   (*Format.printf "@.%a <=> %a@." Formula.pr p Formula.pr fmla';*)
                   SMTProver.is_valid_dyn (Formula.mk_iff p fmla'))
                pset_sig
              |> fst
            with Not_found -> -200000
        in
        List.map f conjs
      | 1 ->
        if Formula.is_true fmla then [-1]
        else
          let dest_and f = Formula.let_and f (Pair.make) in
          let rec destruct f =
            try
              List.findi
                (fun i p ->
                   (*Format.printf "@.%a <=> %a@." Formula.pr p Formula.pr fmla';*)
                   SMTProver.is_valid_dyn (Formula.mk_iff p f))
                pset_sig
              |> fst
              |> (fun x -> [x])
            with Not_found ->
              let f1, f2 = dest_and f in
              destruct f1 @ destruct f2
          in destruct fmla
      | _ -> failwith "pset error"
    in
    v, pos >> List.map (function -1 -> Formula.mk_true | i -> List.nth pset i)
  in
  List.map mapf sigma_t  

let app_sigma_t_inv v q sigma_t_inv =
  let f = List.assoc v sigma_t_inv in
  f q

let mk_b (p, i) fmla =
  let pstr = Idnt.string_of p in
  let istr = string_of_int i in
  let fstr = Formula.string_of fmla in
  let str = "b(" ^ pstr ^ ":" ^ istr ^ "," ^ fstr ^")" in
  (*  Format.printf "@.%s@." str;*)
  try
    List.map (fun (x, y) -> y, x) !b_assoc
    |> List.assoc (p, (i, fmla))
  with
  | Not_found -> 
    let b = Formula.mk_var (Idnt.new_var ()) [] in
    b_assoc := (b, (p, (i, fmla)))::!b_assoc;
    b

(* [(0, [q1, q2, q3]); (1, [q2; q4])] *)
let bc sigma_t_inv tsubs =
  let tsubs' =
    let rec loop ts =
      let ts' = List.remove_if (snd >> Formula.of_term >> Formula.is_true) ts in
      if ts = ts' then ts
      else loop ts'
    in loop tsubs
  in
  List.map
    (fun (eta, q) ->
       let fq = Formula.of_term q in
       orig eta,
       app_sigma_t_inv eta fq sigma_t_inv) tsubs'
  |> List.map
    (fun (pi, qs) ->
       List.map (mk_b pi) qs)
  |> List.concat
  |> Formula.band


let psi p_psets p_newids p_tmps hc =
  List.iter
    (fun (p, ps) ->
       Format.printf "@.predicate set(%a):@, %a @." Idnt.pr p Formula.pr_list ps)
    p_psets;
  let sigma_t, hctmp = mk_temp_fmla p_newids p_tmps hc in
  Format.printf "@.HC(using template):@, %a @." Formula.pr hctmp;
  let sigma_t_inv = mk_sigma_t_inv sigma_t p_psets in
  let rhos, etas = polarity hctmp in
  Format.printf "@.rhos:@, %a @." Idnt.pr_list rhos;
  Format.printf "@.etas:@, %a @." Idnt.pr_list etas;
  let _S = s p_psets sigma_t hctmp in
  let disj1 = 
    List.map (bc sigma_t_inv) _S
    |> (fun fs ->
        let fs' = bor2 fs in assert(Formula.mk_iff (Formula.bor fs) fs' |> SMTProver.is_valid_dyn); fs')
  in
  let _Q' = mk_Q' sigma_t p_psets in
  let b_imp rhoi qj =
    let pi = orig rhoi in
    match app_sigma_t_inv rhoi qj sigma_t_inv with
    | [q] ->
      Format.printf "@.-----------------@.";
      let b = mk_b pi q in
      let _S_ij = s_ij p_psets sigma_t hctmp rhoi qj in
      let disj2 = List.map (bc sigma_t_inv) _S_ij
                  |> (fun fs -> let fs' = bor2 fs in assert(Formula.mk_iff (Formula.bor fs) fs' |> SMTProver.is_valid_dyn); fs') in
      Formula.imply b disj2
    | _ -> failwith "error"
  in
  let f2 rhoi =
    let qjs = _Q' rhoi in
    Format.printf "@.qjs: %a@." Formula.pr_list qjs;
    List.map (b_imp rhoi) qjs
    |> Formula.band in
  let conj = List.map f2 rhos
             |> Formula.band in
  Format.printf "@.disj1:@, %a @," Formula.pr disj1;
  Format.printf "@.conj:@, %a @," Formula.pr conj;
  let f = Formula.mk_and disj1 conj in
  (*  Format.printf "@.psi(%a):@, %a @." HornClause.pr hc Formula.pr f; *)
  Formula.fvs f
  |> List.map (Term.mk_var >> Formula.of_term)
  |> List.map (fun v -> v, List.assoc v !b_assoc)
  |> List.split
  |> (fun (vs, p_ifs) -> Format.printf "@.----@.";
       List.iter2 (fun v (p, (i, f)) -> Format.printf "@.%a = b(%a:%d, %a)@." Formula.pr v Idnt.pr p i Formula.pr f) vs p_ifs);
  unk_assoc := [];
  f

let solve ?(auto=false) c p_predset_list =
  let c' = List.map (rename p_predset_list) c in  
  Format.printf "@.HCCS(rename):@, %a @," HCCS.pr c';
  let p_gfs =
    let gfs =
      HCCS.goals_of c'
      |> List.map HornClause.bphi_of
      |> List.map Formula.conjuncts_of
      |> List.concat
      |> List.unique
    in
    List.map
      (fun (pid, preds) ->
         let ids = List.hd preds |> fst |> List.map fst in
         pid,
         List.filter
           (fun f -> List.for_all (flip List.mem ids) (Formula.fvs f))
           gfs) 
      p_predset_list
  in
  let p_psets = mk_pset p_predset_list p_gfs auto in
  let p_newids = List.map
      (Pair.map_snd (List.hd >> fst >> List.map fst))
      p_predset_list in
  let p_tmps = List.map
      (fun (p, _) ->
         Format.printf "@.template(%a): @." Idnt.pr p;
         if auto then p, parse_temp "v"
         else p, parse_temp (read_line ()))
      p_psets in
  let psi_prog = List.map (psi p_psets p_newids p_tmps) c'
                 |> Formula.band in
  let tseitin = TseitinEncoding.encode psi_prog in
  Format.printf "@.psi_prog:@, %a @." Formula.pr psi_prog;
  (*  Format.printf "@.Tseitin encode:@, %a @." Formula.pr_list tseitin;*)

  let v_str = Formula.fvs (Formula.band tseitin)
              |> List.map (Idnt.string_of >> (^) "v ")
              |> String.concat "\n" in
  let string_of_lit = Literal.formula_of
    >> Formula.string_of in
  let cnf_str fmla = DNF.of_formula fmla
                     |> DNF.disjunction_of
                     |> List.map (Literal.of_formula)
                     |> List.map
                       (fun l ->
                          if Literal.is_pos l then
                            string_of_lit l
                          else "-" ^ (string_of_lit (Literal.bnot l)))
                     |> List.fold_left
                       (fun str l -> str ^ " " ^ l) "" in
  let c_str =
    if Formula.is_false psi_prog then "v dummy\nc dummy\nc -dummy\n"
    else
      List.concat_map (CNF.of_formula >> CNF.to_conjunction) tseitin
      |> List.map (cnf_str >> (^) "c")
      |> String.concat "\n"
  in
  let str = v_str ^ "\n" ^ c_str in
  (*Format.printf "@. %s @." str;*)
  let filename = "tmp.formula" in
  let oc = open_out filename in
  Printf.fprintf oc "%s" str;
  close_out oc;
  let inds = List.map fst !b_assoc in
  let satans = SATSolver.solve_minisat_dyn filename
               |> List.filter (fst >> flip List.mem inds)
               |> List.sort ~cmp:(fun p1 p2 -> compare (fst p1) (fst p2)) in
  Format.printf "[variable assignment]@.";
  List.iter (fun (f, b) -> Format.printf "%a = %b @." Formula.pr f b) satans;
  let uselist = List.map
      (fun (f, b) ->
         let p, (i, fmla) = List.assoc f !b_assoc in
         p, ((i, b), fmla))
      satans in
  List.iter (fun (p, ((i, b), f)) -> if b then (Format.printf "%a:%d, %a,@.") Idnt.pr p i Formula.pr f) uselist;
  if uselist = [] then []
  else
    let p_sol = mk_sol p_tmps uselist in
    let p_tenv = List.map (Pair.map_snd (List.hd >> fst)) p_predset_list in
    let psub = List.map (fun (p, tenv) -> p, (tenv, List.assoc p p_sol)) p_tenv in
    b_assoc := [];
    (* solution check *)
    let solcheck = HCCS.subst psub c
                   |> HCCS.simplify_full []
    in
    Format.printf "@.HCCS:@, %a@." HCCS.pr c;
    Format.printf "@.check:@, %a@." HCCS.pr solcheck;
    assert (HCCS.is_solution c psub);
    psub
