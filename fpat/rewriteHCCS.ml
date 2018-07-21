open Util
open Combinator

type lemma =
  | Hypo of (Idnt.t list * Pva.t * HornClause.body)
  | Lem of (Idnt.t list * Pva.t * HornClause.body)

(** rewriting *)

let union_arg arg1 arg2 =
  arg1 @ List.fold_left (fun l x -> List.remove l x) arg2 arg1

let lmax = List.fold_left max neg_infinity

let remove_nth l n = List.remove l (List.nth l n)

let rec argmax (p, m) = function
  | [] -> p
  | (i, x)::rest ->
    if x > m then argmax (i, x) rest
    else argmax (p, m) rest
let argmax = function
  | [] -> failwith "argmax: list is empty"
  | (i, x)::rest -> argmax (i, x) rest

let search l x =
  let rec loop i = function
    | [] -> -1 (* failwith "search" *)
    | y::rest -> if x = y then i else loop (i + 1) rest
  in loop 0 l

let diff l1 l2 =
  List.fold_left List.remove l1 l2

let subset l1 l2 =
  let l' = List.fold_left List.remove l2 l1 in
  List.length l' >= List.length l2 - List.length l1

let formula_of_tsub tsub =
  let fs =
    List.map
      (fun (id, t) ->
         Formula.eq Type.mk_int
           (Term.mk_var id)
           t)
      tsub
  in Formula.band fs

let idnt_subset pvas1 pvas2 =
  subset
    (List.map Pva.idnt_of pvas1)
    (List.map Pva.idnt_of pvas2)

let remove_None xs =
  List.fold_right
    (fun x res ->
       match x with
         Some y -> y::res
       | None -> res)
    xs
    []

let rec remove_notassoc_all quas = function
  | [] -> []
  | (id, t)::rest ->
    if List.mem id quas then (id, t) :: remove_notassoc_all quas rest
    else remove_notassoc_all quas rest

let rec remove_assoc_all quas = function
  | [] -> []
  | (id, t)::rest ->
    if List.mem id quas then remove_assoc_all quas rest
    else (id, t) :: remove_assoc_all quas rest

let rec assoc_all quas tsub =
  let f = remove_notassoc_all quas tsub in
  let s = remove_assoc_all quas tsub in
  (f, s)

let rec transpose = function
  | [] -> []
  | [l] -> List.map (fun x -> [x]) l
  | v :: l -> List.map2 (fun x y -> x::y) v (transpose l)

let rec combinate f = function
  | [xs] -> xs
  | xs :: rest ->
    List.concat
      (List.map
         (fun x -> List.map (fun y -> f x y) (combinate f rest))
         xs)
let combinate f = function
  | [] -> []
  | x -> combinate f x

let twoexists f l =
  List.exists f (List.remove_if f l)

let rec rem_loop = function
  | [] -> []
  | hc::rest ->
    match HornClause.hpv_of hc with
      None -> hc :: rem_loop rest
    | Some pv -> 
      if
        [Pva.of_pvar pv] = HornClause.bpvas_of hc
        && Formula.is_true (HornClause.bphi_of hc)
      then rem_loop rest
      else hc :: rem_loop rest

let forming =
  (*  HCCS.simplify_full []*)
  HCCS.simplify_lv2
  (*   HCCS.normalize2 ~force:false*)
  >> HCCS.simplify_lv2

let forming_full =
  HCCS.simplify_lv2
  >> HCCS.simplify_full []
  >> HCCS.simplify_lv2
(*  >> HCCS.normalize2 ~force:false *)

(*
  同じ述語変数内の引数同士をチェック。
  Oのタグを持つ引数でペアがある＝＞rename
*)
let rwlist l n r =
  let rec loop i = function
    | [] -> []
    | x::rest when i = n -> r::rest
    | x::rest -> x::loop (i + 1) rest
  in loop 0 l

let pva_rename pva tag =
  let idnt = Pva.idnt_of pva in
  let args = Pva.args_of pva in
  let _, couples, _ = 
    args
    |> List.fold_left
      (fun (i, ps, xs) arg ->
         let j = search xs arg in
         if j <> -1 then i + 1, (i , j) :: ps, xs @ [arg]
         else i + 1, ps, xs @ [arg])
      (0, [], [])
  in
  List.fold_left
    (fun (newargs, f) (i, j) ->
       let tti = List.nth args i in
       let tti' = TypTerm.fresh tti in
       let ttj = List.nth args j in
       let ttj' = TypTerm.fresh ttj in
       match tag idnt i, tag idnt j with
       | Tag.O, Tag.O ->
         rwlist (rwlist newargs i tti') j ttj',
         Formula.mk_and f (Formula.eq_tt tti' ttj')
       | Tag.O, _ ->
         rwlist newargs i tti',
         Formula.mk_and f (Formula.eq_tt tti' ttj)
       | _, Tag.O ->
         rwlist newargs j ttj',
         Formula.mk_and f (Formula.eq_tt tti ttj')
       | _, _ -> newargs, f)
    (args, Formula.mk_true)
    couples
  |> fun (args', f) -> Pva.make idnt args', f

let hc_rename hc tag =
  let pvas = HornClause.bpvas_of hc in
  let pvas', phis =
    List.map (flip pva_rename tag) pvas
    |> List.split
  in
  HornClause.body_map (fun _ -> pvas') (fun phi -> Formula.band phis |> Formula.mk_and phi)
  |> fun f -> HornClause.map_body f hc

let forming_hc hc =
  match forming [hc] with
  | [] -> HornClause.mk_goal [] Formula.mk_false
  | [hc'] -> hc'

let homogenize2_body body =
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  HornClause.mk_goal pvas phi
  |> HornClause.normalize2 ~force:false
  |> HornClause.body_of

let forming_body body =
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  HornClause.mk_goal pvas phi
  |> forming_hc
  |> HornClause.body_of

let forming_bodies bodies =
  let pvass = List.map (HornClause.pvas_of_body) bodies in
  let phis = List.map (HornClause.phi_of_body) bodies in
  List.map2 HornClause.mk_goal pvass phis
  |> forming
  |> List.map HornClause.body_of

let subst_body sbst body =
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  HornClause.mk_goal pvas phi
  |> HornClause.subst_varsB sbst
  |> HornClause.body_of

let map_phi_body f body =
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  HornClause.mk_goal pvas phi
  |> HornClause.map_phi f
  |> HornClause.body_of

let print_psi (quas, r, body) =
  Format.printf "@,for_all[%a]. " Idnt.pr_list quas;
  Format.printf "@,%a  <=> " Pva.pr r;
  Format.printf "@,  %a@," HornClause.pr_body body

let print_lem = function
  | Hypo psi ->
    Format.printf "@,induction hypothisis:";
    print_psi psi
  | Lem psi ->
    Format.printf "@,lemma: @,";
    print_psi psi

let rw_tag pva1 pva2 pva3 tag =
  let p1 = Pva.idnt_of pva1 in
  let p2 = Pva.idnt_of pva2 in
  let p3 = Pva.idnt_of pva3 in
  let arg1 = Pva.args_of pva1 in
  let arg2 = Pva.args_of pva2 in
  let arg3 = Pva.args_of pva3 in
  let newtag (i, j) = 
    if i = -1 then tag p2 j
    else if j = -1 then tag p1 i
    else 
      match tag p1 i, tag p2 j with
      | Tag.O, _ | _, Tag.O -> Tag.O
      | Tag.Im, _ | _, Tag.Im -> Tag.Im
      | _ -> Tag.Ip
  in
  let addtag r rtgs tg = 
    fun p i -> if p = r then List.nth rtgs i else tg p i
  in
  Format.printf "@,  %a@," TypTerm.pr_list arg1;
  Format.printf "@,  %a@," TypTerm.pr_list arg2;
  Format.printf "@,  %a@," TypTerm.pr_list arg3;
  List.map
    (fun a -> search arg1 a, search arg2 a)
    (*      ((try
            search arg1 a
            with Failure "search" -> -1)
            ,
            (try
            search arg2 a
            with Failure "search" -> -1)) *)
    arg3
  |> List.map newtag
  |> (fun rtgs -> addtag p3 rtgs tag)

let is_rec hc =
  (HornClause.nodeH hc, HornClause.nodesB hc)
  |> fun (Some idh, idbs) -> 
  List.exists ((=) idh) (remove_None idbs)

let rec unique_mem = function
  | x::xs ->
    if List.mem x xs then
      unique_mem (List.remove_all xs x)    
    else x :: unique_mem xs
  | [] -> []

let onepat_of hccs =
  let unique_ids =
    List.map HornClause.nodeH hccs
    |> unique_mem
    |> remove_None
  in
  let f hc =
    match HornClause.nodeH hc with
      Some id -> List.mem id unique_ids
    | None -> false
  in
  List.filter f hccs

let onepat_reduct hccs body =
  let reduct pva =
    let onepat_ids =
      onepat_of hccs
      |> List.map HornClause.nodeH
      |> remove_None
    in
    if List.mem (Pva.idnt_of pva) onepat_ids then
      let defc =
        List.find
          (HornClause.nodeH >> (=) (Some (Pva.idnt_of pva)))
          hccs
      in
      let hd =
        HornClause.hpv_of defc
        |> function Some x -> x
      in
      let bd = HornClause.body_of defc in
      let subst = Pva.pat_match hd pva in
      subst_body subst bd
    else HornClause.mk_body [pva] Formula.mk_true
  in
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  List.map reduct pvas
  |> HornClause.and_body
  |> HornClause.body_map id (Formula.mk_and phi)

let algo_goal gccs tag =
  let algo_rank pva1 pva2 =
    let rank i1 i2 =
      let p1 = Pva.idnt_of pva1 in
      let p2 = Pva.idnt_of pva2 in
      match tag p1 i1, tag p2 i2 with
      | Tag.Im, Tag.Im -> 1.
      | Tag.Im, Tag.O  -> 1.
      | Tag.O, Tag.Im  -> 1.
      | Tag.O , Tag.O  -> neg_infinity
      | _      -> 0.
    in
    let pos i arg = i , arg in
    let posarg1 = List.mapi pos (Pva.args_of pva1) in
    let posarg2 = List.mapi pos (Pva.args_of pva2) in
    List.fold_left (fun sum1 (i1, a1) -> 
        List.fold_left (fun sum2 (i2, a2) -> 
            if a1 = a2 then sum2 +. rank i1 i2
            else sum2
          ) 0. posarg2) 0. posarg1
  in
  let rec allcheck i = function
    | pva::rest ->
      List.mapi
        (fun j pva'-> ((i, i + 1 + j), algo_rank pva pva'))
        rest
      @ allcheck (i + 1) rest
    | [] -> []
  in
  let allcheck = allcheck 0 in
  if List.for_all (HornClause.bpvas_of >> allcheck
                   >> List.map snd >> lmax
                   >> (=) neg_infinity) gccs then None
  else 
    let k, l, m =
      List.mapi
        (fun k gc ->
           List.map
             (fun ((l, m), r) -> (k, l, m), r)
             (allcheck (HornClause.bpvas_of gc)))
        gccs
      |> List.concat
      |> argmax
    in
    (*    assert (l <> m);*)
    let gc = List.nth gccs k in
    let pvas = HornClause.bpvas_of gc in
    let phi = HornClause.bphi_of gc in
    let p = List.nth pvas l in
    let q = List.nth pvas m in
    let pargs = Pva.args_of p in
    let qargs = Pva.args_of q in
    let rargs = union_arg pargs qargs in
    let r = Pva.make (Idnt.new_pvar ()) rargs in
    let pvas' =
      pvas 
      |> fun xs -> remove_nth xs l
                   |> fun xs -> remove_nth xs (m - 1)
                                |> fun xs -> r::xs in
    let gc' = HornClause.mk_goal pvas' phi in
    let gccs' =
      gccs
      |> fun xs -> remove_nth xs k
                   |> fun xs -> gc'::xs in
    Some (p, q, r, gccs', rw_tag p q r tag)


(*val subst_elim_duplicate : TermSubst.t -> TermSubst.t * Formula.t list*)
let subst_elim_duplicate =
  List.classify (comp2 (=) fst fst)
  >> List.map
    (fun (((_, t1) as xt) :: tsub') ->
       xt, List.map (snd >> Formula.eq Type.mk_unknown t1) tsub')
  >> List.split
  >> Pair.map_snd List.flatten

(* app hypo(lemma) function *)
let app_hypo lem psi =
  match lem with
  | Hypo psih ->
    let quas, r, body = psi in
    let pvas = HornClause.pvas_of_body body in
    let phi =  HornClause.phi_of_body body in
    let quash, rh, bodyh = psih in
    let pvash = HornClause.pvas_of_body bodyh in
    let pvsh, pvshfmla = pvash |> List.map (Pva.pvar_of) |> List.split in
    let phih =  HornClause.phi_of_body bodyh in
    let rm_dup l =
      let half = List.length l / 2 in
      let l1 = List.take half l |> List.map snd in
      let l2 = List.drop half l |> List.map snd in
      if List.length l mod 2 = 0 
         && List.combine l1 l2 |> List.for_all (fun (x, y) -> x = y) then
        None
      else Some l
    in
    (* List.iter (Format.printf "@,pvas:  %a@," Pva.pr) pvas; *)
    if idnt_subset pvash pvas then (
      List.map
        (fun pva ->
           List.map (fun pvh ->
               try Some(Pva.pat_match pvh pva) with
                 _ -> None) pvsh)
        pvas
      |> transpose
      |> List.map remove_None
      |> combinate (@)
      (* todo: 同じ述語変数で2回以上代入を作らない *)
      (* 応急処置 *)
      |> List.map rm_dup
      |> remove_None
      |> (fun ts ->
          List.iter (Format.printf "@,tsub:  %a@," TermSubst.pr) ts;
          ts)
      |> List.map subst_elim_duplicate  (*: sigma candidate list *)
      |> (fun sigmas ->
          try
            List.find (*: suffice (phi => sigma phih)*)
              (fun (subst, fs) ->
                 let fm = remove_assoc_all quash subst
                          |> formula_of_tsub in
                 let subst = remove_notassoc_all quash subst in
                 let sigphih =
                   Formula.subst subst phih
                   |> Formula.mk_and fm
                   |> Formula.mk_and (Formula.band fs)
                 in
                 
                 Format.printf "@,phi:  %a@," Formula.pr phi;
                 Format.printf "@,sigma:  %a@," TermSubst.pr subst;
                 Format.printf "@,sigphih:  %a@," Formula.pr sigphih;
                 
                 SMTProver.implies_dyn [Formula.band (phi::pvshfmla)] [sigphih])
              sigmas
          with Not_found -> (*Format.printf "@,Not_found@,";*) [], [])
      |> 
      begin
        function
        | [], _ -> quas, r, body
        | sigma, _ -> 
          let body' = Pva.subst sigma rh
                      |> fun pva -> pva :: (List.drop (List.length pvash) pvas)
                                    |> flip HornClause.mk_body phi
          in
          (* Format.printf "@,app_hypo:  %a@," HornClause.pr_body body'; *)
          quas, r, body'
      end)
    else quas, r, body
  | _ -> failwith "app_hypo"

let rec app_hypos hypos psi =
  match hypos with
  | [] -> psi
  | h::hs ->
    let psi' = app_hypo h psi in
    if psi = psi' then app_hypos hs psi
    else psi'

let mk_ivars (quas, r, body) tag =
  let pvas = HornClause.pvas_of_body body in
  let rpv, rf = Pva.pvar_of r in
  let bphi = HornClause.phi_of_body body in
  let r_id = Pva.idnt_of r in
  let r_args = List.split (PredVar.args_of rpv) |> fst in
  let cmp (tg1, _) (tg2, _) =
    match tg1, tg2 with
    | Tag.Im, Tag.Im -> 0
    | Tag.Im, Tag.Ip -> -1
    | Tag.Im, Tag.O -> -1
    | Tag.Ip, Tag.Im -> 1
    | Tag.Ip, Tag.Ip -> 0
    | Tag.Ip, Tag.O -> -1
    | Tag.O, Tag.Im -> 1
    | Tag.O, Tag.Ip -> 1
    | Tag.O, Tag.O -> 0
  in
  List.map
    (fun qua -> tag r_id (search r_args qua), qua)
    quas
  |> List.sort ~cmp
  |> List.filter (fst >> (=) Tag.Im)
  |> List.map snd

(*let ivars = ref []*)

let pat_match_hccs hccs pvas =
  let rec loop = function
    | [] -> []
    | hc::rest ->
      let Some headpv = HornClause.hpv_of hc in
      let sbsts = List.map (fun pva ->
          try Some (Pva.pat_match headpv pva) with
          | _ -> None) pvas in
      sbsts :: loop rest
  in
  loop hccs

let rec appsubss c = function
  | ([], []) -> []
  | (h::rest1, subs::rest2) ->
    let b =
      (List.mapi
         (fun i -> function
            | None -> HornClause.body_of_formulas [Formula.mk_false]
            | Some sub ->
              (* Format.printf "@,sub%d%d: %a@," c i TermSubst.pr sub; *)
              let h = HornClause.subst_varsB sub h in
              (* Format.printf "@,%a@," HornClause.pr h; *)
              h
              |> HornClause.body_of)
         subs) in
    b :: appsubss (c + 1) (rest1, rest2)

let assump gamma psi pickpvas =
  let (quas, r, body) = psi in
  let quass, hccs = List.split gamma in
  let pvas = HornClause.pvas_of_body body in
  let phi = HornClause.phi_of_body body in
  let ks = diff (Pva.fvs r) quas
           |> List.map Term.mk_var in
  (*  print_psi psi; *)
  pickpvas
  |> pat_match_hccs hccs (* hccs[sigmadP1[gP, gQ]; dP2; dQ1, dQ2] *)
  |> fun subss ->
  (* Format.printf "@,len:  %d@," (List.length subss);*)
  appsubss 0 (hccs, subss)
  |> transpose
  |> combinate (fun x y -> HornClause.and_body [x; y])
  |> fun bs ->
  List.map
    (HornClause.body_map
       (fun s -> diff pvas pickpvas |> (@) s)
       (Formula.mk_and (HornClause.phi_of_body body)))
    bs
  |> forming_bodies
  |> fun bodies ->
  assert (List.length bodies <= 1);
  let [b] = bodies in
  (*    (List.iter (Format.printf "@,after:  %a@," HornClause.pr_body) bodies; bodies)*)
  quas, r, b

let rec mk_base_hypo gamma hypos ivar =
  match hypos with
  | [] -> []
  | h::hs ->
    begin
      match h with
      | Hypo psi ->
        let (quas, r, body) = psi in
        let sbst0 = [ivar, IntTerm.zero] in
        let psi0 = List.remove quas ivar,
                   Pva.subst sbst0 r,
                   subst_body sbst0 body
        in
        let (_, _, body0) = psi0 in
        let pvas = HornClause.pvas_of_body body in
        let pvas0 = HornClause.pvas_of_body body0 in
        let pickpvas = diff pvas0 pvas in (* 現状、I-が0でないものも取られる *)
        let h0 = Hypo (assump gamma psi0 pickpvas) in
        Format.printf "@,-base hypothisis-";
        print_lem h0;
        h0 :: h :: mk_base_hypo gamma hs ivar
      | _ -> h :: mk_base_hypo gamma hs ivar
    end

let rec algo_ind gamma hypos psi tag ivars =
  (* --- INDUC --- *)
  let rec rule_induc gamma hypos psi =
    let (quas, r, body) = psi in
    let pvas = HornClause.pvas_of_body body in
    let rpv, rf = Pva.pvar_of r in
    let bphi = HornClause.phi_of_body body in
    let r_id = Pva.idnt_of r in
    let r_args = List.split (PredVar.args_of rpv) |> fst in
    let ivar = List.hd ivars in
    (*    let ivar = List.hd !ivars in *)
    (*
      let ivar =
      List.map
      (fun qua -> tag r_id (search r_args qua), qua)
      (List.rev quas) quas
      |> try List.assoc Tag.Im with Not_found -> List.assoc Tag.Ip
      in
    *)
    let quas' = List.remove quas ivar in
    let sbst0 = [ivar, IntTerm.zero] in
    let psi0 = quas',
               Pva.subst sbst0 r,
               subst_body sbst0 body
    in
    let k = Term.mk_var (Idnt.new_var ()) in
    let sbsti = [ivar, IntTerm.add k (IntTerm.one)] in
    let fi = Formula.mk_and (IntFormula.geq k IntTerm.zero) in
    let psii = quas',
               Pva.subst sbsti r,
               subst_body sbsti (map_phi_body fi body)
    in
    let sbsth = [ivar, k] in
    let psih = Hypo (quas',
                     Pva.subst sbsth r,
                     subst_body sbsth (map_phi_body fi body))
    in
    let hypo = psih in
    Format.printf "@,<rule_i>: @,";
    print_psi psi;
    Format.printf "@,ivars: ";
    List.iter (Format.printf "%a " Idnt.pr) ivars;
    Format.printf "@,ivar:  %a@," Idnt.pr ivar;
    Format.printf "@,base case:";
    print_psi psi0;
    Format.printf "@,step case:";
    print_psi psii;
    print_lem hypo;
    if hypos <> [] then
      begin
        Format.printf "@,-all hypothisis-";
        List.iter print_lem hypos
      end;
    let hypos0 = mk_base_hypo gamma hypos ivar in
    (*    ivars := List.tl !ivars; *)
    let ivars' = match ivars with | [] -> [] | _ -> List.tl ivars in
    try
      let base = algo_ind gamma hypos0 psi0 tag ivars' in
      let step = algo_ind gamma (hypo::hypos) psii tag ivars' in
      base @ step
    with  Assert_failure _ -> Format.printf "@, -------back-----------@,";
      (* ivars := List.tl !ivars; *)
      (* ivars := (List.tl !ivars) @ [List.hd !ivars]; *)
      (*    if !ivars = [] then [(quas, r, body)]
            else (Format.printf "@,ivars: %a@," Idnt.pr_list !ivars;
              rule_induc gamma hypos ((*List.rev*) quas, r, body))
      *)    
      if ivars = [] then [(quas, r, body)]
      else (Format.printf "@,ivars: %a@," Idnt.pr_list ivars;
            rule_induc gamma hypos ((*List.rev*) quas, r, body))
  in
  (* --- induc end ---*)
  (* --- ASSUMP --- *)
  let rule_assump gamma hypos psi =
    let (quas, r, body) = psi in
    let quass, hccs = List.split gamma in
    let pvas = HornClause.pvas_of_body body in
    let phi = HornClause.phi_of_body body in
    let ks = diff (Pva.fvs r) quas
             |> List.map Term.mk_var in
    let pick pvas =
      let f pva =
        let f1 id =
          (* 再帰関数でないhccs *)
          let nonrec_hccs =
            List.map snd gamma
            |> onepat_of
            |> List.filter (is_rec >> not)
          in
          (*      Format.printf "@,Non rec HCCS: %a@," HCCS.pr nonrec_hccs;
                  Format.printf "@,id: %a@," Idnt.pr id;
                  List.iter (Format.printf "@,def_idh: %a@," Idnt.pr) (HCCS.pvsH nonrec_hccs);*)
          (* 再帰していない *)
          List.mem id (HCCS.pvsH nonrec_hccs)
        in
        (||)
          (f1 (Pva.idnt_of pva))
          begin
            let p = Pva.idnt_of pva in
            let arg = (Pva.args_of >> List.map fst) pva in
            let argfvs = List.fold_left (fun l t -> Term.fvs t @ l) [] arg in
            let ksfvs = List.fold_left (fun l t -> Term.fvs t @ l) [] ks in
            let im_terms =
              List.mapi 
                (fun i a -> if tag p i = Tag.Im then Some a else None)
                arg
              |> remove_None
            in
            List.iter (Format.printf "@,im_term: %a@," Term.pr) im_terms;
            (* todo: im_term が変数のときは展開したくない*)
            let im_terms_fvs = List.fold_left (fun l t -> Term.fvs t @ l) [] im_terms in
            let im_terms_geq =
              im_terms
              |> List.map (NumFormula.leq Type.mk_int IntTerm.zero)
            in
            List.iter (Format.printf "@,im_term_geq: %a@," Formula.pr) im_terms_geq;
            let phifvs =
              Formula.conjuncts_of phi
              |> fun fs -> List.iter (Format.printf "@,phi: %a@," Formula.pr) fs;
              fs
              (* What is this ?
                     |> List.filter
                 (fun f -> List.for_all (Formula.equiv f) im_terms_geq)
                 |> fun fs -> List.iter (Format.printf "@,phi': %a@," Formula.pr) fs; fs
              *)
              |> Formula.band
              |> Formula.fvs
            in
            (*    let phifvs = Formula.fvs phi in *)
            (||)
              (List.exists (Term.is_var >> not) im_terms)
              (List.exists (flip List.mem phifvs) im_terms_fvs)
          end
      in
      List.filter f pvas
    in
    Format.printf "@,<rule_a>:  @,";
    print_psi psi;
    (pvas, phi)
    |> fun (pvas, phi) -> 
    let pickpvas = pick pvas in
    List.iter 
      (Format.printf "@,pick:  %a@," Pva.pr)
      pickpvas;
    pickpvas
    |> pat_match_hccs hccs (* hccs[sigmadP1[gP, gQ]; dP2; dQ1, dQ2] *)
    |> fun subss -> Format.printf "@,len:  %d@," (List.length subss);
    appsubss 0 (hccs, subss)
    |> transpose
    |> combinate (fun x y -> HornClause.and_body [x; y])
    |> fun bs ->
    List.map
      (HornClause.body_map
         (fun s -> diff pvas pickpvas |> (@) s)
         (Formula.mk_and (HornClause.phi_of_body body)))
      bs
    |> forming_bodies
    |> fun bodies ->
    assert (List.length bodies <= 1);
    (List.iter (Format.printf "@,after:  %a@," HornClause.pr_body) bodies; bodies)
    |> List.map (fun b -> quas, r, b)
    |> List.map (fun psi -> algo_ind gamma hypos psi tag (mk_ivars psi tag))
    |> List.concat
  in
  (* --- rule end --- *)
  (* --- algo_ind --- *)
  let rec rdct b =
    let b' = onepat_reduct (List.map snd gamma) b in
    if b = b' then b
    else rdct b'
  in
  Format.printf "@,[algo_ind]: ";
  print_psi psi;
  let (quas, r, body) = psi in
  let body' = rdct body in
  let psi = (quas, r, body') in
  if body <> body' then
    begin
      Format.printf "@,->Reduct: ";
      print_psi psi
    end;
  let psi' =
    begin
      match ivars (*!ivars*) with
      | [] ->
        app_hypos hypos psi
      | _ -> psi
    end
  in
  if psi <> psi' then
    begin
      Format.printf "@,->app_hypo: ";
      print_psi psi'
    end;
  let (quas, r, body) = psi' in
  (* let body = homogenize2_body body in *)
  let body' = rdct body in
  let psi = (quas, r, body') in
  if body <> body' then
    begin
      Format.printf "@,->Reduct: ";
      print_psi psi
    end;
  let pvas = HornClause.pvas_of_body body in
  let pvafvss = List.map Pva.fvs pvas in
  let phifvs = HornClause.phi_of_body body |> Formula.fvs in
  (* todo *)
  if List.for_all
      (fun pva -> Format.printf "@, body pv: %a " Idnt.pr (Pva.idnt_of pva);
        Pva.idnt_of pva
        |> (=) (Pva.idnt_of r)) pvas then (Format.printf "@, finish, @,"; [psi])
  else
    begin
      Format.printf "@,cond, @,";
      let ids = List.map Pva.idnt_of pvas in
      (*  let args = List.map Pva.fvs pvas in *)
      let args = List.map (Pva.args_of >> List.map fst) pvas in
      let tagss = List.map2 
          (fun id arg ->
             List.mapi
               (fun i a -> a, tag id i)
               arg
          ) ids args
      in
      (*  let fvars = HornClause.phi_of_body body |> Formula.fvs in *)
      let ks = diff (Pva.fvs r) quas
               |> List.map Term.mk_var in
      let rec checktag = function
        | [] -> false (* false: induc, true: assump *)
        | tags::rest when ks = [] ->
          List.exists
            (fun (a, t) -> t = Tag.Im
                           && (IntTerm.is_zero a
                               || (List.mem a (List.map Term.mk_var phifvs))))
            tags
          || checktag rest
        | tags::rest when ks <> [] ->
          Format.printf "@,ivars: ";
          List.iter (Format.printf "%a " Idnt.pr) ivars;
          (List.exists
             (fun (a, t) -> t = Tag.Im && not(List.mem a ks) && ivars = [])
             tags)
          || checktag rest
          (*|| (List.mem a (phifvs |> List.map Term.mk_var)
            || twoexists (List.mem a) (pvafvss |> List.map (List.map Term.mk_var)))*)
      in
      List.iter (Format.printf "@,ks:  %a@," Term.pr) ks;
      if checktag tagss then (Format.printf "@,<<ASSUMP>>@,"; rule_assump gamma hypos psi)
      else (Format.printf "@,<<INDUC>>@,"; (*ivars := mk_ivars psi tag;*) rule_induc gamma hypos psi)
    end
(* return: psi list *)

let algo_def p q r hccs tag =
  let psi =
    let body = HornClause.mk_body [p; q] Formula.mk_true in
    (Pva.fvs r, r, body)
  in
  let gamma =
    HCCS.defs_of hccs
    |> List.map (fun h -> HornClause.fvs h, h)
  in
  (* 補題用
     let parse str = 
     HCCSParser.parser_main HCCSLexer.token 
      (Lexing.from_string str)
     in
  *)
  let to_hc psi =
    let (quas, r, body) = psi in
    let (pv, phi') = Pva.pvar_of r in
    let body' = map_phi_body (Formula.mk_and phi') body in
    HornClause.mk_head (Some pv)
    |> flip HornClause.make body'
  in
  algo_ind gamma [] psi tag (mk_ivars psi tag)
  |> List.map to_hc

let rec algo_solve hccs (tag: Idnt.t -> int -> Tag.t) =
  match algo_goal (HCCS.goals_of hccs |> List.map (flip hc_rename tag)) tag with
  | None -> hccs
  | Some(p, q, r, gccs, tag') ->
    let hdef =
      algo_def p q r (HCCS.defs_of hccs) tag'
      |> fun x -> Format.printf "@,after(algo_def):  %a@," HCCS.pr x;
      x
      |> forming_full
      |> fun x -> Format.printf "@,->Simplify:  %a@," HCCS.pr x; x
    in algo_solve (HCCS.defs_of hccs @ hdef @ gccs) tag'
