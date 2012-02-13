open Utilities;;
open Grammar;;
open Automaton;;

type aty = ITbase of state | ITfun of ty * aty 
         | ITcase of int * int  (* (arity, position of non-top argument) *)
         | ITpair1 of aty | ITpair2 of aty
and ty = aty list
type name = string
type te = (nameNT * ty) list  (*** type environment for non-terminals ***)
type vte = (nameNT * ty) list  (*** type environment for variables ***)
type cte = (nameT * ty) list (*** type environment for constants ***)

let init_te nt =
  Utilities.list2hash (List.map (fun (x,_) -> (x, [])) nt)

let fst_of_pairty ty =
  List.fold_left (merge_and_unify compare) []
   (List.map
    (function aty ->
        match aty with
           ITpair1(aty1) -> [aty1]
         | _ -> []) ty)

let snd_of_pairty ty =
  List.fold_left (merge_and_unify compare) []
   (List.map
    (function aty ->
        match aty with
           ITpair2(aty2) -> [aty2]
         | _ -> []) ty)

let rec print_ty ty =
  match ty with
    [] -> print_string "Top"
  | [aty] -> 
         (match aty with
           ITbase(q) -> print_string q
         | _ -> (print_string "("; print_aty aty; print_string ")")
         )
  | aty::ty' -> 
      match aty with
        (ITpair1(_) | ITpair2(_)) ->
           let ty1 = fst_of_pairty ty in
           let ty2 = snd_of_pairty ty in
              (print_string "("; print_ty ty1;
               print_string " * "; print_ty ty2;
               print_string ")")
       | _ -> (print_ty [aty]; print_string "/\\"; print_ty ty' )
and print_aty_parens aty =
  match aty with
     ITbase(q) -> print_aty aty
   | _ -> (print_string "("; print_aty aty; print_string ")")
and print_aty aty =
  match aty with
    ITbase(q) -> print_string q
  | ITfun(ty, aty) ->
       (print_ty ty; print_string " -> "; print_aty aty)
  | ITcase(m,n) ->
       (print_string ("case("^(string_of_int m)^", "^(string_of_int n)^")"))
  | ITpair1(aty1) ->
       (print_aty_parens aty1; print_string " * Top "; )
  | ITpair2(aty1) ->
       (print_string "Top * "; print_aty_parens aty1)

let print_tbinding (f, ty) =
  (print_string (f^" : \n  ");
   List.iter (fun aty -> (print_aty aty; print_string "\n  ")) ty;
   print_string "\n")
  
let print_te te =
  List.iter print_tbinding (List.sort (fun (f1,_) (f2,_) -> compare f1 f2) te)

let lookup_te f te =
  try Hashtbl.find te f with Not_found -> raise (Grammar.UndefinedNonterminal f)
let update_te f ty te =
  (Hashtbl.replace te f ty; te)



let singletontype_of_value n = ITbase(string_of_int n)
let value_of_singletontype aty = 
  match aty with
     ITbase(q) -> int_of_string q
   | _ -> assert false

let compute_type_head h te nte vte cte =
  match h with
(**    NT(f) -> merge_and_unify compare (lookup_te f te) (lookup_te f nte) **)
    NT(f) -> List.rev_append (lookup_te f nte) (lookup_te f te) 
              (* Shouldn't this return a sorted list? *)
  | T(a) -> List.assoc a cte
  | FD(n) -> [(singletontype_of_value n)]
  | Var(v) -> List.assoc v vte
  | CASE(_) -> raise (Fatal "compute_type_head: case")
  | PAIR -> raise (Fatal "compute_type_head: pair")
  | DPAIR -> raise (Fatal "compute_type_head: dpair")


let rec subtype aty1 aty2 =
  match (aty1,aty2) with
    (ITbase(q1), ITbase(q2)) -> q1=q2
  | (ITfun(ty1,aty11), ITfun(ty2, aty21)) ->
      (subtype aty11 aty21) 
      && (List.for_all (fun aty12 -> List.exists (fun aty22 -> subtype aty22 aty12) ty2) ty1)
  | (ITpair1(aty11), ITpair1(aty21)) ->
      (subtype aty11 aty21)
  | (ITpair2(aty11), ITpair2(aty21)) ->
      (subtype aty11 aty21)
  | _ -> false

let add_te te telist =
  let _ =
    List.iter
      (fun (f, atys) ->
        let atys1 = lookup_te f te in
        let atys1' = List.filter (fun aty1 -> not(List.exists
                           (fun aty -> subtype aty aty1) atys)) atys1 in
        let atys1'' = merge_and_unify compare atys atys1' in  
        let _ = update_te f atys1'' te in
         ())
    telist
  in
    te

let filter_valid_types telist nte =
  List.map 
   (fun (f, atys) -> 
     let atys1 = lookup_te f nte in
      (f, List.filter (fun aty -> not(List.exists 
                           (fun aty1 -> subtype aty1 aty) atys1)) atys)) 
(**
     (f, List.filter (fun aty -> not(List.mem aty atys1)) atys))  
**)
    telist

let rec range_type fty argtys =
  match fty with
    ITbase(q) -> Some(fty)
  | ITfun(ty, rty) ->
      (match argtys with
        [] -> Some(fty)
      | ty1::argtys' ->
         if (** subset_sortedlist compare ty ty1 **)
           List.for_all (fun aty -> List.exists (fun aty1 -> subtype aty1 aty) ty1) ty
         then
           range_type rty argtys'
         else
           None
       )
  | ITcase(m,n) -> raise (Fatal "range_type: case type should not occur")
  | ITpair1 _ -> if argtys=[] then Some(fty) else None
  | ITpair2 _ -> if argtys=[] then Some(fty) else None
       

let rec range_types ftys argtys rtys =
  match ftys with
    [] -> rtys
  | fty::ftys' ->
      match range_type fty argtys with
        Some(rty) -> 
        range_types ftys' argtys (merge_and_unify compare [rty] rtys)
      | None ->
        range_types ftys' argtys rtys

let mk_pairty ty1 ty2 =
  merge_and_unify compare
    (List.map (fun aty -> ITpair1 aty) ty1)
    (List.map (fun aty -> ITpair2 aty) ty2)


let rec compute_type term te nte vte cte: ty =
  match term with
    App(CASE(m), t::terms) ->
      (match compute_type t te nte vte cte with
        [] -> []
       | aty::_ ->
          let n = value_of_singletontype aty in
          let t1 = List.nth terms n in
            compute_type t1 te nte vte cte
      )
  | App(PAIR, [t1;t2]) ->
      let ty1 = compute_type t1 te nte vte cte in
      let ty2 = compute_type t2 te nte vte cte in
        mk_pairty ty1 ty2
  | App(PAIR, _) ->
        raise (Fatal "compute_type: pair constructor has a wrong number of arguments")
  | App(DPAIR, [t1;t2]) ->
      let ty1 = compute_type t1 te nte vte cte in
      let ty2 = compute_type t2 te nte vte cte in
      let ty11 = fst_of_pairty ty1 in
      let ty12 = snd_of_pairty ty1 in
        range_types ty2 [ty11;ty12] []
  | App(h, terms) ->
      let ty_h = compute_type_head h te nte vte cte in
      let tl = compute_type_tl terms te nte vte cte in
      let ty = range_types ty_h tl [] in
        ty
and compute_type_tl terms te nte vte cte: ty list =
  List.map (fun term-> compute_type term te nte vte cte) terms

let rec codom rty ty =
  match ty with
    ITbase(q') -> ty=rty
  | ITfun(ty1,aty) -> codom rty aty
  | ITcase(m,n) -> raise (Fatal "codom: ITcase should not occur")
  | ITpair1 _ -> raise (Fatal "codom: ITpair1 should not occur")
  | ITpair2 _ -> raise (Fatal "codom: ITpair2 should not occur")

let rec codom2 rty terms aty =
  match terms with
    [] -> subtype aty rty
  | _::terms' ->
     match aty with
       ITfun(ty1,aty) -> codom2 rty terms' aty
     | ITbase _ -> false
     | _ -> assert false

let rec compute_apptype_tl funtys terms te nte vte cte =
  match terms with
    [] -> funtys
  | t::terms' ->
      let tl = compute_type t te nte vte cte in
      let funtys' = List.map (fun fty -> range_type fty [tl]) funtys in
      let funtys'' = Utilities.take_some funtys' in
         compute_apptype_tl funtys'' terms' te nte vte cte

let rec mk_vte vars at =
    match at with
      ITbase(q) -> 
         ([], ITbase(q))
    | ITfun(ty, aty1) ->
       (  match vars with
            [] -> raise (Fatal "check_aty")
        | v::vars' -> 
             let (ve1, rt1) = mk_vte vars' aty1 in
               ((v, ty)::ve1, rt1)
       )
    | ITcase _ -> raise (Fatal "mk_vte: ITcase should not occur")
    | ITpair1 _ -> raise (Fatal "mk_vte: ITpair1 should not occur")
    | ITpair2 _ -> raise (Fatal "mk_vte: ITpair2 should not occur")


let rec argtys aty =
  match aty with
    ITbase(_) -> []
  | ITfun(ty1,aty1) -> ty1::(argtys aty1)
  | ITcase(m,n) -> raise (Fatal "argtys")
  | ITpair1 _ -> raise (Fatal "argtys: ITpair1")
  | ITpair2 _ -> raise (Fatal "argtys: ITpair2")

let rec has_ty_term term ty te nte vte cte env g =
  List.for_all 
  (fun aty -> has_aty_term term aty te nte vte cte [] g)
  ty
and has_aty_term term aty te nte vte cte env g =
  match term with
    App(CASE(m), t::terms) ->
      let ty_of_t = compute_type t te nte vte cte in
       (match ty_of_t with
         [] -> List.for_all
               (fun t' -> has_aty_term t' aty te nte vte cte env g)
               terms
        | aty1::_ ->
            let n = value_of_singletontype aty1 in
            let t1 = List.nth terms n in
            has_aty_term t1 aty te nte vte cte env g
        )
    | App(PAIR,_) -> assert false (* pairs are not supported *)
    | App(DPAIR,_) -> assert false 
    | App(NT(f) as h,terms) ->
        if Grammar.is_recfun f then
           let ty1 = compute_type_head h te nte vte cte in
           let ty2 = List.filter (fun aty1 -> codom2 aty terms aty1) ty1 in
             List.exists (fun aty2 ->
                 has_aty_args terms aty2 te nte vte cte env g) ty2 
        else
          let (vars,body) = List.assoc f g.r in
          let n = List.length terms in
          let (vars1,vars2) = list_take_n_and_rest vars n in
          let (vte1,aty1) = mk_vte vars2 aty in
          let st = List.combine vars1 terms in
          let body' = Grammar.app_subst st body in
             has_aty_term body' aty1 te nte (vte1@vte) cte env g
     | App(h,terms) ->
           let ty1 = compute_type_head h te nte vte cte in
           let ty2 = List.filter (fun aty1 -> codom2 aty terms aty1) ty1 in
             List.exists (fun aty2 ->
                 has_aty_args terms aty2 te nte vte cte env g) ty2
and has_aty_args terms aty te nte vte cte env g =
  match terms with
    [] -> true
  | t::terms' ->
     ( match aty with
        ITfun(ty1,aty2) ->
           (has_ty_term t ty1 te nte vte cte env g)
           && (has_aty_args terms' aty2 te nte vte cte env g)
      | _ -> false
      )
    
let rec has_type term rty te nte vte cte g =
  has_aty_term term rty te nte vte cte [] g
(***
  match term with
    App(CASE(m), t::terms) ->  
      (match compute_type t te nte vte cte with
        [] -> false
      | aty::_ ->
         let n = value_of_singletontype aty in
         let t1 = List.nth terms n in
            has_type t1 rty te nte vte cte
      )
  | App(PAIR, [t1;t2]) ->
     (match rty with
       ITpair1(aty1) ->
          (has_type t1 aty1 te nte vte cte)
      | ITpair2(aty1) ->
          (has_type t2 aty1 te nte vte cte)
      | _ -> false
     )
  | App(DPAIR, _) ->
     let ty = compute_type term te nte vte cte in
       List.mem rty ty
  | App(h, terms) ->
      let ty_h = List.filter (fun ty -> codom rty ty) 
                (compute_type_head h te nte vte cte) in
      match h with
        T(a) ->
          (match ty_h with
              [] -> false
            | ty::_ -> 
                 let tys = argtys ty in
                 let ty_term_pairs = List.combine tys terms in
                   List.for_all
                     (fun (ty,t) ->
                        List.for_all (fun aty ->
                         has_type t aty te nte vte cte) ty) 
                   ty_term_pairs
           )
       | _ ->
         let atys = compute_apptype_tl ty_h terms te nte vte cte in
            atys!=[]
***)

(*** checks whether the body of f has type at ***)
let rec check_aty (f: nameNT) (at: aty) te nte cte g =
  let (vars, body) = get_def f g in
  let (vte, rang_ty) =  mk_vte vars at in
    has_type body rang_ty te nte vte cte g

let rec gfp te cte nte unchecked dmap g =
  match unchecked with
    [] -> ()
  | f::unchecked' ->
     let fty = lookup_te f te in
     let fty' = List.filter (fun aty -> check_aty f aty te nte cte g) fty in
       if List.length fty=List.length fty' then
         gfp te cte nte unchecked' dmap g
       else
         let to_be_checked = List.assoc f dmap in
         let te' = update_te f fty' te in
           gfp te' cte nte (merge_and_unify compare unchecked' to_be_checked) dmap g

let rec gfp_check te cte nte unchecked dmap g =
  match unchecked with
    [] -> ()
  | f::unchecked' ->
     let _ = print_string ("checking "^f^"\n") in
     let fty = lookup_te f te in
     let fty' = List.filter (fun aty -> check_aty f aty te nte cte g) fty in
       if List.length fty=List.length fty' then 
          gfp_check te cte nte unchecked' dmap g
       else assert false

let compute_te te cte nte dmap g =
  let unchecked = List.map fst (g.nt) in
    (gfp te cte nte unchecked dmap g; 
(*     gfp_check te cte nte unchecked dmap g; *)
     te)

let rec get_related nts dmap acc =
  match nts with
     [] -> acc
   | nt::nts' ->
        let nts1 = List.assoc nt dmap in
        let acc' = merge_and_unify compare nts1 acc in
           get_related nts' dmap acc'

let merge_ty = merge_and_unify compare
let merge_tylist tylist =
  List.fold_left merge_ty [] tylist

let add_aty_ty aty ty =
  if List.exists (fun aty1 -> subtype aty1 aty) ty
  then ty
  else 
   let ty' = List.filter (fun aty1 -> not(subtype aty aty1)) ty in
     merge_ty [aty] ty'

let diff_ty ty1 ty2 =
  List.filter (fun aty -> not(List.mem aty ty2)) ty1

let rec diff_telist telist1 telist2 =
  match telist1 with
    [] -> []
  | (f,ty1)::telist1' ->
       let ty2 = List.assoc f telist2 in
       let ty = diff_ty ty1 ty2 in
         (f,ty)::(diff_telist telist1' telist2)
let rec merge_telist telist1 telist2 =
  match (telist1,telist2) with
    ([], _) -> telist2
  | (_, []) -> telist1
  | ((f1,ty1)::telist1', (f2,ty2)::telist2') ->
      if f1=f2 then
         (f1, merge_ty ty1 ty2)::(merge_telist telist1' telist2')
      else if f1<f2 then
        (f1,ty1)::(merge_telist telist1' telist2)
      else 
        (f2,ty2)::(merge_telist telist1 telist2')

let rec mk_funcaty tylist aty =
  match tylist with
    [] -> aty
  | ty::tylist' -> ITfun(ty, mk_funcaty tylist' aty)

let rec filter_out_supty ty =
  filter_out_supty_aux ty []
     
and filter_out_supty_aux ty ty1 =
  match ty with
    [] -> List.rev_append ty1 []
  | aty::ty' ->
      if List.exists (fun aty1 -> subtype aty1 aty) ty'
        || List.exists (fun aty1 -> subtype aty1 aty) ty1
      then filter_out_supty_aux ty' ty1
      else filter_out_supty_aux ty' (aty::ty1)

let expand_te_head h terms ty vte te cte nte =
  match h with
    NT(f) -> 
     let tylist = List.map (fun arg -> compute_type arg te nte vte cte) terms in
     let tylist' = List.map filter_out_supty tylist in
     let ty1 = List.map (fun aty -> mk_funcaty tylist' aty) ty in
        [(f, ty1)]
  | _ -> []

let rec rty_nth aty n =
  if n=0 then aty
  else 
     match aty with
        ITfun(_,aty') -> rty_nth aty' (n-1) 
      | _ -> assert false

let rec compute_type_head_ext h terms ty te nte vte cte =
  match h with
(**    NT(f) -> merge_and_unify compare (lookup_te f te) (lookup_te f nte) **)
    (NT(_)| T(_)| FD(_)| Var _) -> 
       let hty = compute_type_head h te nte vte cte in
       let arity = List.length terms in
       let hty' = List.filter
                   (fun aty -> 
                     let rty = rty_nth aty arity in
                      List.exists (subtype rty) ty)
                   hty
       in
       let tyseq = List.map (fun aty ->
                             let tys = argtys aty in
                             list_take_n tys arity) hty' 
       in
          mk_term_ty_list terms tyseq
  | CASE(n) -> 
        let ty1 = compute_type (List.hd terms) te nte vte cte in
        ( match ty1 with
           aty::_ ->
              let i = value_of_singletontype aty in
              let term1 = List.nth terms (i+1) in
                [(term1, ty)]
         | _ -> [] (* give up expanding types *)
        )
  | PAIR -> []
  | DPAIR -> []
and mk_term_ty_list terms tyseq =
  match terms with
     [] -> []
   | term::terms' ->
       let tys = List.map List.hd tyseq in
       let ty = merge_tylist tys in
        (term,ty)::(mk_term_ty_list terms' (List.map List.tl tyseq))

let rec expand_te_term term ty vte te cte nte =
  let App(h, terms) = term in
  let telist1 = expand_te_head h terms ty vte te cte nte in
  let term_ty_list = 
      compute_type_head_ext h terms ty te nte vte cte in
  let telist2 = expand_te_terms term_ty_list vte te cte nte in
     merge_telist telist1 telist2

and expand_te_terms term_ty_list vte te cte nte =
  match term_ty_list with
     [] -> []
   | (term,ty)::term_ty_list' ->
        let telist1 = expand_te_term term ty vte te cte nte in
        let telist2 = expand_te_terms term_ty_list' vte te cte nte in
          merge_telist telist1 telist2

let expand_te_rule_aty f rule aty te cte nte =
   let (vars, term) = rule in
   let (vte, aty') = mk_vte vars aty in
     expand_te_term term [aty'] vte te cte nte

let expand_te_rule_ty f rule ty te cte nte =
  List.fold_left 
  (fun telist aty -> merge_telist (expand_te_rule_aty f rule aty te cte nte) telist)
   [] ty

let expand_te_rule f rule te cte nte = 
  let ty = lookup_te f te in
    expand_te_rule_ty f rule ty te cte nte

let rec add_te_ret_ns te newtelist = 
  match newtelist with
    [] -> []
  | (f,ty)::telist' ->
       let ty1 = lookup_te f te in
       let ty2 = merge_and_unify compare ty ty1 in
         if List.length ty2= List.length ty1 then
            add_te_ret_ns te telist'
         else
          let _ = update_te f ty2 te in
           merge_and_unify compare [f] (add_te_ret_ns te telist')

let rec expand_repeat te cte nte unchecked dmap g =
  match unchecked with
     [] -> ()
   | f::unchecked' ->
       let rule = List.assoc f g.r in
       let te1 = expand_te_rule f rule te cte nte in
       let te2 = filter_valid_types te1 nte in
       let nts = add_te_ret_ns te te2 in (* nts <- updated nts *)
       let to_be_checked = get_related nts dmap nts in
       let unchecked' = merge_and_unify compare unchecked' to_be_checked in
          expand_repeat te cte nte unchecked' dmap g

(* a function to create sdmap from dmap; dmap should be sorted *)
let rec sym_dmap dmap =
  List.map
   (fun (f, gs) -> 
      let gs2 = List.map fst 
                 (List.filter (fun (_,gs') -> List.mem f gs') dmap) in
         (f, merge_and_unify compare [f] 
             (merge_and_unify compare gs gs2))) dmap

(* sdmap maps f to the set of non-terminals whose rules refer to f
   or which occur in f's rule *)
let expand_te te cte nte sdmap g =
  let unchecked = List.map fst (g.nt) in
    expand_repeat te cte nte unchecked sdmap g

(** convert a transition q->q1...qn(=qs) to a type **)
let rec tr2aty q qs =
  match qs with
   [] -> ITbase(q)
 | q1::qs' ->
     let aty = tr2aty q qs' in
       ITfun([ITbase(q1)], aty)
let extend_cte a aty cte =
  try 
    let (ty, cte') = list_assoc2 a cte in
      (a, aty::ty)::cte'
  with
    Not_found -> cte

let automaton2cte m =
  let delta = m.delta in
  let cte0 = List.map (fun (a,_) -> (a, [])) m.alpha in
    List.fold_left 
     (fun cte -> fun ((q, a), qs) ->
        let aty = tr2aty q qs in
          extend_cte a aty cte) 
     cte0 delta



let fst_of_pairty ty =
  List.fold_left (merge_and_unify compare) []
   (List.map
    (function aty ->
        match aty with
           ITpair1(aty1) -> [aty1]
         | _ -> []) ty)

let snd_of_pairty ty =
  List.fold_left (merge_and_unify compare) []
   (List.map
    (function aty ->
        match aty with
           ITpair2(aty2) -> [aty2]
         | _ -> []) ty)

(*** examples for debugging ***)
let q0 = ITbase "q0"
let q1 = ITbase "q1"
let ft0 = ITfun([], q0);;
let ft1 = ITfun([q0], q0);;
let ft2 = ITfun([q0;q1], q0);;

let cte1 = [("a", [ITfun([q0], ITfun([q0], q0)); ITfun([q1], ITfun([q1], q1))]);
            ("b", [ITfun([q1], q0); ITfun([q1], q1)]);
            ("c", [q0; q1])]
let te1 = [("S",[q0]);
           ("F", [ITfun([], q0); ITfun([q0], q0); ITfun([q0;q1], q0)])]
let te2 = [("S",[q0]);
           ("F", [ITfun([], q0); ITfun([q0], q0)])]
