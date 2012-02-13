open Utilities;;
open Grammar;;

type state = int
type state_eqrel = state list list
type alpha = Tpair | Tfun | Tbase of string 
           | Tepsilon (** for intersections **)

type translist = ((state list * alpha) * state) list 

type transition = (state, (state list * alpha) list) Hashtbl.t

type subcache = (state*state, bool) Hashtbl.t
  
(* automata representation of types *)
(* TO DO: states should be classified into universal and existential ones *)
type automaton = {states: state list;  (* the set of states *)
                  univ: state list ref; (* the set of universal states *)
                  delta: transition;
                         (* (non-deterministic) transition function *)
                  gen: (state * state) list (* generalize relation *);
                  sub: subcache;
                  cache_aty: (Typing.aty * state) list;
                  cache_ty: (Typing.ty * state) list
                 }

let new_delta (): transition = Hashtbl.create 1000;;

let delta_add (delta:transition) x tr =
  Hashtbl.add delta x tr

let empty_delta: transition = 
  let delta = new_delta() in
  let _ = delta_add delta 0 [] in
      delta;;

let new_subcache (): subcache = Hashtbl.create 10000;;

let memoize_sub x y m =
  (Hashtbl.add m.sub (x,y) true; m)
(**  (m.sub := (x,y)::!(m.sub); m)**)

let memoize_subn x y m =
  (Hashtbl.add m.sub (x,y) false; m)
(**  (m.sub := (x,y)::!(m.sub); m)**)


let empty_automaton = 
  {states=[0]; univ= ref [0]; delta=empty_delta; 
   gen=[]; sub = new_subcache(); cache_aty=[]; cache_ty=[([],0)]}


let union_delta delta1 delta2: translist = delta1@delta2



let rec delta_add_trlist delta deltalist =
  match deltalist with
     [] -> ()
   | ((qs,act),q)::deltalist' ->
        let (deltalist1, deltalist2) =
          list_filter2 (fun (_,q1)->q=q1) deltalist' in
        let tr1 = List.map fst deltalist1 in
        let _ = delta_add delta q ((qs,act)::tr1) in
          delta_add_trlist delta deltalist2

let trans2list delta =
  let l = hash2list delta in
  let l2 = List.map (fun (x, tr) -> List.map (fun act -> (act,x)) tr) l in
     (List.flatten l2)

let list2trans deltalist = 
  let delta = new_delta() in
  let _ = delta_add_trlist delta deltalist in
    delta

let rec replace eqrel q =
  match eqrel with
     [] -> q
   | eqset::eqrel' ->
      if List.mem q eqset then List.hd eqset
      else replace eqrel' q

let del_states l1 l2 =
  List.filter (fun x->List.mem x l1) l2

let disjoint st1 st2 =
  not(List.exists (fun x->List.mem x st2) st1)

let unions sets =
  List.fold_left (merge_and_unify compare) [] sets;;

let union_st  =
  merge_and_unify compare;;

let rec merge_eqrel_sub states eqrel =
  let (eqrel1, eqrel2) = list_filter2 (disjoint states) eqrel in
    (unions (states::eqrel2))::eqrel1

let rec merge_eqrel eqrel1 eqrel2 =
  match eqrel1 with
    [] -> eqrel2
   | states::eqrel1' ->
       let eqrel2' = merge_eqrel_sub states eqrel2 in
        merge_eqrel eqrel1' eqrel2'

let same_dom tr1 tr2 =
  (fst tr1)=(fst tr2)

let rec find_eqclass eqrel q =
  match eqrel with
    [] -> [q]
  | qs::eqrel' ->
      if List.mem q qs then qs
      else find_eqclass eqrel' q

(* find the representative element of the equivalence class containing q *)
let rec representative eqrel q =
  match eqrel with
    [] -> q
   |states::eqrel' -> 
      if List.mem q states then List.hd(states) else representative eqrel' q

(* check whether q is a non-representative element of the equivalence relation eqrel *)
let rec nonrep eqrel q =
  match eqrel with
    [] -> false
  | qs::eqrel' ->
       (List.mem q (List.tl qs)) || (nonrep eqrel q)

(* check whether q1 and q2 belong to the same equivalence class *)
let related eqrel q1 q2 =
  q1=q2
  || List.exists 
       (fun states -> (List.mem q1 states && List.mem q2 states)) eqrel

let eq_dom eqrel1 eqrel2 (qs1,a1) (qs2,a2) =
  a1=a2 &&
  (List.length qs1 = List.length qs2) &&
  List.for_all 
    (fun (q1,q2) -> (related eqrel1 q1 q2) || (related eqrel2 q1 q2))
     (List.combine qs1 qs2)

(* get_tr x delta = {(qs,a) |  delta(qs,a)=x} *)
let get_tr x delta =
    Hashtbl.find delta x
(**  List.map fst (List.filter (fun (_,y) -> y=x) delta)
 **)

let get_trm x m =
  get_tr x m.delta

(* check whether the states x and y are equivalent, 
 * up to the equivalence relation eqrel1@eqrel2
 * (giving eqrel1 and eqrel2 separately is just for a technical convenience)
 *)
let eq_state delta eqrel1 eqrel2 x y =
  let tr1 = get_tr x delta in
  let tr2 = get_tr y delta in
     (List.for_all (fun d1 -> List.exists (fun d2 -> eq_dom eqrel1 eqrel2 d1 d2) tr2) tr1)
   && (List.for_all (fun d1 -> List.exists (fun d2 -> eq_dom eqrel1 eqrel2 d1 d2) tr1) tr2)
  
(* find equivalent states *)
let rec find_eqst delta (eqrel: state_eqrel) (checked: state_eqrel) =
   match eqrel with
     [] -> checked
   | states::eqrel' ->
       (match states with
           [] -> find_eqst delta eqrel' checked
         | [x] -> find_eqst delta eqrel' checked
         | x::states' ->
              let (states1, states2) = 
                   list_filter2 (eq_state delta eqrel checked x) states' in
                if states2=[] then 
                   find_eqst delta eqrel' (states::checked)
                else find_eqst delta (eqrel'@((x::states1)::states2::checked)) []
        )

let is_univstate automaton q =
  (List.mem q !(automaton.univ)) (** && List.length(get_tr q delta)>1 **)

let is_exstate automaton (q: state)  =
  not(is_univstate automaton q);;

let get_exstates automaton: state list =
   List.filter (is_exstate automaton) automaton.states

let rec merge_states (delta: translist) eqrel =
  let delta' = List.map (fun ((qs,a), q) -> 
                      ((List.map (replace eqrel) qs, a), replace eqrel q)) delta in
      delta'

let rec is_nonrep eqrel x =
  match eqrel with
     [] -> false
   | qs::eqrel' -> (List.mem x (List.tl qs))||(is_nonrep eqrel' x)

let minimize_automaton automaton =
   let univ_states = !(automaton.univ) in
   let ex_states = get_exstates automaton in
   let eqrel = find_eqst (automaton.delta) [ex_states; univ_states] [] in
   let delta' = 
         list2trans
          (delete_duplication_unsorted 
            (merge_states (trans2list (automaton.delta)) eqrel)) in
   let states' = List.filter (fun x-> not(is_nonrep eqrel x)) automaton.states in
   let univ' = List.filter 
              (fun x-> List.mem x states') (**  && (List.length (get_tr x delta'))>1) **)
              !(automaton.univ) in
      ({states = states'; univ=ref univ'; 
       delta=delta'; gen = automaton.gen; sub = automaton.sub;
        cache_ty = automaton.cache_ty; cache_aty = automaton.cache_aty}, eqrel)



let stateid = ref 1;;
let gen_newstate() =
   (let x = !stateid in
     (stateid := !stateid+1; x))


let new_st old q =
  not(List.mem q old)


let rename_state ren q =
  try
    List.assoc q ren
  with
    Not_found -> q

let rename_states ren qs =
  List.map (rename_state ren) qs

let merge_eqrel qs eqrel =
  let (eqrel1,eqrel2) = list_filter2 (fun qs1 -> List.mem (List.hd qs) qs1) eqrel in
    if eqrel1=[] then
        qs::eqrel
    else
       let qs' = union_st qs (List.hd eqrel1) in
          qs'::eqrel2

let non_disjoint qs1 qs2 =
  List.exists (fun x->List.mem x qs2) qs1


(*** conversion from term representation of types to automata representation ***)
(** aty2automaton :
  aty ->
  state list ->
  ((state list * alpha) * state) list ->
  (aty * state) list ->
  (ty * state) list ->
  state * state list * ((state list * alpha) * state) list *
  (aty * state) list * (ty * state) list 
 **)


let rec aty2automaton aty m =
  try
    let state = List.assoc aty m.cache_aty in
       (state, m)
  with Not_found ->
(**  let _ = if !debugging then
 **         (print_string "creating a state for:\n  ";
 **          Typing.print_aty aty;
 **          print_string "\n")
 **       else () in
 **)
  let x = gen_newstate() in
  let cache_aty' = (aty,x)::m.cache_aty in
  let states' = x::m.states in 
  let m0 = {states=states'; univ=m.univ; delta=m.delta; sub = m.sub;
            gen=m.gen; cache_aty=cache_aty'; cache_ty=m.cache_ty} in
    match aty with
       Typing.ITbase(q) -> 
         let _ = 
             delta_add m.delta x [([], Tbase(q))] in
         let m = {states=m0.states; univ=m0.univ; delta=m.delta; sub=m0.sub;
                gen=m0.gen; cache_aty=m0.cache_aty; cache_ty=m0.cache_ty} in
           (x, m)
    | Typing.ITfun(ty, aty1) ->
        let (y, m1) = ty2automaton ty m0 in
        let (z, m2) = aty2automaton aty1 m1 in
        let _ = delta_add m2.delta x [([y;z], Tfun)] in
        let m3 =  {states=m2.states; univ=m2.univ; delta=m.delta; sub=m2.sub;
                gen=m2.gen; cache_aty=m2.cache_aty; cache_ty=m2.cache_ty} in
           (x, m3)
    | _ -> raise (Fatal "aty2automaton: pair or case types")
and ty2automaton ty m =
  try
    let x = List.assoc ty m.cache_ty in
      (x, m)
  with Not_found ->
  let x = gen_newstate() in
  let cache_ty' = (ty,x)::m.cache_ty in
  let states' = x::m.states in
  let m0 = {states=states'; univ=m.univ; delta=m.delta; sub=m.sub;
            gen=m.gen; cache_aty=m.cache_aty; cache_ty=cache_ty'} in
  match ty with
     [] -> (* top *)
           raise (Fatal "ty2automaton: top type should already exist")
  | ((Typing.ITfun(_,_)::_) | (Typing.ITbase _::_))->
      let (qs, m1) = atylist2automaton ty m0 in
      let tr = List.sort compare (List.map (fun q->([q],Tepsilon)) qs) in
      let _ = delta_add m1.delta x tr in
      let m2 = {states=m1.states; univ= ref (union_st [x] !(m1.univ)); 
                delta= m1.delta; sub=m1.sub;
                gen=m1.gen; cache_aty=m1.cache_aty; cache_ty=m1.cache_ty}  in
         (x, m2)
  | _ -> (* pair types *)
      let ty1 = Typing.fst_of_pairty ty in
      let ty2 = Typing.snd_of_pairty ty in
      let (x1, m1) = 
         ty2automaton ty1 m0 in 
      let (x2, m2) = 
         ty2automaton ty2 m1 in
      let _ = delta_add m2.delta x [([x1;x2], Tpair)] in
      let m3 = {states=m2.states; univ=m2.univ;
                delta= m2.delta; sub=m2.sub;
                gen=m2.gen; cache_aty=m2.cache_aty; cache_ty=m2.cache_ty}  in
        (x, m3)
and atylist2automaton ty m =
  match ty with
    [] -> (([]: state list), m)
  | aty::ty' ->
      let (q,m1) = aty2automaton aty m in
      let (qs,m2) = atylist2automaton ty' m1 in
        (q::qs, m2)

let rec te2automaton te m =
  match te with
   [] -> ([], m)
 | (x, ty)::te' ->
     let (qs, m1) = atylist2automaton ty m in
     let (mte, m2) = te2automaton te' m1 in
        ((x,qs)::mte, m2)

let is_top y m =
  (get_tr y (m.delta) = [])

(* The following function judges x<y conservatively: it may answer false even if x < y holds. *)
let rec subtype_aux x y m tmp =
  if x=y then (true, m)
  else 
  try 
      (Hashtbl.find m.sub (x,y), m)
  with Not_found ->
  if List.mem (x,y) tmp
  then
     (true, m)
  else 
  let tmp' = (x,y)::tmp in
   if is_univstate m y then
     let tr = get_tr y (m.delta) in
     let qs = List.map 
             (function ([q],Tepsilon) -> q 
               | _ -> raise 
                 (Fatal ("subtype: univstate"^(string_of_int y)
                        ^" has a non-epsilon transition")))
              tr in
        all_subtype_t_tl x qs m tmp'
   else if is_univstate m x then
     let tr = get_tr x (m.delta) in
     let qs = List.map 
             (function ([q],Tepsilon) -> q 
               | _ -> 
                 raise (Fatal ("subtype::2: univstate"^(string_of_int x)
                        ^" has a non-epsilon transition")))
              tr 
     in
        ex_subtype_tl_t qs y m tmp' 
   else
     let tr1 = get_trm x m in
     let tr2 = get_trm y m in
       subtype_tr_tr tr1 tr2 m tmp'
and ex_subtype_tl_t qs y m tmp =
  match qs with
     [] -> if is_top y m then (true,m) else (false,m)
   | q::qs' ->
       let (b,m')=subtype_aux q y m tmp in
         if b then (true, m')
         else ex_subtype_tl_t qs' y m' tmp
and all_subtype_t_tl x qs m tmp =
  match qs with
     [] -> (true, m)
   | q::qs' ->
       let (b,m')=subtype_aux x q m tmp in
         if b then all_subtype_t_tl x qs' m' tmp
         else (false, m')
and subtype_tr_tr tr1 tr2 m tmp =
  match tr1 with
    [] -> (true, m)
  | (qs,a)::tr1' ->
       let (b,m') = subtype_act_tr (qs,a) tr2 m tmp in
         if b then subtype_tr_tr tr1' tr2 m' tmp
         else (false, m')
and subtype_act_tr (qs,a) tr m tmp =
  if a=Tfun then
    match qs with
       [qarg;qret] -> subtype_fun_tr qarg qret tr m tmp
      | _ -> raise (Fatal "subtype_act_tr")
  else
    subtype_act_tr_aux (qs,a) tr m tmp
and subtype_fun_tr qarg qret tr m tmp: bool * automaton =
  match tr with
    [] -> (false, m)
  | (qs2,a2)::tr2 ->
       if Tfun=a2 then
         match qs2 with
           [qarg2;qret2] -> 
              let (b1, m1) = subtype_aux qarg2 qarg m tmp in
              if b1 then 
                let (b2,m2) = subtype_aux qret qret2 m1 tmp in
                   if b2 then (true, m2)
                   else subtype_fun_tr qarg qret tr2 m2 tmp
              else subtype_fun_tr qarg qret tr2 m1 tmp
          | _ -> raise (Fatal "subtype_fun_tr")
       else subtype_fun_tr qarg qret tr2 m tmp
and subtype_act_tr_aux (qs,a) tr m tmp =
  match tr with
    [] -> (false, m)
  | (qs2,a2)::tr2 ->
       if a=a2 & List.length qs = List.length qs2 then
          let (b, m') = subtype_qs_qs qs qs2 m tmp in
          if b then (true,m')
          else
             subtype_act_tr (qs,a) tr2 m tmp
       else
             subtype_act_tr (qs,a) tr2 m tmp
and subtype_qs_qs qs1 qs2 m tmp =
  match (qs1,qs2) with
    ([], []) -> (true, m)
  | (q1::qs1', q2::qs2') ->
       let (b, m') = subtype_aux q1 q2 m tmp in
         if b then subtype_qs_qs qs1' qs2' m' tmp
         else (false, m')
  | _ -> raise (Fatal "subtype_qs_qs")

(** This should be updated **)
let subtype x y m =
  if x=y then
     (true, m)
  else
  try
     (Hashtbl.find m.sub (x,y), m)
  with Not_found ->
  let (b, m') = subtype_aux x y m [] in
    if b then
      (b, memoize_sub x y m')
    else
      (b, memoize_subn x y m')

let is_subtype x y m = 
  let (b,_)=subtype x y m in b

let get_intersections m q =
  let tr = get_trm q m in
  let qs = List.map 
          (function ([q1],Tepsilon)->q1 
                  | _ -> raise (Fatal "get_intersections"))
           tr 
  in
      qs

let rec get_pairs tr =
  match tr with
   [] -> []
  | ([q1;q2],Tpair)::tr' -> ([q1],[q2])::(get_pairs tr')
  | _::tr' -> get_pairs tr'

let rec is_subtype_qs_q qs q m =
  if is_univstate m q 
  then
     let qs2 = get_intersections m q in
       List.for_all (fun q2 -> is_subtype_qs_q qs q2 m) qs2
       (**  /\qs < q=/\qs2 <--> /\qs < q2 for all q2 in qs2 **)
     else
       List.exists (fun q1 -> is_subtype q1 q m) qs
       (**  /\qs < q= <-- q1 < q for some q1 in qs **)

let string_of_state x = "V"^(string_of_int x)

let dflag = ref false;;

let rec print_ty_automaton x m traversed =
  if List.mem x traversed then
     (string_of_state x,[x])
  else
    (let traversed' =x::traversed in
    let tr = get_trm x m in
      match tr with
       [] -> ("top", [])
     | (_,Tepsilon)::_ ->
         if is_exstate m x then 
            raise (Fatal "print_ty_automaton: Tepsilon in existential state")
         else
            let qs = List.map (fun (qs1,_)->List.hd qs1) tr in
            let (s, referenced) = print_ity qs m traversed' in
              if !dflag || List.mem x referenced then
                  ("["^(string_of_state x)^"]("^s^")", referenced)
              else
                 (s, referenced)
     | _ ->
         if is_univstate m x then 
            raise (Fatal "print_ty_automaton: Tbase in universal state")
         else
            let (s,referenced)= print_tr tr m traversed' in
              if !dflag || List.mem x referenced then
                  ("["^(string_of_state x)^"]("^s^")", referenced)
              else if List.length tr>1 then
                   ("("^s^")", referenced)
                else (s, referenced)
       )
and print_ity qs m traversed =
  match qs with
    [] ->  ("top", [])
  | x::qs' ->
    if List.mem x traversed then
       (string_of_state x,[x])
    else
       let (s1,referenced1) = print_ty_automaton x m traversed in
        if qs'=[] then (s1,referenced1)
        else
          let (s2,referenced2) = print_ity qs' m traversed in
             (s1^"/\\"^s2, referenced1@referenced2)
and print_tr (tr: (state list * alpha) list) m traversed =
  match tr with
    [] -> ("bot", [])
  | (qs,a)::tr' ->
     let (s1, referenced1) = print_onetr (qs,a) m traversed in
       if tr'=[] then (s1,referenced1)
       else
         let (s2,referenced2) = print_tr tr' m traversed in
            (s1^"\\/"^s2, referenced1@referenced2)
and print_onetr (qs,a) m traversed =
  match a with
     Tepsilon -> raise (Fatal "print_tr")
   | Tbase n -> (n, [])
   | Tfun ->
       (match qs with
          [x;y] -> 
              let (s1,referenced1) = print_ty_automaton x m traversed in
              let (s2,referenced2) = print_ty_automaton y m traversed in
                  ("("^s1^"->"^s2^")", referenced1@referenced2)
         | _ -> raise (Fatal "print_onetr: wrong arguments for ->")
        )
   | Tpair->
       (match qs with
          [x;y] -> 
              let (s1,referenced1) = print_ty_automaton x m traversed in
              let (s2,referenced2) = print_ty_automaton y m traversed in
                  ("("^s1^"*"^s2^")", referenced1@referenced2)
         | _ -> raise (Fatal "print_onetr: wrong arguments for *")
        )

let print_ty x m = 
  let (s,_) = print_ty_automaton x m [] in
     print_string s;;

let dprint_ty x m = 
  (dflag := true; print_ty x m; dflag := false)


let rec print_tylist qs m =
  match qs with
    [] -> print_string "\n"
   | q::qs' ->
       (print_string " ";
        print_ty q m;
        print_string "\n";
        print_tylist qs' m)

let rec print_te mte m =
  match mte with
    [] -> ()
  | (f, qs)::mte' ->
      (print_string (f^" :\n");
       print_tylist qs m;
       print_te mte' m)

let dprint_te mte m = 
  (dflag := true; print_te mte m; dflag := false)

let rec image f qs =
  let f' = List.filter (fun (x,y)->List.mem x qs) f in
    delete_duplication_unsorted (List.map snd f')

let rec expand_qs m qs =
  let qs' = image m.gen qs in
    if qs'=[] then qs
    else merge_and_unify compare (expand_qs m qs') qs

let expand_te mte m =
  List.map (fun (f, qs) -> (f, (expand_qs m qs))) mte

let todo() = raise (Fatal "todo")

let empty_te = []

let rec decompose_fty m t =
  if is_univstate m t
  then
    let tr = get_trm t m in
      match tr with
       [([q], Tepsilon)] -> decompose_fty m q
      | _ -> raise (Fatal "decompose_fty: wrong form of function type")
  else
    let tr = get_trm t m in
      match tr with
       [([qarg;qret], Tfun)] ->
            ([qarg],qret)
     | _ ->  raise (Fatal "decompose_fty: non-atomic function type")

let rec mk_vte m vars t =
  match vars with
    [] -> (empty_te, t)
  | v::vars' ->
     let (argty,rty) = decompose_fty m t in
     let (vte',rty') = mk_vte m vars' rty in
       ((v,argty)::vte', rty')

let rec val_of_singletontype m q =
  if is_univstate m q then
    let qs = get_intersections m q in
      val_of_singletontype m (List.hd qs)
  else
    let tr = get_trm q m in
    match tr with
      [([], Tbase s)] -> int_of_string s
    | _ -> raise (Fatal ("val_of_singletontype: non-singleton type: "^(string_of_int q)))

let cache_tl2ty = ref [];;
let add_to_cache_tl2ty tl x =
   (cache_tl2ty := (tl,x)::!cache_tl2ty);;

let add_to_univ m qs =
  m.univ := qs@(!(m.univ));;

let tl2ty m tl =
  match tl with
    [q] -> q
  | _ -> 
     try
       List.assoc tl !cache_tl2ty
     with Not_found ->
       let x = gen_newstate() in
       let tr = List.map (fun q1 -> ([q1],Tepsilon)) tl in
        (delta_add m.delta x tr;
         add_to_univ m [x];
         add_to_cache_tl2ty tl x;
         x)
let cache_pair = ref [];;
let add_to_cache_pair (q1,q2) x =
   cache_pair := ((q1,q2),x)::!cache_pair

let pair2ty m q1 q2 =
  try
    List.assoc (q1,q2) !cache_pair
  with Not_found ->
(**
    let _ = if !debugging then
               (print_string "creating pair types for:\n";
                print_ty q1 m;
                print_string "\n";
                print_ty q2 m;
                print_string "\n"; flush stdout)
            else () in
**)
    let x = gen_newstate() in
    let tr = [([q1;q2],Tpair)] in
        (delta_add m.delta x tr;
         add_to_cache_pair (q1,q2) x;
         x)

let cache_disjunct = ref [];;
let add_to_cache_disjunct qs x =
   (cache_disjunct := (qs,x)::!cache_disjunct);;

let mk_disjunction (m:automaton) qs =
  match qs with
     [q] -> q
   | _ ->
     try
       List.assoc qs !cache_disjunct
     with Not_found ->
       let x = gen_newstate() in
       let trs = List.map (fun q1 ->
                           (get_trm q1 m)) qs in
       let tr = delete_duplication_unsorted (List.flatten trs) in
        (delta_add m.delta x tr;
         add_to_cache_disjunct qs x;
         x)
         
let mk_pairty m ty1 ty2: state list = 
  let q1 = tl2ty m ty1 in
  let q2 = tl2ty m ty2 in
    [pair2ty m q1 q2]

let rec decompose_pairty m q =
  if is_univstate m q 
  then
    let qs = get_intersections m q in
      match qs with
       [q1] -> decompose_pairty m q1
     | _ ->  raise (Fatal "decompose_pairty: intersection of pair types is not supported")
  else
    get_pairs (get_trm q m)
and decompose_pairtys m qs =
  match qs with
    [] -> []
  | [q] -> decompose_pairty m q
  | _ -> 
     raise (Fatal "decompose_pairtys: intersection of pair types is not supported")
    
let rec fst_of_pairty m ty = 
  let tys = List.map (fun q -> fst_of_apairty m q) ty in
    delete_duplication_unsorted (List.flatten tys)
and fst_of_apairty m q =
  if is_univstate m q 
  then
    let tr = get_trm q m in
    let qs = List.map 
            (function ([q1],Tepsilon)->q1 | _ -> raise (Fatal "fst_of_apairty"))
             tr 
    in fst_of_pairty m qs
  else
    let tr = List.filter
             (function (_,Tpair) -> true | _ -> false)
             (get_trm q m) in
    let qs = List.map 
            (function ([q1;q2],Tpair)->q1 | _ -> raise (Fatal "fst_of_apairty:2"))
             tr  in
    let q1 = mk_disjunction m qs 
    in [q1]

let rec snd_of_pairty m ty = 
  let tys = List.map (fun q -> snd_of_apairty m q) ty in
    delete_duplication_unsorted (List.flatten tys)
and snd_of_apairty m q =
  if is_univstate m q 
  then
    let tr = get_trm q m in
    let qs = List.map 
            (function ([q1],Tepsilon)->q1 | _ -> raise (Fatal "snd_of_apairty"))
             tr 
    in snd_of_pairty m qs
  else
    let tr = List.filter
             (function (_,Tpair) -> true | _ -> false)
             (get_trm q m) in
    let qs = List.map 
            (function ([q1;q2],Tpair)->q2 | _ -> raise (Fatal "snd_of_apairty:2"))
             tr in
    let q1 = mk_disjunction m qs 
    in [q1]


let singletontype_of_value n = Typing.ITbase(string_of_int n)

let lookup_te f te = List.assoc f te
let update_te f ty te =
  let (ty',te1) = list_assoc2 f te in
    (f,ty)::te1
    
let compute_type_head m h te nte vte cte = 
  match h with
    NT(f) -> List.rev_append (lookup_te f nte) (lookup_te f te) 
  | T(a) -> List.assoc a cte
  | FD(n) -> let (x,m') =
              aty2automaton (singletontype_of_value n) m
             in
               [x]
             (** TO DO: cache should also be updated **)
  | Var(v) -> List.assoc v vte
  | CASE(_) -> raise (Fatal "compute_type_head: case")
  | PAIR -> raise (Fatal "compute_type_head: pair")
  | DPAIR -> raise (Fatal "compute_type_head: dpair")


let rec get_tyfun m tr =
  match tr with
    [([qarg;qret], Tfun)] -> (qarg,qret)
  | _ -> raise (Fatal "get_tyfun")

let rec range_type m q argty = 
  if is_univstate m q 
  then
      let qs = get_intersections m q in
      range_type_qs m qs argty
  else
    let tr = get_trm q m in
    let (qarg,qret) = get_tyfun m tr in
       if is_subtype_qs_q argty qarg m
       then [qret]
       else []
and range_type_qs m qs argty =
  match qs with
    [] -> []
  | q::qs' ->
     let rt1 = range_type m q argty in
     let rt2 = range_type_qs m qs' argty in
       merge_and_unify compare rt1 rt2
(**
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
**)
let rec range_types m ftys argtys =
  match argtys with
    [] -> ftys
  | ty::argtys' ->
      let t = range_type_qs m ftys ty in
        range_types m t argtys'



let state_intersection qs1 qs2 =
  List.filter (fun q->List.mem q qs2) qs1
let rec state_intersections qss =
  match qss with
     [] -> []
   | [qs] -> qs
   | qs::qss' -> 
       state_intersection
        qs
       (state_intersections qss')

let rec compute_type m term te nte vte cte: state list =
  match term with
    App(CASE(k), t::terms) ->
      (match compute_type m t te nte vte cte with
        [] -> []
       | aty::_ ->
          let n = val_of_singletontype m aty in
          let t1 = List.nth terms n in
            compute_type m t1 te nte vte cte
      )
  | App(PAIR, [t1;t2]) ->
      let ty1 = compute_type m t1 te nte vte cte in
      let ty2 = compute_type m t2 te nte vte cte in
        mk_pairty m ty1 ty2
  | App(PAIR, _) ->
        raise (Fatal "Generalize.compute_type: pair constructor has a wrong number of arguments")
  | App(DPAIR, [t1;t2]) ->
      let ty1 = compute_type m t1 te nte vte cte in
      if ty1=[] then []
      else
      let ty2 = compute_type m t2 te nte vte cte in
      let typl = decompose_pairtys m ty1 in
      let tll = List.map 
               (fun (t1,t2)-> range_types m ty2 [t1;t2])
                typl
      in
         state_intersections tll
      (** let ty11 = fst_of_pairty m ty1 in
       ** let ty12 = snd_of_pairty m ty1 in
       **  range_types m ty2 [ty11;ty12] 
       **)
  | App(h, terms) ->
      let ty_h = compute_type_head m h te nte vte cte in
      let tl = compute_type_tl m terms te nte vte cte in
      let ty = range_types m ty_h tl in
        ty
and compute_type_tl m terms te nte vte cte =
  List.map (fun term-> compute_type m term te nte vte cte) terms


let has_type m body rty te nte vte cte = 
  let tl = compute_type m body te nte vte cte in
    List.exists (fun t->is_subtype t rty m) tl

(*** checks whether the body of f has type at ***)
let rec check_aty (m:automaton) (f: nameNT) (at: state) te nte cte g =
  let (vars, body) = get_def f g in
  let (vte, rang_ty) =  mk_vte m vars at in
    has_type m body rang_ty te nte vte cte 


let rec gfp (m:automaton) te cte nte unchecked dmap g =
  match unchecked with
    [] -> te
  | f::unchecked' ->
     let fty = lookup_te f te in
     let fty' = List.filter (fun aty -> check_aty m f aty te nte cte g) fty in
       if List.length fty=List.length fty' then
         gfp m te cte nte unchecked' dmap g
       else
         let to_be_checked = List.assoc f dmap in
         let te' = update_te f fty' te in
           gfp m te' cte nte (merge_and_unify compare unchecked' to_be_checked) dmap g

let compute_te (m:automaton) te cte nte dmap g =
  let unchecked = List.map fst (g.nt) in
  let te' = gfp m te cte nte unchecked dmap g in
   te';;


let diff_type m q1 q2 = todo()
  (* return the difference between q1 and q2 and its depth;
     By the difference, we mean the pair (q1',q2') such that
      q1 = C[q1'], q2=C[q2'], and q1' = C1[q2'] for some C and C1
   *)


type diff = Same | Diff of ((state * state) * int * bool) list

let add_to_diff (p: (state * state)*int*bool) d =
  match d with
      Diff x -> Diff (p::x)
    | Same -> Same

let topty m =
  List.assoc [] m.cache_ty

let filter_same_state qs1 qs2 visited =
  List.filter 
  (fun q1 -> not(List.exists (fun q2-> q1=q2||List.mem (q1,q2) visited) qs2) )
  qs1

let matching_act visited (qs1,act1) (qs2,act2) =
  if act1=act2 then
    let qqs = List.combine qs1 qs2 in
      List.for_all 
      (fun (q1,q2)->q1=q2||List.mem (q1,q2) visited)
      qqs
  else false

let filter_same_act tr1 tr2 visited =
  List.filter
   (fun (qs1,act1) ->
      not(List.exists (matching_act visited (qs1,act1)) tr2))
  tr1;;


let rec diff_type m q1 q2 visited depth polarity =
  if q1=q2 || List.mem (q1,q2) visited then
     Same
  else if is_univstate m q1 then
    let qs1 = get_intersections m q1 in
      if is_univstate m q2 then
        let qs2 = get_intersections m q2 in
          let d = diff_tl m qs1 qs2 ((q1,q2)::visited) (depth+1) polarity in
            add_to_diff ((q1,q2),depth,polarity) d
      else
         let d = diff_tl m qs1 [q2] ((q1,q2)::visited) (depth+1) polarity in
            add_to_diff ((q1,q2),depth,polarity) d
  else if is_univstate m q2 then
    let qs2 = get_intersections m q2 in
    let d = diff_tl m [q1] qs2 ((q1,q2)::visited) (depth+1) polarity in
            add_to_diff ((q1,q2),depth,polarity) d
  else
    let tr1 = get_trm q1 m in
    let tr2 = get_trm q2 m in
    let d = diff_tr m tr1 tr2 visited (depth+1) polarity in
      add_to_diff ((q1,q2),depth,polarity) d

and diff_tl m qs1 qs2 visited depth polarity =
  let qs1' = filter_same_state qs1 qs2 visited in
  let qs2' = filter_same_state qs2 qs1 visited in
    if List.length qs1'>1 || List.length qs2'>1 then
        Diff []
    else if qs1'=[] &&qs2'=[] then
       Same
    else if qs1'=[]||qs2'=[] then
       Diff []
        (** Diff (topty m, List.hd qs2')**)
    else
       diff_type m (List.hd qs1') (List.hd qs2') visited depth polarity

and diff_tr m tr1 tr2 visited depth polarity = 
  let tr1' = filter_same_act tr1 tr2 visited in 
  let tr2' = filter_same_act tr2 tr1 visited in 
    if List.length tr1'>1 || List.length tr2'>1 then
         Diff []
    else if tr1'=[]&&tr2'=[] then
       Same
    else if tr1'=[]||tr2'=[] then
       Diff([])  
    else 
       let (qs1,act1) = List.hd tr1' in
       let (qs2,act2) = List.hd tr2' in
         if act1=act2 then
           diff_qs m qs1 qs2 visited depth act1 polarity
         else
            Diff []
and diff_qs m qs1 qs2 visited depth act pol =
  let qqs = List.combine qs1 qs2 in
  let pols = if act=Tfun then [not(pol);pol] else mk_list (List.length qs1) pol in
  let qqps = List.combine qqs pols in
  let qqps' = List.filter (fun ((q1,q2),p)->not(q1=q2||List.mem (q1,q2) visited)) qqps in
    match qqps' with
      [] -> Same
    | [((q1,q2),pol)] -> diff_type m q1 q2 visited depth pol
    | _ -> Diff []

(* Check whether q1=C[q2] and returns Some(C) or None *)
let rec reachable m q1 q2 visited =
  if q1=q2 then Some([])
  else if List.mem q1 visited then None
  else
   let tr = get_trm q1 m in
     reachable_tr m tr q2 (q1::visited)

and reachable_tr m tr q visited =
  match tr with
    [] -> None
  | (qs,act)::tr' ->
      let r = reachable_qs m qs q visited 0 in
        match r with
          Some(p,ind) -> Some(((qs,act), ind)::p)
        | None -> reachable_tr m tr' q visited
and reachable_qs m qs q visited ind =
  match qs with
    [] -> None
  | q1::qs' ->
      let r = reachable m q1 q visited in
        match r with
           Some(p) -> Some(p,ind)
         | None -> reachable_qs m qs' q visited (ind+1)

let rec select_deepest d =
  if d=[] then []
  else
    let d' = List.sort
             (fun (_,d1,_,_)->fun (_,d2,_,_)-> compare d2 d1)
             d
    in [List.hd d']

let select_diff m d =
  match d with
    Same -> []
  | Diff l ->
     let l1 = List.filter (fun (_,depth,_) -> depth>0) l in
     let l2 =
       List.fold_left
        (fun l -> fun ((q1,q2),depth,pol) ->
          let r1 = reachable m q1 q2 [] in
            match r1 with
              Some(p) -> ((q1,q2),depth,pol,p)::l
            | None ->
              (let r2 = reachable m q2 q1 [] in
                match r2 with
                  Some(p') -> ((q2,q1),depth,pol,p')::l
                | None -> l))
         [] l1
   in select_deepest l2

let rec print_diff d m =
  match d with
    [] -> ()
  | ((q1,q2),_,_,_)::d' -> 
       (print_string "Diff: \n  ";
        print_ty q1 m;
        print_string "\n  ";
        print_ty q2 m;
        print_string "\n";
        print_diff d' m
        )
let diff_in_tl tl m =
  let qq = combination2 tl in
    List.fold_left
      (fun d -> fun (q1,q2) ->
(**        merge_and_unify compare 
**)
        (select_diff m (diff_type m q1 q2 [] 0 true))@d)
      [] qq

let rec diff_in_te te m =
  match te with
     [] -> []
   | (f,tl)::te' ->
        let d1 = diff_in_tl tl m in
        let d2 = diff_in_te te' m in
              d1@d2
(**           merge_and_unify compare d1 d2**)

let rec diff_in_te' te m =
  match te with
     [] -> []
   | (f,tl)::te' ->
        let d1 = diff_in_tl tl m in
        let d2 = diff_in_te' te' m in
           (f, d1)::d2

let rec copy_path p q1 q2 =
  match p with
    [] -> []
  | [((qs,act),ind)] ->
      let qs' = list_repl ind q2 qs in
        [((qs',act),q1)]
  | ((qs,act),ind)::p' ->
      let q = gen_newstate() in
      let delta1 = copy_path p' q q2 in
      let qs' = list_repl ind q qs in
        ((qs',act),q1)::delta1

let cache_refinement = ref []
let add_to_cache_refinement q1 q2 =
   cache_refinement := (q1,q2)::!cache_refinement
let mem_refinement q1 q2 =
   List.mem (q1,q2) !cache_refinement


let merge_type m q1 q2 p =
  let q3 = gen_newstate() in
  let delta1 = copy_path p q3 q3 in
  let delta2 = List.map(fun x -> (x,q3)) (get_trm q2 m) in
  let delta3 = delta1@delta2 in
  let _ = delta_add_trlist m.delta delta3 in
     q3


let rec is_refinement_aux q1 q2 m visited =
  if q1=q2 || (mem_refinement q1 q2) || List.mem (q1,q2) visited 
  then true
  else 
  let visited' = (q1,q2)::visited in
   if is_univstate m q1 || is_univstate m q2
    then
      let qs1 = if is_univstate m q1 
                then get_intersections m q1
                else [q1]
      in
      let qs2 = if is_univstate m q2 
                then get_intersections m q2
                else [q2]
      in
         is_refinement_qs qs1 qs2 m visited'
  else 
     let tr1 = get_trm q1 m in
     let tr2 = get_trm q2 m in
         is_refinement_tr tr1 tr2 m visited'
and is_refinement_qs qs1 qs2 m visited =
  List.for_all 
  (fun q1 -> List.exists (fun q2->is_refinement_aux q1 q2 m visited) qs2)
  qs1
and is_refinement_tr tr1 tr2 m visited =
  List.for_all 
  (fun a1 -> List.exists (fun a2->is_refinement_act a1 a2 m visited) tr2)
  tr1
and is_refinement_act (qs1,act1) (qs2,act2) m visited =
  if act1=act2 then
     is_refinement_seq qs1 qs2 m visited
  else
     false
and is_refinement_seq qs1 qs2 m visited =
  match (qs1,qs2) with
    ([], []) -> true
  | (q1::qs1', q2::qs2') ->
       (is_refinement_aux q1 q2 m visited) 
      && (is_refinement_seq qs1' qs2' m visited)
  | _ -> false (* This case should not happen, actually *)

let is_refinement q1 q2 m =
  if is_refinement_aux q1 q2 m [] 
  then 
     (add_to_cache_refinement q1 q2; true)
  else
     false

let cache_tr = ref [];;
let lookup_cache_tr tr = 
  List.assoc tr !cache_tr
let add_to_cache_tr tr q =
  cache_tr := (tr,q)::!cache_tr

(* tr should be sorted? *)
let mk_newstate tr m flag =
  try
     lookup_cache_tr tr
  with
   Not_found -> 
     let q = gen_newstate() in
     let _ = if flag then
                 m.univ := q::!(m.univ) 
             else () in
     let _ = delta_add m.delta q tr in
     let _ = add_to_cache_tr tr q in
        q

let rec gen_type m q qs visited = 
  if List.mem q visited then
    []
  else
    let (qs1,qs2) = 
         list_filter2 
              (fun q1 -> is_refinement q q1 m) qs in
    let tr = get_trm q m in
    let trs = gen_tr m tr qs2 (q::visited) in
    let flag = is_univstate m q in
    let qs3 = List.map (fun tr1 -> mk_newstate tr1 m flag) trs in
       merge_and_unify compare qs1 qs3
and gen_tr m tr qs visited =
  match tr with
    [] -> []
  | onetr::tr' ->
     let onetrs = gen_onetr m onetr qs visited in
     let trs = gen_tr m tr' qs visited in
        comb_trs onetr onetrs tr' trs
and comb_trs onetr onetrs tr trs =
  let trs1 = List.map (fun tr1-> onetr::tr1) trs in
  let trs2 =
    List.flatten 
       (List.map (fun onetr1 ->
           List.map (fun tr1 -> onetr1::tr1) (tr::trs)) onetrs) in
    trs1@trs2
and gen_onetr m (qs1,act) qs visited =
  let qss = gen_qs m qs1 qs visited in
    List.map (fun qs2 -> (qs2,act)) qss
and gen_qs m qs1 qs visited =
  match qs1 with
    [] -> []
  | q::qs1' ->
      let qs2 = gen_type m q qs visited in
      let qss2 = gen_qs m qs1' qs visited in
      let qss3 = 
         List.map (fun qs3->q::qs3) qss2 in
      let qss4 =
       List.flatten
        (List.map (fun q4 ->
          List.map (fun qs4->q4::qs4) (qs1'::qss2)) qs2) in
        qss3@qss4

let rec gen_te te m qs =
  match te with
     [] -> []
   | (f,qs1)::te' ->
       let qs1' = gen_tys qs1 m qs in
       let te1 = gen_te te' m qs in
           (f, qs1')::te1
and gen_tys qs1 m qs =
  match qs1 with
    [] -> []
  | q::qs1' -> 
       let qs2 = gen_type m q qs [] in
       let qs3 = gen_tys qs1' m qs in
         merge_and_unify compare qs2 qs3

let rec unionTE te1 te2 = 
  match te1 with
     [] -> te2
   | (f,ty)::te1' ->
        let (ty2,te2') = list_assoc2 f te2 in
          (f, ty@ty2)::(unionTE te1' te2')

let rec filter_out_suptype mte1 mte2 m =
  match mte1 with
    [] -> []
  | (f,ty1)::mte1' ->
       let ty2 = List.assoc f mte2 in
       let ty1' = List.filter 
                 (fun aty1 -> 
                   not(List.exists (fun aty2 ->
                            (is_subtype aty2 aty1 m)) ty2))
                  ty1
       in
       let mte1'' = filter_out_suptype mte1' mte2 m in
          (f, ty1')::mte1''

let rec filter_out_selfsuptype ty m =
  match ty with
    [] -> []
  | aty::ty' ->
     if List.exists (fun aty1->is_subtype aty1 aty m) ty'
     then
        filter_out_selfsuptype ty' m
     else
        let ty'' =
            List.filter (fun aty1 -> not(is_subtype aty aty1 m)) ty'
        in
           aty::(filter_out_selfsuptype ty'' m)
let filter_out_selfsuptype_te mte m =
  List.map 
 (fun (f,ty) -> 
   (f, filter_out_selfsuptype ty m))
  mte

exception NoDiff

let diff_most_important diffs gcache =
  let diffs' = List.map (fun ((q1,q2),_,_,path) -> (q1,q2,path)) diffs in
  let l = list_count diffs' in
  let l' = List.filter
            (fun (x,_) -> not(List.exists (fun (y,_)->x=y) gcache)) l in
   if l'=[] then
       raise NoDiff
   else
    let d = list_max (fun (_,x)->fun (_,y)-> compare x y) l' in
        fst d


let rec repeat_gen mte mcte m dmap g limit gcache =
  if limit<=0 then ()
  else
   try
    let diffs = diff_in_te mte m in
    let (q1,q2,path) = diff_most_important diffs gcache in
    let q = merge_type m q1 q2 path in
    let _ = print_string ("generalized type:\n") in
    let _ = print_ty q m in
    let _ = print_string ("\n\n") in
    let gcache' = ((q1,q2,path),q)::gcache in
    let _ = show_time() in
    let _ = debug "phase 2" in
    let mte2 = gen_te mte m [q] in
    let _ = show_time() in
    let _ = debug "phase 3" in
    let mte3 = filter_out_suptype mte2 mte m in
    let _ = show_time() in
    let _ = debug "phase 4" in
    let mte4 = compute_te m mte3 mcte mte dmap g in
    let _ = show_time() in
    let _ = debug "phase 5" in
    let mte4' = filter_out_selfsuptype_te mte4 m in
    let mte5 = filter_out_suptype mte mte4' m in
    let mte6 = unionTE mte4' mte5 in
    let _ = show_time() in
    let _ = debug "phase 5 end" in
    let _ = print_string "generalized TE:\n" in
    let _ = print_te mte6 m in
    let _ = print_string ("generalization limit: "^(string_of_int (limit-1))^"\n") in
      repeat_gen mte6 mcte m dmap g (limit-1) gcache'
   with
    NoDiff -> print_string "No more candidate for generalization\n"


let generalize te cte dmap g limit =
  let _ = debugging := true in
  let _ = show_time() in
  let _ = debug "phase 1" in
  let (mte1, m1) = te2automaton te empty_automaton in
  let (mcte, m1) = te2automaton cte m1 in
    repeat_gen mte1 mcte m1 dmap g limit


