
let name = "Knuth-Bendix"

let length l =
  let rec j(k,x) =
    match x with
      [] -> k
    | a::x -> j(k+1,x)
  in
  j(0,l)

let rec (@) r l =
  match r with
    [] -> l
  | a::r -> a :: (r@l)

let rev l =
  let rec f (r, h) =
    match r with
      [] -> h
    |  a::r -> f(r, a::h)
  in
  f(l,[])

let app f =
  let rec app_rec l =
    match l with
       [] -> ()
     | a::l -> (f a; app_rec l)
  in
  app_rec

let map f =
  let rec map_rec l =
    match l with
      [] -> []
    | a::l -> f a :: map_rec l
  in
  map_rec


(******* Quelques definitions du prelude CAML **************)

exception Failure of string

let failwith s = raise(Failure s)

let fst (x,y) = x
let snd (x,y) = y


let it_list f =
  let rec it_rec a l =
    match l with
      [] -> a
    | (b::l) -> it_rec (f a b) l
  in it_rec

let it_list2 f =
  let rec it_rec a l1 l2 =
    match l1, l2 with
    | [],       []    -> a
    | (a1::l1), (a2::l2) -> it_rec (f a (a1,a2)) l1 l2
    | _ -> failwith "it_list2"
  in it_rec

let exists p =
  let rec exists_rec l =
    match l with
      [] -> false
    | (a::l) -> (p a) || (exists_rec l)
  in exists_rec

let for_all p =
  let rec for_all_rec l =
    match l with
      [] -> true
    | (a::l) -> (p a) && (for_all_rec l)
  in for_all_rec

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  |(x::l1) -> rev_append l1 (x::l2)

let try_find f =
  let rec try_find_rec l =
    match l with
      []    -> failwith "try_find"
     | (a::l) -> try (f a) with Failure _ -> try_find_rec l
  in try_find_rec

let partition p =
  let rec part_rec l =
    match l with
      []  -> ([],[])
        |  (a::l) ->
             let (pos,neg) = part_rec l in
              if p a then  ((a::pos), neg) else (pos, (a::neg))
  in part_rec


(* 3- Les ensembles et les listes d'association *)

let mem a =
  let rec mem_rec l =
    match l with
      [] -> false
        | (b::l) -> (a=b) || mem_rec l
  in mem_rec

let union l1 l2 =
  let rec union_rec l =
    match l with
      [] -> l2
    | (a::l) ->
        if mem a l2 then union_rec l else a :: union_rec l
  in union_rec l1

let mem_assoc a =
  let rec mem_rec l =
    match l with
      [] -> false
        | ((b,_)::l) -> (a=b) || mem_rec l
  in mem_rec

let assoc a =
  let rec assoc_rec l =
    match l with
      [] -> failwith "find"
    | ((b,d)::l) -> if a=b then d else assoc_rec l
  in assoc_rec

(* 4- Les sorties *)

let print s = print_string s
let print_string = print
let print_num x = print (string_of_int x)
let print_newline () = print "\n"
let message s = (print s; print "\n")

(* 5- Les ensembles *)

let union l1 =
  let rec union_rec l =
    match l with
      [] -> l1
    |  (a::l) -> if mem a l1 then union_rec l else a :: union_rec l
  in union_rec


(****************** Term manipulations *****************)

type term
  = Var of int
  | Term of string * term list

let rec vars t =
  match t with
    (Var n) -> [n]
  | (Term(_,l)) ->
      let rec vars_of_list ts =
        match ts with
          [] -> []
        | (t::r) -> union (vars t) (vars_of_list r)
      in
      vars_of_list l

let substitute subst =
  let rec subst_rec t =
    match t with
      (Term(oper,sons)) -> Term(oper, map subst_rec sons)
    | ((Var n))     -> try (assoc n subst) with Failure _ -> t
  in subst_rec

let change f =
  let rec change_rec t n =
    match t with
      (h::t) -> if n=1 then f h :: t
                else h :: change_rec t (n-1)
    | _      -> failwith "change"
  in change_rec


(* Term replacement replace M u N => M[u<-N] *)
let replace mm u nn =
  let rec reprec t u =
    match t, u with
      (_, []) -> nn
    | (Term(oper,sons), (n::u)) ->
             Term(oper, change (fun p -> reprec p u) sons n)
    | _ -> failwith "replace"
  in reprec mm u


(* matching = - : (term -> term -> subst) *)
let matching term1 term2 =
  let rec match_rec subst (t1, t2) =
    match t1,t2 with
      (Var v, mm) ->
          if mem_assoc v subst then
            if mm = assoc v subst then subst else failwith "matching"
          else
            (v,mm) :: subst
        | (Term(op1,sons1), Term(op2,sons2)) ->
	  if op1 = op2 then it_list2 match_rec subst sons1 sons2
                       else failwith "matching"
        | _ -> failwith "matching"
  in match_rec [] (term1,term2)


(* A naive unification algorithm *)

let compsubst subst1 subst2 =
  (map (fun (v,t) -> (v, substitute subst1 t)) subst2) @ subst1

let occurs n =
  let rec occur_rec t =
    match t with
      (Var m) -> (m=n)
        |  (Term(_,sons)) -> exists occur_rec sons
  in occur_rec

let rec unify (term1, term2) =
  match term1, term2 with
    ((Var n1)), _ ->
      if term1 = term2 then []
      else if occurs n1 term2 then failwith "unify"
      else [(n1,term2)]
  | (term1, Var n2) ->
      if occurs n2 term1 then failwith "unify"
      else [(n2,term1)]
  | (Term(op1,sons1), Term(op2,sons2)) ->
      if op1 = op2 then
	it_list2 (fun s -> fun (t1,t2) -> compsubst (unify(substitute s t1,
                                                         substitute s t2)) s)
                 [] sons1 sons2
      else failwith "unify"

(* We need to print terms with variables independently from input terms
  obtained by parsing. We give arbitrary names v1,v2,... to their variables. *)

let infixes = ["+";"*"]

let rec pretty_close pt m =
  match m with
    Term(oper, _) ->
      if mem oper infixes then
        (print_string "("; pt m; print_string ")")
      else pt m
  | m -> pt m
let rec pretty_term t =
  match t with
    (Var n) ->
      (print_string "v"; print_num n)
  | (Term (oper,sons)) ->
      if mem oper infixes then
        match sons with
            [s1;s2] ->
              (pretty_close pretty_term s1; print_string oper; pretty_close pretty_term s2)
          | _ ->
              failwith "pretty_term : infix arity <> 2"
      else
       (print_string oper;
        match sons with
	     []   -> ()
          | t::lt ->(print_string "(";
                     pretty_term t;
                     app (fun t -> (print_string ","; pretty_term t)) lt;
                     print_string ")"))

(****************** Equation manipulations *************)

(* standardizes an equation so its variables are 1,2,... *)

let mk_rule m n =
  let all_vars = union (vars m) (vars n) in
      let (k,subst) =
        it_list (fun (i,sigma) -> fun v -> (i+1,(v,Var(i))::sigma))
               (1,[]) all_vars
  in (k-1, (substitute subst m, substitute subst n))

(* checks that rules are numbered in sequence and returns their number *)
let check_rules l = it_list (fun n -> fun (k,_) ->
	  if k=n+1 then k else failwith "Rule numbers not in sequence")
	0 l

let pretty_rule (k,(n,(mm,nn))) =
 (print_num k; print_string " : ";
  pretty_term mm; print_string " = "; pretty_term nn;
  print_newline())

let pretty_rules l = app pretty_rule l


(****************** Rewriting **************************)

(* Top-level rewriting. Let eq:L=R be an equation, M be a term such that L<=M.
   With sigma = matching L M, we define the image of M by eq as sigma(R) *)
let reduce l mm =
  substitute (matching l mm)

(* A more efficient version of can (rewrite1 (L,R)) for R arbitrary *)
let reducible l =
  let rec redrec m =
    try
    (matching l m; true)
    with Failure _ ->
      match m with
        Term(_,sons) -> exists redrec sons
      |        _     -> false
  in redrec

(* mreduce : rules -> term -> term *)
let mreduce rules m =
  let rec redex (_,(_,(l,r))) = reduce l m r in try_find redex rules

(* One step of rewriting in leftmost-outermost strategy, with multiple rules *)
(* fails if no redex is found *)
(* mrewrite1 : rules -> term -> term *)
let mrewrite1 rules =
  let rec rewrec m =
    try (mreduce rules m) with Failure _ ->
      let rec tryrec x =
        match x with
          [] -> failwith "mrewrite1"
        | (son::rest) ->
            try
            (rewrec son :: rest) with Failure _ -> son :: tryrec rest
      in match m with
          Term(f, sons) -> Term(f, tryrec sons)
        | _ -> failwith "mrewrite1"
  in rewrec

(* Iterating rewrite1. Returns a normal form. May loop forever *)
(* mrewrite_all : rules -> term -> term *)
let mrewrite_all rules m =
  let rec rew_loop m =
    try
    rew_loop(mrewrite1 rules m)
    with Failure _ -> m
  in rew_loop m

(*
pretty_term (mrewrite_all Group_rules M where M,_=<<A*(I(B)*B)>>);;
==> A*U
*)


(************************ Recursive Path Ordering ****************************)

type ordering = Greater | Equal | NotGE

let ge_ord order pair = match order pair with NotGE -> false | _ -> true
let gt_ord order pair = match order pair with Greater -> true | _ -> false
let eq_ord order pair = match order pair with Equal -> true | _ -> false

let rem_eq equiv =
  let rec remrec x l =
    match l with
      [] -> failwith "rem_eq"
    | (y::l) -> if equiv (x,y) then l else y :: remrec x l
  in remrec

let diff_eq equiv (x,y) =
  let rec diffrec p =
    match p with
      ([],_) -> p
    | ((h::t), y) ->
        try
            diffrec (t,rem_eq equiv h y)
        with Failure _ ->
              let (x',y') = diffrec (t,y) in (h::x',y')
  in if length x > length y then diffrec(y,x) else diffrec(x,y)


(* multiset extension of order *)
let mult_ext order t1 t2 =
  match t1, t2 with
    (Term(_,sons1), Term(_,sons2)) ->
      (match diff_eq (eq_ord order) (sons1,sons2) with
           ([],[]) -> Equal
         | (l1,l2) ->
             if for_all (fun n -> exists (fun m -> order (m,n) = Greater) l1) l2
             then Greater else NotGE)
  | (_, _) -> failwith "mult_ext"

(* lexicographic extension of order *)
let lex_ext order (m, n) =
  match m, n with
    ((Term(_,sons1)), (Term(_,sons2))) ->
      let rec lexrec (l1, l2) =
        match l1,l2 with
          ([] , []) -> Equal
            |  ([] , _ ) -> NotGE
            |  ( _ , []) -> Greater
            |  (x1::l1, x2::l2) ->
                match order (x1,x2) with
                  Greater -> if for_all (fun n' -> gt_ord order (m,n')) l2
                             then Greater else NotGE
                | Equal -> lexrec (l1,l2)
                | NotGE -> if exists (fun m' -> ge_ord order (m',n)) l1
                           then Greater else NotGE
      in lexrec (sons1, sons2)
  | _ -> failwith "lex_ext"

(* recursive path ordering *)

let rpo op_order ext =
  let rec rporec (mm,nn) =
    if mm=nn then Equal else
      match mm with
          Var mm -> NotGE
        | Term(op1,sons1) ->
            match nn with
                Var n ->
                  if occurs n mm then Greater else NotGE
              | Term(op2,sons2) ->
                  match (op_order op1 op2) with
                      Greater ->
                        if for_all (fun nn' -> gt_ord rporec (mm,nn')) sons2
                        then Greater else NotGE
                    | Equal ->
                        ext rporec (mm,nn)
                    | NotGE ->
                        if exists (fun mm' -> ge_ord rporec (mm',nn)) sons1
                        then Greater else NotGE
  in rporec


(****************** Critical pairs *********************)

(* All (u,sig) such that N/u (&var) unifies with M,
   with principal unifier sig *)

let super mm =
  let rec suprec nn =
    match nn with
      (Term(_,sons)) ->
begin
      let rec collate (pairs,n) son =
                (pairs @ map (fun (u,sigma) -> (n::u,sigma)) (suprec son), n+1) in
      let insides =
                fst (it_list collate ([],1) sons)
      in try  ([], unify(mm,nn)) :: insides  with Failure _ -> insides
end
    | _ -> []
  in suprec

(* Ex :
let (M,_) = <<F(A,B)>>
and (N,_) = <<H(F(A,x),F(x,y))>> in super M N;;
=-> [[1],[2,Term ("B",[])];                      x <- B
     [2],[2,Term ("A",[]); 1,Term ("B",[])]]     x <- A  y <- B
*)

(* All (u,sigma), u&[], such that N/u unifies with mm *)
(* super_strict : term -> term -> (num list & subst) list *)

let super_strict m n =
  match n with (Term(_,sons)) ->
        let rec collate (pairs,n) son =
          (pairs @ map (fun (u,sigma) -> (n::u,sigma)) (super m son), n+1)
        in fst (it_list collate ([],1) sons)
  |  _ -> []

(* Critical pairs of L1=R1 with L2=R2 *)
(* critical_pairs : term_pair -> term_pair -> term_pair list *)
let critical_pairs (l1,r1) (l2,r2) =
  let rec mk_pair (u,sigma) =
     (substitute sigma (replace l2 u r1), substitute sigma r2) in
  map mk_pair (super l1 l2)

(* Strict critical pairs of l1=R1 with l2=R2 *)
(* strict_critical_pairs : term_pair -> term_pair -> term_pair list *)
let strict_critical_pairs (l1,r1) (l2,r2) =
  let rec mk_pair (u,sigma) =
    (substitute sigma (replace l2 u r1), substitute sigma r2) in
  map mk_pair (super_strict l1 l2)

(* All critical pairs of eq1 with eq2 *)
let mutual_critical_pairs eq1 eq2 =
  (strict_critical_pairs eq1 eq2) @ (critical_pairs eq2 eq1)

(* Renaming of variables *)

let rename n (t1,t2) =
  let rec ren_rec t =
    match t with
      (Var k) -> Var(k+n)
    | (Term(oper,sons)) -> Term(oper, map ren_rec sons)
  in (ren_rec t1, ren_rec t2)

(************************ Completion ******************************)

let deletion_message (k,_) =
  (print_string "rule ";print_num k; message " deleted")

(* Generate failure message *)
let non_orientable (m,n) =
  (pretty_term m; print_string " = "; pretty_term n; print_newline())

(* Improved Knuth-Bendix completion procedure *)
(* kb_completion : (term_pair -> bool) -> num -> rules -> term_pair list -> (num & num) -> term_pair list -> rules *)
let kb_completion greater =
  let rec kbrec n rules =
    let normal_form = mrewrite_all rules in
    let get_rule k = assoc k rules in
    let rec process failures =
      let rec processf (k,l) =
        let rec processkl eqs =
          match eqs with
            [] ->
            if k<l then next_criticals (k+1,l) else
              if l<n then next_criticals (1,l+1) else
                (match failures with
                   [] -> rules (* successful completion *)
                 | _  -> (message "nnon-orientable equations :";
                          app non_orientable failures;
                          failwith "kb_completion"))
          | ((mm,nn)::eqs) ->
              let mm' = normal_form mm in
              let nn' = normal_form nn in
              let enter_rule(left,right) =
                let new_rule = (n+1, mk_rule left right) in
                (pretty_rule new_rule;
                 let rec left_reducible (_,(_,(l,_))) = reducible left l in
                 let (redl,irredl) = partition left_reducible rules
                 in (app deletion_message redl;
                     let rec right_reduce (m,(_,(l,r))) =
                       (m,mk_rule l (mrewrite_all (new_rule::rules) r))
                     in
                     let irreds = map right_reduce irredl in
                     let eqs' = map (fun (_,(_,pair)) -> pair) redl
                     in kbrec (n+1) (new_rule::irreds) [] (k,l)
                              (eqs @ eqs' @ failures)
                    )
                )
              in if mm'=nn' then processkl eqs else
                   if greater(mm',nn') then enter_rule(mm',nn') else
                     if greater(nn',mm') then enter_rule(nn',mm') else
                       process ((mm',nn')::failures) (k,l) eqs
        in processkl
      and next_criticals (k,l) =
        (try
            let (v,el) = get_rule l in
            if k=l then
              processf (k,l) (strict_critical_pairs el (rename v el))
            else
              (try
                  let (_,ek) = get_rule k in
                  processf (k,l) (mutual_critical_pairs el (rename v ek))
                with
                  Failure "find" (*rule k deleted*) ->
                  next_criticals (k+1,l))
          with
            Failure "find" (*rule l deleted*) ->
            next_criticals (1,l+1))
      in processf
    in process
  in kbrec

let kb_complete greater complete_rules rules =
    let  n = check_rules complete_rules in
    let eqs = map (fun (_,(_,pair)) -> pair) rules in
    let completed_rules =
               kb_completion greater n complete_rules [] (n,n) eqs
    in (message "Canonical set found :";
        pretty_rules (rev completed_rules);
        ())

let group_rules = [
  (1, (1, (Term("*", [Term("U",[]); Var 1]), Var 1)));
  (2, (1, (Term("*", [Term("I",[Var 1]); Var 1]), Term("U",[]))));
  (3, (3, (Term("*", [Term("*", [Var 1; Var 2]); Var 3]),
           Term("*", [Var 1; Term("*", [Var 2; Var 3])]))))]

let geom_rules = [
 (1,(1,(Term ("*",[(Term ("U",[])); (Var 1)]),(Var 1))));
 (2,(1,(Term ("*",[(Term ("I",[(Var 1)])); (Var 1)]),(Term ("U",[])))));
 (3,(3,(Term ("*",[(Term ("*",[(Var 1); (Var 2)])); (Var 3)]),
  (Term ("*",[(Var 1); (Term ("*",[(Var 2); (Var 3)]))])))));
 (4,(0,(Term ("*",[(Term ("A",[])); (Term ("B",[]))]),
  (Term ("*",[(Term ("B",[])); (Term ("A",[]))])))));
 (5,(0,(Term ("*",[(Term ("C",[])); (Term ("C",[]))]),(Term ("U",[])))));
 (6,(0,
  (Term
   ("*",
    [(Term ("C",[]));
     (Term ("*",[(Term ("A",[])); (Term ("I",[(Term ("C",[]))]))]))]),
  (Term ("I",[(Term ("A",[]))])))));
 (7,(0,
  (Term
   ("*",
    [(Term ("C",[]));
     (Term ("*",[(Term ("B",[])); (Term ("I",[(Term ("C",[]))]))]))]),
  (Term ("B",[])))))
]

let group_rank s =
  match s with
    "U" -> 0
  | "*" -> 1
  | "I" -> 2
  | "B" -> 3
  | "C" -> 4
  | "A" -> 5

let group_precedence op1 op2 =
  let r1 = group_rank op1 in
  let r2 = group_rank op2
  in
    if r1 = r2 then Equal else
    if r1 > r2 then Greater else NotGE


let group_order = rpo group_precedence lex_ext

let greater pair = (match group_order pair with Greater -> true | _ -> false)

let doit() = kb_complete greater [] geom_rules
