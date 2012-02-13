(** check the sorts of non-terminals, print them and the order of the recursion scheme **)
(** Note: the implementation below is naive and slow **)

open Utilities;;
open Grammar;;
open Automaton;;
open Typing;;

type tvar = int
type st = STvar of tvar | STfd | STbase | STfun of st * st | STpair of st * st

let tvid = ref 0
let new_tvid() =
  let x = !tvid in
   (tvid := x+1; x)

let new_tvar() = STvar(new_tvid())

exception IllSorted of string
exception UnifFailure of st * st

let is_stfun sty =
  match sty with
    STfun _ -> true
  | _ -> false


let subst_id = []

let rec app_subst sub st =
  match st with
     STvar v -> (try List.assoc v sub with Not_found -> st)
   | STfun(st1,st2) -> STfun(app_subst sub st1, app_subst sub st2)
   | STpair(st1,st2) -> STpair(app_subst sub st1, app_subst sub st2)
   | _ -> st

let comp_subst sub1 sub2 =
  let sub1' = List.map (fun (x,st) -> (x, app_subst sub2 st)) sub1 in
  let dom1 = List.map fst sub1 in
  let sub2' = List.filter (fun (x,st) -> not(List.mem x dom1)) sub2 in
    sub1'@sub2'

let rec occur v sty =
  match sty with
    STvar(v1) -> v=v1
  | STfun(sty1,sty2) -> (occur v sty1)||(occur v sty2)
  | STpair(sty1,sty2) -> (occur v sty1)||(occur v sty2)
  | _ -> false

let rec unify_sty sty1 sty2 =
  if sty1=sty2 then
      (sty1, subst_id)
  else
   match (sty1,sty2) with
    (STvar v1, STvar v2) ->
        (sty1, [(v2, sty1)])
  | (STvar v1, sty2) ->
       if occur v1 sty2
       then raise (UnifFailure (sty1,sty2))
       else (sty2, [(v1, sty2)])
  | (_, STvar _) -> unify_sty sty2 sty1
  | (STfun(st11,st12), STfun(st21,st22)) ->
      let (st1, sub1) = unify_sty st11 st21 in
      let st12' = app_subst sub1 st12 in
      let st22' = app_subst sub1 st22 in
      let (st2, sub2) = unify_sty st12' st22' in
        (STfun(st1,st2), comp_subst sub1 sub2)
  | (STpair(st11,st12), STpair(st21,st22)) ->
      let (st1, sub1) = unify_sty st11 st21 in
      let st12' = app_subst sub1 st12 in
      let st22' = app_subst sub1 st22 in
      let (st2, sub2) = unify_sty st12' st22' in
        (STpair(st1,st2), comp_subst sub1 sub2)
  | _ -> raise (UnifFailure (sty1,sty2))
  
let rec unify_all c =
  match c with
     [] -> []
  | (sty1,sty2)::c1 ->
      let (_, sub1) = unify_sty sty1 sty2 in
      let c1' = List.map (fun (sty1,sty2)->(app_subst sub1 sty1, app_subst sub1 sty2)) c1 in
      let sub2 = unify_all c1' in
        comp_subst sub1 sub2

let rec stys2sty stys =
  match stys with
    [] -> new_tvar()
  | [sty] -> sty
  | sty::stys' ->
      let sty1 = stys2sty stys' in
      let (sty2,_)= unify_sty sty sty1 in
         sty2


let rec ty2sty ty =
  let stys = List.map aty2sty ty in
    stys2sty stys
and aty2sty aty =
  match aty with
     ITbase _ -> STbase
   | ITfun(t1,at2) -> STfun(ty2sty t1, aty2sty at2)
   | ITpair1(t1) -> STpair(aty2sty t1, new_tvar())
   | ITpair2(t2) -> STpair(new_tvar(), aty2sty t2)
   | ITcase(m,_) ->
          tcase2sty m
and tcase2sty m =
  if m<=0 then STbase
  else STfun(STbase, tcase2sty(m-1))
                                          
let cte2cste cte =
  let f(c,t) = (c, try ty2sty t with 
                    UnifFailure _ -> raise (IllSorted ("The arity of "^c^" is inconsistent\n"))) in
     List.map f cte

let rec mk_functy stys sty =
  match stys with
    [] -> sty
  | sty1::stys' ->
       STfun(sty1, mk_functy stys' sty)

let rec print_sty sty =
  match sty with
    STvar(tv) -> print_string ("'a"^(string_of_int tv))
  | STfd -> print_string "int"
  | STbase -> print_string "o"
  | STfun(sty1,sty2) ->
       let flag1 = is_stfun sty1 in
         (if flag1 then print_string "(" else ();
          print_sty sty1;
          if flag1 then print_string ")" else ();
          print_string " -> ";
          print_sty sty2)
  | STpair(sty1,sty2) ->
       let flag1 = is_stfun sty1 in
       let flag2 = is_stfun sty2 in
         (if flag1 then print_string "(" else ();
          print_sty sty1;
          if flag1 then print_string ")" else ();
          print_string "*";
          if flag2 then print_string "(" else ();
          print_sty sty2;
          if flag2 then print_string ")" else ())
         
let print_sortbinding (f, sty) =
  (print_string (" "^f^" : ");
   print_sty sty;
   print_string "\n")

let print_ste nste =
  let _ = print_string "\nSorts of non-terminals:\n" in
  let _ = print_string "======================\n" in
  let _ = List.map print_sortbinding nste in
  let _ = print_string "\n" in
    ()


let tcheck_head h vte cte nte =
  match h with
    NT(f) -> (List.assoc f nte, [])
  | T(a) -> (List.assoc a cte, [])
  | Var(v) -> (List.assoc v vte, [])
  | FD(_) -> (STfd, [])
  | CASE(n) ->
       let stys1 = STfd::(mk_list n STbase) in
         (mk_functy stys1 STbase, [])
  | PAIR ->
       let sty1 = new_tvar() in 
       let sty2 = new_tvar() in 
         (STfun(sty1, STfun(sty2, STpair(sty1,sty2))), [])
  | DPAIR ->
       let sty1 = new_tvar() in 
       let sty2 = new_tvar() in 
       let sty3 = STpair(sty1,sty2) in
       let sty4 = STfun(sty1, STfun(sty2, STbase)) in
         (STfun(sty3, STfun(sty4, STbase)), [])
       
 

let rec tcheck_term t vte cte nte =
  match t with
    App(h,terms) ->
       let (stys, c1) = tcheck_terms terms vte cte nte in
       let (sty2, c2) = tcheck_head h vte cte nte in
       let sty3 = new_tvar() in
       let sty1 = mk_functy stys sty3 in
          (sty3, (sty2,sty1)::(c2@c1))

and tcheck_terms terms vte cte nte =
  match terms with
    [] -> ([], [])
  | t1::terms' ->
      let (sty1,c1) = tcheck_term t1 vte cte nte in
      let (stys2,c2) = tcheck_terms terms' vte cte nte in
        (sty1::stys2, c1@c2)

let tcheck_rule (f, (vars, body)) cste nste =
  let vste = List.map (fun v -> (v, new_tvar())) vars in
  let _ = if !debugging then print_string ("Checking the rule for "^f^"\n") in
  let _ = if !debugging then print_ste cste in
  let _ = if !debugging then print_ste nste in
  let _ = if !debugging then print_ste vste in
  let (_,c1) = tcheck_term body vste cste nste in
  let fty1 = mk_functy (List.map snd vste) STbase in
  let fty2 = List.assoc f nste in
    (fty1,fty2)::c1

let tcheck_rules rules cste nste =
  List.fold_left
     (fun c -> fun r -> (tcheck_rule r cste nste)@c) [] rules

let rec order_of_sty sty =
  match sty with
    STfun(sty1,sty2) -> max ((order_of_sty sty1)+1) (order_of_sty sty2)
  | STpair(sty1,sty2) -> max (order_of_sty sty1) (order_of_sty sty2)
  | _ -> 0

let order_of_nste nste =
  let ordmap = List.map (fun (nt, sty) -> (nt, order_of_sty sty)) nste in
  let x = list_max (fun (nt1,ord1) ->fun (nt2,ord2) -> compare ord1 ord2) ordmap in
    x


let print_order (f,ord) =
  let _ = print_string ("\nOrder of recursion scheme: "^(string_of_int ord)^"\n") in
  let _ = print_string ("Non-terminal of highest order: "^f^"\n") in
    ()

let tcheck g cte =
  let cste = cte2cste cte in
  let nste = List.map (fun (n,_) -> (n, new_tvar())) g.nt in
  let _ = if !debugging then print_ste cste in
  let _ = if !debugging then print_ste nste in
  let rules = g.r in
  let c = tcheck_rules rules cste nste in
  let sub =  try
                unify_all c 
             with UnifFailure _ ->
                    raise (IllSorted "Rules are not well-sorted\n")
  in
  let nste' = List.map (fun (nt,sty) -> (nt, app_subst sub sty)) nste in
  let (f,ord) = order_of_nste nste' in
  let _ = print_ste nste' in
  let _ = print_order (f, ord) in
    ()
