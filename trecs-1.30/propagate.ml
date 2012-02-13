open Grammar
open Typing
open Utilities

type label = int
type labels = label list
type aterm = ATermVar of ty * labels | ATermNT of nameNT * labels
           | ATermApp of aterm * aterm * labels | ATermCase of int

let debug_propagate = ref false
let rec print_aterm aterm =
  match aterm with
     ATermVar(ty,labels) -> ( print_string "V(";
                            print_ty ty; print_string ")")
   | ATermNT(f,labels) -> print_string f
   | ATermApp(t1,t2,labels) ->
       print_aterm t1;
       print_string " (";
       print_aterm t2;
       print_string ")"
   | ATermCase(n) ->
       print_string ("_case "^(string_of_int n))

let rec decompose_aterm aterm =
  match aterm with
    ATermApp(t1,t2,_) ->
      let (h,aterms) = decompose_aterm t1 in
        (h, aterms@[t2])
  | _ -> (aterm,[])

let labelid = ref 0
let new_label() =
  let l = !labelid in
  let _ = (labelid := !labelid+1) in
    l

let mk_aterm_head s head cte nte =
  match head with
     NT(f) -> if Grammar.is_recfun f then
                  let ty = lookup_te f nte in ATermVar(ty, [])
              else ATermNT(f, [])
   | T(a) -> let ty = List.assoc a cte in ATermVar(ty, [])
   | Var(v) -> List.assoc v s
   | FD(n) -> ATermVar([singletontype_of_value n], [])
   | CASE(n) -> ATermCase(n)
   | _ -> assert false (* PAIR, DPAIR are not supported *)
let rec mk_aterm_subst s term cte nte =
  let App(h,terms) = term in
  let head = mk_aterm_head s h cte nte in
  let aterms = List.map (fun t -> mk_aterm_subst s t cte nte) terms in
    mk_app_aterm head aterms
and mk_app_aterm head aterms =
  match aterms with
     [] -> head
   | aterm::aterms' ->
       mk_app_aterm (ATermApp(head,aterm,[])) aterms'

let rec merge_lte lte1 lte2 =
  match lte1 with
    [] -> lte2
  | (l,ty1)::lte1' ->
     try 
       let (ty2,lte2') = Utilities.list_assoc2 l lte2 in
         (l, merge_ty ty1 ty2)::(merge_lte lte1' lte2')
     with Not_found ->
        (l,ty1)::(merge_lte lte1' lte2)

let rec add_labels aterms =
  match aterms with
     [] -> ([], [])
   | aterm::aterms' ->
      let (aterms1, labels1) = add_labels aterms' in
      let (aterm2,label2) = add_label aterm in
        (aterm2::aterms1, label2::labels1)
and add_label aterm =
  let l = new_label() in
    match aterm with
      ATermVar(r,labels) -> (ATermVar(r,l::labels), l)
    | ATermNT(f,labels) -> (ATermNT(f,l::labels), l)
    | ATermApp(t1,t2,labels) -> (ATermApp(t1,t2,l::labels), l)
    | ATermCase _ -> assert false (* "_case n" cannot be an argument *)

let rec decompose_aty n aty =
  if n=0 then ([], aty)
  else
    match aty with
      ITfun(ty1,aty2) ->
        let (tys,aty3) = decompose_aty (n-1) aty2 in
           (ty1::tys, aty3)
    | _ -> assert false

let rec decompose_aty_for_args aty args =
  let n = List.length args in
  let (tys, aty1) = decompose_aty n aty in
    (List.combine args tys, aty1)

let mk_aterm body vars args aty cte nte =
  let n = (List.length vars)-(List.length args) in
  let (tys, aty1) = decompose_aty n aty in
  let args1 = List.map (fun ty -> ATermVar(ty, [])) tys in
  let args2 = args@args1 in
  let (args2', labels) = add_labels args2 in
  let s = List.combine vars args2' in
  let aterm = mk_aterm_subst s body cte nte in
   (aterm, aty1, labels)

let rec mk_aterm_case_all n aterm aty =
  let argtys = Utilities.mk_list n [aty] in
  let aty1 = Typing.mk_funcaty argtys aty in
    mk_aterm_case_aux aterm aty1 
and mk_aterm_case n i aterm aty =
  let l = Utilities.fromto 0 n in
  let argtys = List.map (fun j -> if j=i then [aty] else []) l in
  let aty1 = Typing.mk_funcaty argtys aty in
    mk_aterm_case_aux aterm aty1 
and mk_aterm_case_aux aterm aty =
  match aterm with
     ATermApp(ATermCase _, _,labels) -> ATermVar([aty], labels)
   | ATermApp(t1,t2,labels) ->
       let t1' = mk_aterm_case_aux t1 aty in
         ATermApp(t1',t2,labels)
   | _ -> assert false

let mk_lte_labels_aty labels aty =
  List.map (fun l -> (l,[aty])) labels

let rec mk_lte aty aterm =
  fst(mk_lte_aux aty aterm)
and mk_lte_aux aty aterm =
  match aterm with
     ATermVar(_,labels) ->
        (mk_lte_labels_aty labels aty, aty)
   | ATermNT(_,labels) -> 
        (mk_lte_labels_aty labels aty, aty)
   | ATermApp(t1,t2, labels) ->
       let (lte1,aty1) = mk_lte_aux aty t1 in
        ( match aty1 with
            ITfun(_,aty2) ->
              let lte2 = mk_lte_labels_aty labels aty2 in
              let lte3 = merge_lte lte1 lte2 in
                (lte3, aty2)
         | _ -> assert false
         )
   | ATermCase _ -> assert false

let rec mk_aty lte labels aty =
  match labels with
    [] -> (aty, lte)
  | l::labels' ->
      let (aty1,lte1) = mk_aty lte labels' aty in
        try 
          let (ty,lte2) = Utilities.list_assoc2 l lte1 in
             (ITfun(ty,aty1), lte2)
        with Not_found ->
           (ITfun([],aty1), lte1)


let register_type f aty nte =
 let _ = if !debug_propagate then
  let _ = print_string ("new type for "^f^" found: ") in
  let _ = print_aty aty in
  let _ = print_string "\n" in
  ()
 in
  try
    let ty = Hashtbl.find nte f in
    let ty' = add_aty_ty aty ty in
      Hashtbl.replace nte f ty'
  with Not_found ->
    Hashtbl.add nte f [aty]

let rec remove_headaty aterm =
  match aterm with
    ATermVar(aty::ty, labels) -> ATermVar(ty,labels)
  | ATermVar([], labels) -> assert false
  | ATermNT _ -> assert false
  | ATermApp(t1,t2,labels) ->
       ATermApp(remove_headaty t1, t2, labels)
  | ATermCase _ -> assert false

let rec find_typing aterm aty cte nte g =
 let _ = if !debug_propagate then
  let _ = print_string "Checking typing:\n" in
  let _ = print_aterm aterm in
  let _ = print_string " : "; print_aty aty in
  let _ = print_string "\n" in ()
 in
  let (head,args) = decompose_aterm aterm in
  match head with
    ATermVar(ty, _) ->
     ( match ty with
        [] -> None
      | aty1::ty' ->
          let (term_tys, aty2) = decompose_aty_for_args aty1 args in
           if subtype aty2 aty then
              match find_typing_aterms term_tys cte nte g with
                Some(lte) -> 
                   let lte1 = mk_lte aty1 aterm in
                   let lte2 = merge_lte lte lte1 in
                      Some(lte2)
              | None -> (
                 let aterm' = remove_headaty aterm in
                    find_typing aterm' aty cte nte g)
           else
               let aterm' = remove_headaty aterm in
                 find_typing aterm' aty cte nte g
       )
  | ATermNT(f, _) ->
     let (vars,body) = List.assoc f g.r in
     let m = List.length args in
     let n = List.length vars in
     let (vars,body) =
        if n>=m then (vars,body)
        else Grammar.add_args vars body (m-n)
     in
     let (aterm1, aty1, label_args) = mk_aterm body vars args aty cte nte in
       ( match find_typing aterm1 aty1 cte nte g with
           Some(lte) -> 
              let (aty2, lte') = mk_aty lte label_args aty1 in
              let _ = register_type f aty2 nte in
              let lte1 = mk_lte aty2 aterm in
              let lte2 = merge_lte lte' lte1 in
                   Some(lte2)
         | None -> None
        )
  | ATermCase(n) -> 
     ( match args with
        [] -> assert false (* _case n should have an additional argument *)
      | fd::args' ->
         (match fd with
           ATermVar(ty, labels) ->
              ( match ty with
                 [] -> 
                   let aterm' = mk_aterm_case_all n aterm aty in
                      find_typing aterm' aty cte nte g
               | aty1::_ ->
                   let i = value_of_singletontype aty1 in
                   let aterm' = mk_aterm_case n i aterm aty in
                   let lte0 = List.map (fun l -> (l, [aty1])) labels in
                     (match find_typing aterm' aty cte nte g with
                        Some(lte1) -> Some(merge_lte lte0 lte1)
                      | None -> None)
               )
          | _ -> assert false
         )
       )
  | ATermApp _ -> assert false

and find_typing_aterms term_tys cte nte g =
  match term_tys with
    [] -> Some([])
  | (term,ty)::term_tys' ->
       match find_typing_ty term ty cte nte g with
        Some(lte1) ->
         ( match find_typing_aterms term_tys' cte nte g with
             Some(lte2) -> Some(merge_lte lte1 lte2)
           | None -> None
          )
     | None -> None

and find_typing_ty term ty cte nte g =
  match ty with
    [] -> Some([])
  | aty::ty' ->
       match find_typing term aty cte nte g with
        Some(lte1) ->
         ( match find_typing_ty term ty' cte nte g with
             Some(lte2) -> Some(merge_lte lte1 lte2)
           | None -> None
          )
     | None -> None

let propagate_aty_rule f vars body aty cte nte g =
(**  let _ = print_string ("propagating "^f^"\n") in **)
  let n = List.length vars in
  let (tys,aty1) = decompose_aty n aty in
  let aterms = List.map (fun ty -> ATermVar(ty, [])) tys in
  let s = List.combine vars aterms in
  let aterm = mk_aterm_subst s body cte nte in
(**  let _ = if f= "F_4400" then debug_propagate := true in **)
  let _ = match find_typing aterm aty1 cte nte g with
        Some _ -> ()
      | None -> (print_string ("Cannot propagate "^f^" : ");
                 print_aty aty;
                 print_string "\n"; 
                 debug_propagate := true;
                 let _ = find_typing aterm aty1 cte nte g in
                 let _ =
                    if Typing.check_aty f aty (init_te g.nt) nte cte g 
                    then print_string "check_aty passed\n"
                    else print_string "check_aty do not pass either\n"
                 in
                 assert false)
   in
      ()
(**    (debug_propagate := false) **)

let propagate_ty_rule f vars body ty cte nte g =
  List.iter 
  (fun aty -> propagate_aty_rule f vars body aty cte nte g)
  ty

let propagate_te te cte nte g =
  List.iter
  (fun (f, ty) ->
    let (vars,body) = List.assoc f g.r in
     propagate_ty_rule f vars body ty cte nte g)
  te
     
let rec sanity_check cte nte dmap g flag =
  let _ = if !debugging then
          if flag then
             print_string "checking consistency of nte for all functions\n" 
          else 
             print_string 
             "checking consistency of nte for recursive functions\n" 
          else ()
  in
  let telist1 = Utilities.hash2list nte in
  let nte1 = Utilities.list2hash telist1 in
  let _ = if flag then (Grammar.allfun := true) in
  let nte2 = compute_te nte1 cte (init_te g.nt) dmap g in
  let _ = if flag then (Grammar.allfun := false) in
  let telist2 = Utilities.hash2list nte2 in
  let telist3 = diff_telist telist1 telist2 in
  let _ =  List.iter
    (fun (f,ty) ->
        if ty!=[] then
           (print_string "type environment is not consistent\n";
           print_string (f^" : ");
           print_ty ty;
           print_string "is wrong\n")
        else
          ()
     ) telist3
  in
   debug "done\n"
