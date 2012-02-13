
open Utilities;;
open Grammar;;
open Automaton;;
open Typing;;

exception GiveUp
exception InvalidFD

let loop_count = ref 500;;
let trials_limit = ref 40;;

let depthfirst = ref true
let depth2 = ref false
let breadth2 = ref false
let pruning = ref true
let mixed = ref false
let safemode = ref false
let recmode = ref true

(*** type polarity = POS | NEG
 *** let neg pol =
 ***  match pol with
 ***    POS -> NEG
 *** | NEG -> POS
 ***)

type flag = bool
type tinfo = (ety list * flag) ref
and ety = ETfun of tinfo * ety | ETbase of state | ETindirect of tinfo |ETfixed of aty
         | ETpair of tinfo * tinfo

let singleton_etype_of_value n = ETbase(string_of_int n)

type direction = int
type trace = (nameT * direction) list
let empty_trace = []
let dummy_trace1 = [("this_is_a_wrong_trace1", 0)]
let dummy_trace2 = [("this_is_a_wrong_trace2", 0)]
let dummy_trace3 = [("this_is_a_wrong_trace3", 0)]
(*** The following exception means that
 *** the recursion scheme does not satisfy the property.
 ***)
exception Error of trace

let rec print_trace_aux trace =
  match trace with
    [] -> (print_string ".\n")
  | (n,d)::trace' ->
       (print_string ("("^n^","^(string_of_int(d))^")"); print_trace_aux trace')
let rec print_trace trace = print_trace_aux (List.rev_append trace [])

type visited = ((nameNT * state * int ) list) * ((nameNT * state * int) list)

let empty_visited = ([], [])

let add_to_visited (f,q,i) visited =
  let (vl1,vl2) = visited in
    if List.mem (f,q,i) vl2 then
       visited
    else if List.mem (f,q,i) vl1 then
       (vl1, (f,q,i)::vl2)
    else
       ((f,q,i)::vl1, vl2)

let visited_twice (n,q) visited =
  List.exists (fun (n1,q1,i)-> n=n1 && q=q1) (snd visited)

type id = int

type eterm = EheadNT of nameNT * tinfo * visited * id
          | EheadT of nameT * tinfo * id
          | EheadC of int * tinfo * id
          | EheadFD of int * tinfo * id
          | Epair of tinfo * id
          | Edpair of tinfo * id
          | Eapp of eterm * eterm * tinfo * id

type redex = eterm * ety * trace

let tidtab = Hashtbl.create 100
let ntidtab = Hashtbl.create 100
let appidtab = Hashtbl.create 10000

let id_pair = 0
let id_dpair = 1
let current_termid = ref 2
let id_case n = -2-2*n
let id_fd n = -1-2*n
let new_termid() = 
  let id = !current_termid in
    (current_termid := id+1; id)

let rec size_of_eterm eterm =
  match eterm with
    EheadNT(f,_, _,_) -> 1
  | EheadT(a,_,_) -> 1
  | EheadC(n,_,_) -> 1
  | EheadFD(n,_,_) -> 1
  | Epair _ -> 1
  | Edpair _ -> 1
  | Eapp(et1,et2,_,_) ->
      (size_of_eterm et1)+(size_of_eterm et2)+1

let rec depth_of_eterm eterm =
  match eterm with
    EheadNT(f,_,_,_) -> 0
  | EheadT(a,_,_) -> 0
  | EheadC(_,_,_) -> 0
  | EheadFD(_,_,_) -> 0
  | Epair _ -> 0
  | Edpair _ -> 0
  | Eapp(et1,et2,_,_) ->
      max (size_of_eterm et1) ((size_of_eterm et2)+1)

let rec print_eterm eterm =
  match eterm with
    EheadNT(f,_,_,_) -> print_string f
  | EheadT(a,_,_) -> print_string a
  | EheadC(n,_,_) -> print_string ("_case"^(string_of_int n))
  | EheadFD(n,_,_) -> print_string ("_"^(string_of_int n))
  | Epair _ -> print_string "cons"
  | Edpair _ -> print_string "dcons"
  | Eapp(et1,et2,_,_) ->
      (print_string "(";
       print_eterm et1;
       print_string " ";
       print_eterm et2;
       print_string ")")

(** This is obsolete; 
 ** It should be modified to avoid the exponential explosion of the term size
 ** (when eterm has sharing structurs).
 **)
(**
let rec eterm2term eterm =
  match eterm with
    EheadNT(f,_,_) -> App(NT(f), [])
  | EheadT(a,_) -> App(T(a), [])
  | EheadC(a,_) -> App(CASE(a), [])
  | EheadFD(a,_) -> App(FD(a), [])
  | Epair _ -> App(PAIR, [])
  | Edpair _ -> App(DPAIR, [])
  | Eapp(et1,et2,_) ->
      let App(h, tl) = eterm2term et1 in
      let t2 = eterm2term et2 in
        App(h, tl@[t2])
**)

let tinfomap = ref []

let empty_tinfo:tinfo = ref ([],false)
let new_tinfo() =
  ref ([], false)
exception UnexpectedTinfo of tinfo

let get_tinfo eterm =
  match eterm with
    EheadNT(_, tinfo,_,_) -> tinfo
  | EheadT(_, tinfo,_) -> tinfo
  | EheadC(_, tinfo,_) -> tinfo
  | EheadFD(_, tinfo,_) -> tinfo
  | Epair(tinfo,_) -> tinfo
  | Edpair(tinfo,_) -> tinfo
  | Eapp(_,_,tinfo,_) -> tinfo
let add_to_tinfo newinfo tinfo =
  let (tys,flag) = !tinfo in
    match newinfo with
       ETbase(q) -> 
            tinfo := (merge_and_unify compare [newinfo] tys, flag)
     | _ -> tinfo := (newinfo::tys, flag)
   (*** The following is wrong, as "compare" may return 0 for different pointers 
    *** merge_and_unify compare [newinfo] !tinfo 
    ***)

let sharing = ref true
module X = struct type t = eterm;; let equal = (==);; let hash = Hashtbl.hash end;;
module EtermTab = Hashtbl.Make(X)
module Y = struct type t = eterm * eterm;; 
                 let equal (t1,t2) (t1', t2') = t1==t1' && t2 == t2';; 
                 let hash = Hashtbl.hash end;;
module Z = struct type t = tinfo;; let equal = (==);; let hash = Hashtbl.hash end;;
module TinfoTab = Hashtbl.Make(Z)
let titable_add = TinfoTab.add
let titable_find = TinfoTab.find
let titable_create = TinfoTab.create
(*** module AppEtermTab = Hashtbl.Make(Y) **)
(** let appEtermTab = AppEtermTab.create 10000;;**)

let id_of_eterm eterm =
  match eterm with
    EheadNT(_,_,_,id) -> id
  | EheadT(_,_,id) -> id
  | EheadC(_,_,id) -> id
  | EheadFD(_,_,id) -> id
  | Epair(_,id) -> id
  | Edpair(_,id) -> id
  | Eapp(_,_,_,id) -> id

let id_terminal s =
  try
     Hashtbl.find tidtab s
  with
   Not_found ->
     let id = new_termid() in
       (Hashtbl.add tidtab s id; id)
 

let id_nt s =
  try
     Hashtbl.find ntidtab s
  with
   Not_found ->
     let id = new_termid() in
       (Hashtbl.add ntidtab s id; id)
 
  
let id_app eterm1 eterm2 =
  let id1 = id_of_eterm eterm1 in
  let id2 = id_of_eterm eterm2 in
    try
        Hashtbl.find appidtab (id1,id2)
    with
       Not_found ->
        let id = new_termid() in
          (Hashtbl.add appidtab (id1,id2) id; id)

let lookup_appEterm eterm1 eterm2 =
(** The following code (to share eterms) is obsolete **
  try 
     AppEtermTab.find appEtermTab (eterm1,eterm2)
  with Not_found -> 
**)
    let r = new_tinfo() in
    let id = id_app eterm1 eterm2 in
    let eterm = Eapp(eterm1, eterm2,r,id) in
(**    let _ = AppEtermTab.add appEtermTab (eterm1,eterm2) eterm in **)
    let tinfo_arg = get_tinfo eterm2 in
    let ety_fun = ETfun(tinfo_arg, ETindirect(r)) in
    let tinfo_fun = get_tinfo eterm1 in
    let _ = add_to_tinfo ety_fun tinfo_fun in
      eterm

let constEtermTab = Hashtbl.create 100;;
let lookup_constEterm h =
(**  try 
    if !sharing then
        Hashtbl.find constEtermTab h
    else raise Not_found
  with Not_found -> 
**)
   let eterm =
       match h with
           T(a) ->
            EheadT(a, new_tinfo(), id_terminal a)
        | CASE(n) ->
            EheadC(n, new_tinfo(), id_case n)
        | FD(n) ->
            EheadFD(n, new_tinfo(), id_fd n)
        | PAIR ->
            Epair(new_tinfo(), id_pair)
        | DPAIR ->
            Edpair(new_tinfo(),id_dpair)
        | _ -> raise (Fatal "lookup_constEterm")
   in
   let _ = Hashtbl.add constEtermTab h eterm in
      eterm
    
(** let processed = EtermTab.create 10000;; **)
let processed = Hashtbl.create 10000;; 
let is_processed eterm ety =
  let id = id_of_eterm eterm in
  try
    let eterm1 = Hashtbl.find processed (id, ety) in
(**    let _ = print_string "\n repetition found\n Linking \n"  in
    let _ = print_eterm eterm1 in
    let _ = print_string "\n" in
**)
      Some(eterm1)
  with
   Not_found -> 
       (Hashtbl.add processed (id,ety) eterm;
        None)
       
let register_typeinfo f (tinfo: tinfo) =
  let r = 
        try List.assoc f !tinfomap 
        with
          Not_found -> raise (Grammar.UndefinedNonterminal f)
  in
  let (tys, atys) = !r in
    r := (tinfo::tys, atys)


let initial_redex g q0 =
  let _ = (tinfomap := List.map (fun (f,_) -> (f, ref ([], ([]: aty list)))) g.nt) in
  let tinfo = ref ([ETbase q0],false) in
(**  let s = g.s in **)
(**  let _ = register_typeinfo s tinfo in **)
    (EheadNT(g.s, tinfo, empty_visited, id_nt g.s), ETbase q0, [])

let rec head_of_eterm eterm =
  match eterm with
    EheadNT _ -> eterm
  | EheadT _ -> eterm
  | EheadC _ -> eterm
  | EheadFD _ -> eterm
  | Epair _ -> eterm
  | Edpair _ -> eterm
  | Eapp(eterm1,_,_,_) ->
      head_of_eterm eterm1

let rec decompose_redex eterm =
  match eterm with
    EheadNT _ -> (eterm, [])
  | EheadT _ -> (eterm, [])
  | EheadC _ -> (eterm, [])
  | EheadFD _ -> (eterm, [])
  | Epair _ ->  (eterm, [])
  | Edpair _ ->  (eterm, [])
  | Eapp(eterm1,eterm2,_,_) ->
      let (f, eterms) = decompose_redex eterm1 in
        (f, eterms@[eterm2])

let rec fdlist2string fdlist =
  match fdlist with
     [] -> ""
   | None::fdlist' -> fdlist2string fdlist'
   | Some(n)::fdlist' ->
        "_"^(string_of_int n)^(fdlist2string fdlist')

let rec fdlist_of_eterms eterms =
  let fdlist = List.map
                 (fun eterm-> 
                    match eterm with EheadFD(n,_,_) ->Some(n)
                                   | _ -> None)
                  eterms
  in
     fdlist2string (fdlist)



let type_of_topterm eterm =
  let tinfo = get_tinfo eterm in
    match !tinfo with
      ([q],_) -> q
    | _ -> raise (UnexpectedTinfo tinfo)

let atype_of_eterm eterm =
  match type_of_topterm eterm with
    ETbase(q) -> ITbase(q)
  | ETfixed(aty) -> aty
  | _ -> raise (Fatal "atype_of_eterm")

let ety2aty ety = 
  match ety with
    ETbase(q) -> ITbase(q)
  | ETfixed(aty) -> aty
  | _ -> raise (Fatal "ety2aty")


let rec mk_app_eterm head eterms =
  match eterms with
    [] -> head
  | t::eterms' ->
      let head' = 
            lookup_appEterm head t in
(**         if !sharing then
            lookup_appEterm head t 
           else Eapp(head, t, new_tinfo()) in 
**)
        mk_app_eterm head' eterms';;

let shallow_copy_term eterm =
 let r = new_tinfo() in
  match eterm with
     EheadNT(f,tinfo,visited,id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
         EheadNT(f, r, visited, id)
   | EheadT(a,tinfo,id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        EheadT(a, r, id)
   | EheadC(a,tinfo,id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        EheadC(a, r, id)
   | EheadFD(a,tinfo,id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        EheadFD(a, r, id)
   | Epair(tinfo,id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        Epair(r, id)
   | Edpair(tinfo, id)->
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        Edpair(r, id)
   | Eapp(t1,t2,tinfo, id) -> 
       let _ = add_to_tinfo (ETindirect(r)) tinfo in
        Eapp(t1,t2,r, id)


 (** to do: implement deep copy **)
let deep_copy_term = shallow_copy_term

let copy_term t =
  if !sharing then
     shallow_copy_term t 
  else
     deep_copy_term t

let is_linear_var v =
   not(String.sub v 0 1 = "!") 

let rec subst_eterm_aux visited subst term f q index =
  match term with
    App(h, terms) ->
     let (head, index') =
        match h with
          NT(f1) -> 
            let tinfo = new_tinfo() in
(**            let _ = register_typeinfo f1 tinfo in **)
            let index1 = if !recmode then index+1 else index in
            let id = id_nt f1 in
              (EheadNT(f1, tinfo,add_to_visited (f,q,index) visited, id), index1)
        | Var(v) ->
            if is_linear_var(v) then
                (List.assoc v subst, index)
            else
               let t = List.assoc v subst in
               let t' = copy_term t  in
                  (t', index)
        | _ ->
            (lookup_constEterm h, index)
     in
     let (eterms, index'') = subst_eterms visited subst terms f q index' in
         (mk_app_eterm head eterms, index'')
and subst_eterms visited subst terms f q index =
  match terms with
    [] -> ([], index)
  | term::terms' ->
      let (term1,index1) = subst_eterm_aux visited subst term f q index in
      let (terms1, index2) = subst_eterms visited subst terms' f q index1 in
        (term1::terms1, index2)
and subst_eterm visited subst term f q =
  let (eterm,_)=subst_eterm_aux visited subst term f q 1 in
     eterm

let replace_tinfo tinfo eterm =
  match eterm with
     EheadNT(f,_,visited,id)->EheadNT(f, tinfo,visited,id)
   | EheadT(a,_,id)->EheadT(a, tinfo,id)
   | EheadC(a,_,id)->EheadC(a, tinfo,id)
   | EheadFD(a,_,id)->EheadFD(a, tinfo,id)
   | Epair(_,id)->Epair(tinfo,id)
   | Edpair(_,id)->Edpair(tinfo,id)
   | Eapp(t1,t2,_,id) -> Eapp(t1,t2,tinfo,id)

(** The following function is used in non-sharing mode.
 ** It allocates a fresh tinfo to keep track of how each argument is used
 **)
let rec process_args eterm =
   match eterm with
     Eapp(t1,t2,tinfo,id) ->
        let r = new_tinfo() in
        let tinfo2 = get_tinfo t2 in
        let _ = add_to_tinfo (ETindirect(r)) tinfo2 in
        let t2' = replace_tinfo r t2 in
        let t1' = process_args t1 in
          Eapp(t1',t2',tinfo,id)
    | EheadNT _ -> eterm
    | EheadT _ -> eterm
    | EheadC _ -> eterm
    | EheadFD _ -> eterm
    | Epair _ -> eterm
    | Edpair _ -> eterm

let refresh_tinfo t =
    let r = new_tinfo() in
    let tinfo = get_tinfo t in
    let _ = add_to_tinfo (ETindirect(r)) tinfo in
         replace_tinfo r t

(** The function process_head makes a link between the types of a function and its arguments **)
(** In sharing mode, it only add the top-level type. **)
let rec process_head_aux eterm ety =
   match eterm with
     Eapp(t1,t2,tinfo,_) ->
        let _ = add_to_tinfo ety tinfo in
        let tinfo_arg = get_tinfo t2 in
        let ety_fun = ETfun(tinfo_arg, ety) in
          process_head_aux t1 ety_fun
    | EheadNT(_,tinfo,_,_) -> add_to_tinfo ety tinfo 
    | EheadT(_,tinfo,_) -> add_to_tinfo ety tinfo 
    | EheadC(_,tinfo,_) -> add_to_tinfo ety tinfo 
    | EheadFD(_,tinfo,_) -> add_to_tinfo ety tinfo 
    | Epair(tinfo,_) -> add_to_tinfo ety tinfo 
    | Edpair(tinfo,_) -> add_to_tinfo ety tinfo 
and process_head eterm ety =
  if !sharing then
     let tinfo = get_tinfo eterm in
        add_to_tinfo ety tinfo
  else
     process_head_aux eterm ety

let mk_link_tinfo tinfo1 tinfo2 =
    if not(tinfo1==tinfo2) then 
      let ety = ETindirect(tinfo2) in
        add_to_tinfo ety tinfo1
    else
       ()

(** TO DO: detect sharing and avoid constructing multiple links **)
let rec mk_link_aux eterm1 eterm2 visited =
 if eterm1 == eterm2 
    || List.exists (fun (x,y) -> x==eterm1 && y == eterm2) visited
 then visited
 else
(** let _ = print_string "Linking \n" in
 let _ = print_eterm eterm1 in
**)
 let visited' = (eterm1,eterm2)::visited in
  match (eterm1, eterm2) with
    (EheadNT(_,tinfo1,_,_), EheadNT(_,tinfo2,_,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (EheadT(_,tinfo1,_), EheadT(_,tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (EheadC(_,tinfo1,_), EheadC(_,tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (EheadFD(_,tinfo1,_), EheadFD(_,tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (Epair(tinfo1,_), Epair(tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (Edpair(tinfo1,_), Edpair(tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2; visited')
  | (Eapp(et11,et12,tinfo1,_), Eapp(et21,et22,tinfo2,_))
        -> (mk_link_tinfo tinfo1 tinfo2;
            let visited1 = mk_link_aux et11 et21 visited' in
            mk_link_aux et12 et22 visited1)
  | _ -> (print_string "Something is wrong in mk_link_aux";
          print_eterm eterm1;
          print_string "\n";
          print_eterm eterm2;
          print_string "\n";
          raise (Fatal "mk_link_aux"))

let mk_link eterm1 eterm2 =
   let _ = mk_link_aux eterm1 eterm2 [] in ()

let rec mk_newredexes etqs traces =
  match (etqs, traces) with
    ([], _) -> []
  | ((eterm,q)::etqs', tr::traces') ->
       let redexes = mk_newredexes etqs' traces' in
       let eterm' = if !sharing then eterm else process_args eterm in 
       let ety = ETbase(q) in
(** extract processed term, and make a link **)
       let processed = if !sharing then is_processed eterm' ety
                       else None
       in
          if processed=None
          then
             let _ =  process_head eterm' ety in
                   (eterm', ety, tr)::redexes
          else
            ( match processed with
               Some(eterm1) ->
                  let _ = mk_link eterm' eterm1 in 
                  let _ = debug "link has been created\n" in
                     redexes
              | _ -> raise (Fatal "mk_newredexes:2")
             )
  | _ -> raise (Fatal "mk_newredexes")
       
let rec reset_callseq eterm x =
  match eterm with
    EheadNT(f,tinfo,visited,id) -> EheadNT(f,tinfo,add_to_visited x empty_visited,id)
  | Eapp(e1,e2,tinfo,id) -> 
       Eapp( (reset_callseq e1 x), e2, tinfo,id)
  | _ -> eterm


exception ArityMismatch of nameNT * int * int;;
exception TArityMismatch of nameT;;

let redcount = ref 0

    

let red_onestep (eterm,ety,trace) g m =
  let _ = if !debugging then
            (print_string "reducing :\n  ";
             print_eterm eterm;
             print_string "\n")
          else ()
  in
  let _ = (redcount := !redcount+1) in
  let (f, eterms) = decompose_redex eterm in
(***  let _ = print_string "reducing a eterm with a trace:\n" in
 ***  let _ = print_trace trace in
 ***)
  let q = match ety with 
            ETbase(q) -> q
          | ETfixed(ITbase q) -> q
          | _ -> raise (Fatal "red_onestep: the redex does not have a tree type")
  in      
   match f with
     EheadNT(f, tinfo, visited, id) ->
     (**  let _ = print_string "\nReducing: " in
       let _ = print_eterm eterm in
      **)
       let _ = register_typeinfo f tinfo in 
       let (vars, body) = Grammar.get_def f g in
       let f' = f^(fdlist_of_eterms eterms) in
     (**  let _ = print_string ("\nCalling "^f'^"\n") in **)
     (**  let visited' = add_to_visited (f',q) visited in **)
       let subst = try 
                     List.combine vars eterms
                   with
                     Invalid_argument _ -> raise (ArityMismatch (f, List.length vars, List.length eterms))
       in
       let eterm1 = subst_eterm visited subst body f' q in
       let eterm2 = if !sharing then eterm1 else process_args eterm1 in
       let processed = if !sharing then is_processed eterm2 (ETbase(q))
                       else None
       in
        if processed=None
        then
         let _ =  process_head eterm2 ety 
         in
           ([(eterm2, ETbase(q), trace)], false)
        else
            ( match processed with
               Some(eterm1) ->
                  let _ = mk_link eterm2 eterm1 in ([], false)
              | _ -> raise (Fatal "mk_newredexes:2")
             )
   | EheadT(a, tinfo, _) ->
      ( match ety with
         (ETbase(q)|ETfixed(ITbase q)) ->
           let qs = try next_state q a m 
                    with Not_found -> raise (Error ((a,0)::trace)) (** the property is not satisified **)
           in
           let etqs = try List.combine eterms qs with
                        Invalid_argument _ -> 
                         raise (TArityMismatch a) in
           let directions = Utilities.fromto 1 ((List.length eterms)+1) in
           let traces = List.map (fun i-> (a,i)::trace) directions in
           let redexes = mk_newredexes etqs traces in
(**             (redexes, (String.length a < 2) || not("br"=(String.sub a 0 2))) **)
             (redexes, true)
       | _ -> raise (Fatal "red_onestep: non-tree type occurs at top level")
      )
   | EheadC(n, _,_) ->
      ( match ety with
         (ETbase(q)|ETfixed(ITbase q)) ->
           let d = 
              try match List.hd eterms with
                     EheadFD(k, tinfo,_) -> 
                         (add_to_tinfo (singleton_etype_of_value k) tinfo;
                          k )
                    | _ -> raise InvalidFD 
              with
                  Failure "hd" -> raise (Fatal "red_onestep: EheadC") 
           in
           let eterm' = List.nth eterms (d+1) in
           let etqs = [(eterm',q)] in
           let traces = [trace] in
           let redexes = mk_newredexes etqs traces in
            (redexes, true)
       | _ -> raise (Fatal "red_onestep: non-tree type occurs at top level")
      )
   | Edpair _ ->
      ( match ety with
         (ETbase(q)|ETfixed(ITbase q)) ->
            (match eterms with
               [p; cont] ->
                 let (f, elems) = decompose_redex(p) in
                 let _ = 
                    match f with
                      Epair(tinfo,_) -> ()
                    | _ -> raise (Fatal "red_onestep: dpair is applied to a non-pair")
                 in
                 let pt_info = get_tinfo p in
                 let (t1, t2) =
                   match elems with
                      [t1;t2] -> (t1,t2)
                    | _ -> raise (Fatal "red_onestep: pair has a wrong number of arguments")
                 in
                 let (t1', t2') = if !sharing then
                                    (t1,t2)
                                  else (refresh_tinfo(t1),refresh_tinfo(t2))
                 in
                 let _ = 
                     if not(!sharing) || (fst(!pt_info)=[]) then
                       add_to_tinfo (ETpair(get_tinfo t1', get_tinfo t2')) pt_info 
                     else
                       ()
                 in
                 let eterm1 = mk_app_eterm cont [t1';t2'] 
(**                    Eapp(Eapp(cont, t1',new_tinfo()),t2',new_tinfo()) **)
                 in
                 let eterm2 = if !sharing then eterm1 else process_args eterm1 in
                 let _ = process_head eterm2 ety in
                    ([(eterm2, ety, trace)], true)
              | _ -> raise (Fatal "red_onestep: 
                                    invalid arguments for dpair")
            )
       | _ -> raise (Fatal "red_onestep: non-tree type occurs at top level")
      )
   | EheadFD(n, _, _) -> raise (Fatal "red_onestep: a value occurs in the head")
   | Epair _ -> raise (Fatal "red_onestep: a pair occurs at top-level")
   | _ -> raise (Fatal "red_onestep: a variable occurs in the head")

let divide_redexes (eterms: redex list) visited =
  let rec divide_redexes_aux eterms visited red1 red2 =
     match eterms with
       [] -> (red1, red2)
     | redex::eterms' ->
        ( let (eterm,ety,trace) = redex in 
          let (f, args) = decompose_redex eterm in
          match f with
          EheadNT(n, tinfo,visited2,_) ->
            let q = match ety with
                       ETbase(q) -> q
                     | ETfixed(ITbase(q)) -> q
                     | _ -> raise (Fatal "divide_redexes")
            in
            let n' = n^(fdlist_of_eterms args) in
(**             let t = raw_eterm eterm in
             (*** Below, we find loops and recursions and give lower priorities to them
              *** For the moment we put loops and recursions in the same queue
              ***)
             if false (*** List.mem (t,q) visited  ***)
             then (*** loop ***)
              ((** for debugging
               print_string "loop found on "; print_eterm t; print_string "\n";flush stdout;
                **)
               divide_redexes_aux eterms' visited red1 ((redex, [])::red2))
             else
**)
             if visited_twice (n',q) visited2
             then (*** recursive call ***)
              (
           (**    print_string "\nRecursive call found on "; 
            **   print_eterm t; print_string "\n";flush stdout;
            **)
                 divide_redexes_aux eterms' visited red1 ((redex, [])::red2))
             else
(**               divide_redexes_aux eterms' visited ((redex, (t,q)::visited)::red1) red2**)
               divide_redexes_aux eterms' visited ((redex, visited)::red1) red2
         | _ -> divide_redexes_aux eterms' visited ((redex, visited)::red1) red2
         )
  in
     divide_redexes_aux eterms visited [] []


let empty_queue = ([], [])
let is_emptyqueue (q1,q2) = q1=[] && q2=[]
let enqueue_redex ((eterm:redex),visited) (q1,q2) =
  let item = ((eterm,visited),0) in
  if !depthfirst
  then  (**  (((eterm,visited),0)::q1,q2)  (** depth first **) **)
(**    match q2 with
       [] -> (q1, [[item]])
     | qtop::q2' ->
           (q1, (item::qtop)::q2')
**) 
    match q1 with
       [] -> ([[item]], q2)
     | qtop::q1' ->
           ((item::qtop)::q1', q2)
  else
     (q1, [item]::q2)  (** breadth first **)

let enqueue_redex_breadth ((eterm:redex),visited) (q1,q2) =
  if !mixed || not(!depthfirst)
  then
    let item = ((eterm,visited),0) in
     (q1, [item]::q2)  (** breadth first **)
  else
    enqueue_redex  (eterm,visited) (q1,q2)

let enqueue_redex_tmp ((eterm:redex),visited) (q1,q2) =
  let item = ((eterm,visited),0) in
     ([item]::q1, q2)

let eterm_of_redex ((eterm,_),_) = eterm

let is_head_terminal eterm =
  let (f,_)=decompose_redex eterm in
   match f with
     EheadT _ -> true
   | _ -> false

let priority_of_eterm eterm =
  0
(**  The following priority does not seem to work well
  if is_head_terminal eterm 
  then 0
  else depth_of_eterm eterm 
**)
(**
  let p = priority_of_eterm eterm in
     List.merge 
       (fun (_,p1) -> fun (_,p2) -> compare p1 p2)
         q  [((eterm,visited),p)]
**)
let rec enqueue_redexes redexes q =
  match redexes with
     [] -> q
   | x::redexes' ->
      enqueue_redexes redexes' (enqueue_redex x q)

let rec enqueue_redexes_tmp redexes q =
  if !depthfirst then 
      let redexes' = if !depth2 then (List.rev_append redexes []) else redexes in
       enqueue_redexes redexes' q
  else 
     let redexes' = List.rev_append redexes [] in
        enqueue_redexes_tmp_aux redexes' q
and enqueue_redexes_tmp_aux redexes q = 
    match redexes with
       [] -> q
     | x::redexes' ->
        enqueue_redexes_tmp_aux redexes' (enqueue_redex_tmp x q)

let rec enqueue_redexes_breadth redexes q =
  match redexes with
     [] -> q
   | x::redexes' ->
      enqueue_redexes_breadth_aux redexes' (enqueue_redex x q)
and enqueue_redexes_breadth_aux redexes q =
  match redexes with
     [] -> q
   | x::redexes' ->
      enqueue_redexes_breadth_aux redexes' (enqueue_redex_breadth x q)

let rec print_queue q =
  match q with
     [] -> ()
   | ((eterm,_),_)::q' ->
       ( print_eterm eterm;
        print_string " : ";
        let q = match type_of_topterm eterm with
                  ETbase(q) -> q
                | ETfixed(ITbase(q)) -> q
                | _ -> raise (Fatal "print_queue") in
        print_string (q^"\n");
        print_queue q')

let print_queues q freezed =
  let _ = print_string "Current queue:\n" in
  let _ = print_queue q in
  let _ = print_string "Freezed:\n" in
    print_queue (List.map (fun x -> (x,0)) freezed)

let print_queues2 (q1,q2) =
  let _ = List.map (fun q -> print_queue q; print_string "\n\n") q1 in
  let _ = List.map (fun q -> print_queue q; print_string "\n\n") q2 in
    ()


let rec less_informative aty1 aty2 =
  match aty1 with
     ITbase(q1) -> 
       (match aty2 with
         ITbase(q2) -> q1=q2
        | _ -> false
       )
   | ITfun(ty1,aty1') ->
       (match aty2 with
         ITbase(_) -> false (*** raise (Fatal "less_informative: incompatible") ***)
       | ITfun(ty2, aty2') ->
           (subset_sortedlist compare ty1 ty2) && (less_informative aty1' aty2')
       | _ -> raise (Fatal "less_informative: ITcase should not occur")
       )
   | _ -> raise (Fatal "less_informative: ITcase should not occur")

let rec subsumed aty1 aty2 =
  match aty1 with
     ITbase(q1) -> 
       (match aty2 with
         ITbase(q2) -> q1=q2
        | _ -> false
       )
   | ITfun(ty1,aty1') ->
       (match aty2 with
         ITbase(_) -> false 
       | ITfun(ty2, aty2') ->
           (subsumed aty1' aty2') && 
           List.for_all (fun aty1 -> List.exists (subsumed aty1) ty2) ty1
       | _ -> raise (Fatal "subsumed: ITcase should not occur")
       )
   | _ -> raise (Fatal "subsumed: ITcase should not occur")
          
(*** Obsolete: this may break the completeness ***)
let rec remove_uninformative ty checked =
  match ty with
   [] -> List.rev_append checked []
  | aty::ty' ->
      if (List.exists (less_informative aty) ty')
         || (List.exists (less_informative aty) checked)
      then remove_uninformative ty' checked
      else remove_uninformative ty' (aty::checked)
  

let rec insert_aty aty ty =
  if List.exists (fun aty' -> Typing.subtype aty' aty) ty
  then
     ty
  else
    merge_and_unify compare [aty] (List.filter (fun aty'->not(Typing.subtype aty aty')) ty)

let rec remove_opentype ty checked =
  match ty with
    [] -> [[]]
  | (aty1,flag1)::ty' ->
      let tylist = remove_opentype ty' (aty1::checked) in
      let _ = if !safemode && List.length tylist >3000 then
               raise GiveUp
              else ()
      in
      if (flag1 && 
         ((List.exists (fun (aty, _) -> subsumed aty1 aty) ty')
          || (List.exists (subsumed aty1) checked)))
      then  (*** open type ***)
        delete_duplication_unsorted
          (List.rev_append
          (List.rev_map (fun ty1 -> insert_aty aty1 ty1) tylist)
           tylist)
      else  
        delete_duplication_unsorted
           (List.rev_map (fun ty1 -> insert_aty aty1 ty1) tylist);;
        
let rec merge_tys tys1 tys2 =
  match (tys1,tys2) with
    ([], _) -> tys2
  | (_, []) -> tys1
  | ((aty1,f1)::tys1', (aty2,f2)::tys2')
       -> let c = compare aty1 aty2 in
          if c=0
          then (aty1,f1&&f2)::(merge_tys tys1' tys2')
          else if c<0 then
               (aty1,f1)::(merge_tys tys1' tys2)
          else
             (aty2,f2)::(merge_tys tys1 tys2')
     
let rec merge_tylist tylist =
  match tylist with
    [] -> []
  | tys1::tylist' ->
       merge_tys tys1 (merge_tylist tylist')

let rec merge_cycles cycles1 cycles2 = 
  match cycles1 with
     [] -> cycles2
   | x::cycles1' ->
       let cycles2' = 
             if List.memq x cycles2 then cycles2 else x::cycles2
       in merge_cycles cycles1' cycles2'
let rec merge_cycleslist cyclelist =
  match cyclelist with
    [] -> []
  | cycles1::cyclelist' ->
     merge_cycles cycles1 (merge_cycleslist cyclelist')

let rec tinfo2ty tinfo h visited =
  try 
     let (tys, flag) = titable_find h tinfo in (tys,flag, [])
  with
    Not_found -> 
      if List.memq tinfo visited then ([], false, [tinfo])
           (*** This part should be modified if we want to support recursive types ***)
      else
        let visited' = tinfo::visited in
        let (etys, flag) = !tinfo in 
(***      let (tys, flag1) = etylist2tys etys h in ***)
        let (tys, flag1, new_etys, cycles) = etylist2tys etys h visited' in 
        let cycles' = List.filter (fun x -> not(x==tinfo)) cycles in
        let _ = if cycles'=[] then (tinfo := (new_etys, flag)) else () in 
        let result = (tys, flag ||flag1, cycles') in
        let _ = if cycles'=[] then titable_add h tinfo (tys, flag ||flag1) else () in
          result
and etylist2tys etys h visited =
  let ty_flag_ety_list = List.rev_map (fun ety -> (ety2ty ety h visited, ety)) etys in
  let open_list = List.filter (fun ((_,b,_),_)->b) ty_flag_ety_list in
  let closed_atys = List.rev_map (fun ((tys,_,_),_)-> tys) 
                     (List.filter (fun ((_,b,_),_)->not(b)) ty_flag_ety_list) in
  let cycles = merge_cycleslist (List.rev_map (fun ((_,_,cycle),_)->cycle) ty_flag_ety_list) in
  let atys1 = merge_tylist closed_atys in
  let new_ety_closed = List.rev_map (fun (aty,_) -> ETfixed(aty)) atys1 in
  let new_ety = new_ety_closed @ (List.rev_map snd open_list) in
  let open_atys = merge_tylist (List.rev_map (fun ((x,_,_),_) -> x) open_list) in
  let atys = merge_tys atys1 open_atys in
    (atys, open_list!=[], new_ety, cycles)
and ety2ty ety h visited =
  match ety with
    ETfun(tinfo,ety1) -> 
      let (ty1,flag1,cycles1) = tinfo2ty tinfo h [] in
           (*** This part should be modified if we want to support recursive types ***)
      let ty1list = 
          (**  if pol=NEG then
                [List.map (fun (at,_)->at) ty1]
            else
           **)
                remove_opentype ty1 [] in
      let (ty2,flag2,cycles2) = ety2ty ety1 h [] in
           (*** This part should be modified if we want to support recursive types ***)
      let tylist =
       List.fold_left (merge_and_unify compare) []
        (List.map (fun ty1' ->
                  (List.sort compare (List.map 
                    (fun (aty, flag3) ->(ITfun(ty1', aty), flag1||flag3))
                  ty2)))
         ty1list
        )
     in
      (tylist, flag1||flag2, [])
 | ETpair(tinfo1,tinfo2) ->
     let (ty1,flag1,cycles1) = tinfo2ty tinfo1 h visited in
     let (ty2,flag2,cycles2) = tinfo2ty tinfo2 h visited in
     let tylist = 
       merge_and_unify compare
        (List.map (fun (aty,flag)->(ITpair1 aty, flag)) ty1)
        (List.map (fun (aty,flag)->(ITpair2 aty, flag)) ty2) in
     let cycles = merge_cycles cycles1 cycles2
     in
      (tylist, flag1||flag2, cycles)
 | ETbase(q) -> ([(ITbase(q), false)], false, [])
 | ETfixed(t) -> ([(t,false)], false, [])
 | ETindirect(tinfo) -> tinfo2ty tinfo h visited;;

let rec merge_atylist atylist =
  match atylist with
    [] -> []
  | [atys] -> atys
  | atys1::atys2::atylist' ->
      let atys = merge_and_unify compare atys1 atys2 in
         merge_atylist (atys::atylist')
 (**      merge_and_unify compare atys (merge_atylist atylist') **)


let rec tinfolist2ty tinfolist h =
  let _ = debug "tinfolist2ty: 1\n" in
  let _ = if !debugging then
             debug ((string_of_int(List.length tinfolist))^"\n")
          else () in
  let tys_flag_tinfo_list = 
       List.rev_map 
         (fun tinfo -> 
           let (ty1,flag,_)=tinfo2ty tinfo h [] in
              ((List.map fst ty1, flag), tinfo)) tinfolist in
  let _ = debug "phase 1\n" in
  let open_list =
       List.filter (fun ((_,b),_) -> b) tys_flag_tinfo_list in
  let _ = debug "phase 2\n" in
  let closed_list =
       List.filter (fun ((_,b),_) -> not(b)) tys_flag_tinfo_list in
  let _ = debug "phase 3\n" in
  let closed_atys = 
        merge_atylist (List.rev_map (fun ((x,_),_)->x) closed_list) in
  let _ = debug "phase 4\n" in
  let open_atys =
        merge_atylist (List.rev_map (fun ((x,_),_)->x) open_list) in
  let _ = debug "phase 5\n" in
  let new_tinfolist =
       List.rev_map snd open_list in
  let _ = debug "phase 6\n" in
  let atys = merge_and_unify compare closed_atys open_atys in
  let _ = debug "tinfolist2ty: 8\n" in
      (atys, new_tinfolist, closed_atys)
       
(***
  match tinfolist with
    [] -> []
  | tinfo::tinfolist' ->
      let (ty1,_) = tinfo2ty tinfo h in
      let ty2 = tinfolist2ty tinfolist' h in
      let ty1' = List.map fst ty1 in
        merge_and_unify compare ty1' ty2
***)
let take_half x =
  let rec tail_n n x =
    if n=0 then x
    else tail_n (n-1) (List.tl x)
  in
  let n = List.length x in
    if n<16 then x
    else let m = n/2 in
             tail_n m x

let tinfomap2telist tinfomap h =
  List.map 
 (fun (f, r) -> 
    let _ = debug ("extracting the type of "^f^"\n") in
    let (tinfolist, atys) = !r in
    let (atys1, new_list, closed_atys) = tinfolist2ty tinfolist h in
    let new_atys = merge_and_unify compare closed_atys atys in
    let _ = (r := (new_list, new_atys)) in
    let _ = debug ("done\n") in
     (f, merge_and_unify compare atys atys1))
  tinfomap

let set_flag_tinfo b tinfo =
  let (ty,_) = !tinfo in
    tinfo := (ty,b)

let rec set_flag_in_eterm b eterm =
  match eterm with
   EheadNT (_,tinfo,_,_) -> set_flag_tinfo b tinfo
 | EheadT (_,tinfo,_) -> set_flag_tinfo b tinfo
 | EheadC (_,tinfo,_) -> set_flag_tinfo b tinfo
 | EheadFD (_,tinfo,_) -> set_flag_tinfo b tinfo
 | Epair (tinfo,_) -> set_flag_tinfo b tinfo
 | Edpair (tinfo,_) -> set_flag_tinfo b tinfo
 | Eapp(e1,e2,tinfo,_) ->
      (set_flag_tinfo b tinfo;
       set_flag_in_eterm b e1; 
       set_flag_in_eterm b e2)

let rec set_flag_in_redex b eterm =
  match eterm with
   EheadNT _ -> ()
 | EheadT _ -> ()
 | EheadC _ -> ()
 | EheadFD _ -> ()
 | Epair _ -> ()
 | Edpair _ -> ()
 | Eapp(e1,e2,tinfo,_) ->
      ( (*** set_flag_tinfo b tinfo; ***)
       set_flag_in_redex b e1; 
       set_flag_in_eterm b e2)
let set_flag_squeue b q =
   List.iter (fun (((eterm,_,_),_),_) -> set_flag_in_redex b eterm) q
let set_flag_queue b (q1,q2) =
  (List.iter (fun sq -> set_flag_squeue b sq) q1;
   List.iter (fun  sq -> set_flag_squeue b sq) q2)
let set_flag_freezed b redexes =
  List.map (fun ((eterm,_,_),_) -> set_flag_in_redex b eterm) redexes


type tyderivation = TDbase of aty 
          | TDapp of aty * tyderivation * tyderivation list 

let aty_of_td td =
  match td with
    TDbase(aty) -> aty
  | TDapp(aty,_,_) -> aty

let rec mk_const_ty m aty =
  if m=0 then aty
  else
    ITfun([], mk_const_ty (m-1) aty)

let rec td_app tdlfun tdlarg =
  let aty = aty_of_td (tdlfun) in
    match aty with
      ITbase(_) -> [] (** This case cannot happen in the simply-typed case **)
    | ITfun(ty, aty') ->
       (try
         let tdl = List.map 
                  (fun aty1 -> 
                    List.find (fun td -> 
                                 subtype (aty_of_td td) aty1) tdlarg)
                   ty
         in
           [TDapp(aty', tdlfun, tdl)]
        with Not_found -> []
       )
    | ITcase(m, n) -> 
        if n=0 then
          let atys = 
              List.map 
                  (fun td ->
                       match td with
                        TDbase(aty) -> aty | _ -> raise (Fatal "td_app")) tdlarg in
          let atys' =
              List.map (fun aty -> ITfun([aty], mk_const_ty (m-1) aty)) atys in
            List.map (fun (aty,td) -> TDapp(aty, tdlfun, [td])) (List.combine atys' tdlarg)
        else
           [TDapp(ITcase(m-1,n-1), tdlfun, [])]
    | ITpair1 _ -> []
    | ITpair2 _ -> []
        
let rec tdlist_app tdlfun tdlarg =
  match tdlfun with
    [] -> []
  | td1::tdlfun' ->
      let apptdl = tdlist_app tdlfun' tdlarg in
          (td_app td1 tdlarg)@apptdl

let mk_tdpair1 td =
  let aty = aty_of_td td in
  let t = ITpair1(aty) in
    TDapp(t,
          TDapp(ITfun([], t), TDbase(ITfun([aty], ITfun([], t))), [td]),
          [])

let mk_tdpair2 td =
  let aty = aty_of_td td in
  let t = ITpair1(aty) in
    TDapp(t,
          TDapp(ITfun([aty], t), TDbase(ITfun([], ITfun([aty], t))), []),
          [td])

let rec tdlist_pair tdl1 tdl2 =
  (List.map mk_tdpair1 tdl1)@(List.map mk_tdpair2 tdl2)

let tdlist_dpair_sub tdl_pair td_cont =
  let aty = aty_of_td td_cont in
    match aty with
      ITfun(ty1, ITfun(ty2, aty1)) ->
        let ty = Typing.mk_pairty ty1 ty2 in
          tdlist_app (tdlist_app [TDbase(ITfun(ty, ITfun([aty], aty1)))] tdl_pair) 
                      [td_cont]
    | _ -> []

let tdlist_dpair tdl1 tdl2 =
  List.fold_left
    (fun tdl -> fun td2 -> (tdlist_dpair_sub tdl1 td2)@tdl) [] tdl2

(*** TO DO: this part should be replaced with "==" ***)
let hash_typing = ref (Hashtbl.create 10);;
let red_init() =  (hash_typing := Hashtbl.create 4096)

(** TO DO: Use a hash table (whose key is the identifier of a term) 
 ** to record typing
 **)
let rec compute_typing_of_eterm eterm te cte =
 let id = id_of_eterm eterm in 
 try
    Hashtbl.find !hash_typing id
 with Not_found ->
  let tdl =
   match eterm with
    EheadNT(f,_,_,_) -> 
     let atys = lookup_te f te in
       List.map (fun aty -> TDbase aty) atys
  | EheadT(a,_,_) ->
     let atys = List.assoc a cte in
       List.map (fun aty -> TDbase aty) atys
  | EheadFD(n,_,_) -> [TDbase (singletontype_of_value n)]
  | Eapp(EheadC(m,_,_), et2,_,_) ->
      ( match et2 with
         EheadFD(n,_,_) -> [TDbase(ITcase(m,n))]
       | _ -> raise (Fatal "compute_typing_of_eterm: the argument of case is not an integer")
      )
  | Eapp(Eapp(Epair _, et1,_,_), et2,_,_) ->
      let tdl1 = compute_typing_of_eterm et1 te cte in
      let tdl2 = compute_typing_of_eterm et2 te cte in
        tdlist_pair tdl1 tdl2
  | Eapp(Eapp(Edpair _, et1,_,_), et2,_,_) ->
      let tdl1 = compute_typing_of_eterm et1 te cte in
      let tdl2 = compute_typing_of_eterm et2 te cte in
        tdlist_dpair tdl1 tdl2
  | Eapp(et1,et2,_,_) ->
     let tdl1 = compute_typing_of_eterm et1 te cte in
     let tdl2 = compute_typing_of_eterm et2 te cte in
       tdlist_app tdl1 tdl2
  | EheadC(n,_,_) -> raise (Fatal "compute_typing_of_eterm: case should have an argument")
  | _ -> raise (Fatal "compute_typing_of_eterm: pair or dpair should have an argument")
  in
    (Hashtbl.add !hash_typing id tdl; tdl) 


let rec find_typing_of_eterm eterm ety te cte =
(**  let _ = print_string "finding type derivation\n" in**)
  let aty = ety2aty ety in 
  let tdl = compute_typing_of_eterm eterm te cte in
  let r =
     try
       Some(List.find (fun td -> aty = aty_of_td td) tdl)
     with
       Not_found -> None
  in
(**  let _ = print_string "end\n" in**)
     r

(**
let check_eterm_typing eterm te1 te2 cte =
  let term = eterm2term eterm in
  let aty = atype_of_eterm eterm in
     Typing.has_type term aty te1 te2 [] cte
**)

let rec elim_eterm eterm td =
  let aty = aty_of_td td in
  let tinfo = get_tinfo eterm in
    (add_to_tinfo (ETfixed(aty)) tinfo;
     elim_eterm_top eterm td)
and elim_eterm_top eterm td =
  match eterm with
    EheadNT _ -> ()
  | EheadT _ -> ()
  | EheadC _ -> ()
  | EheadFD _ -> ()
  | Epair _ -> ()
  | Edpair _ -> ()
  | Eapp(EheadC _, et1, _,_) -> 
      (match et1 with
        EheadFD(n,_,_) ->
         elim_eterm et1 (TDbase (singletontype_of_value n))
       | _ -> raise (Fatal "elim_eterm_top: case should take an integer argument")
       )
  | Eapp(et1,et2,_,_) ->
      match td with
        TDapp(_,td1, tdl2) ->
            (elim_eterm et1 td1;
             List.iter (elim_eterm et2) tdl2)
      | _ -> raise (Fatal "elim_eterm_top")
            
let rec dequeue_redex (q1,q2) te cte =
  match q1 with
    [] -> 
       if q2=[] then (None, (q1,q2))
       else let q1' = List.rev_append q2 [] in dequeue_redex(q1', []) te cte
  | []::q1' ->
       dequeue_redex(q1', q2) te cte
  | ((x, p)::qtop)::q' -> 
     let (eterm,ety,trace) = fst x in
       if !pruning then
        let hd = head_of_eterm eterm in 
         match hd with
           EheadNT (f,_,_,_) ->
             let atys = lookup_te f te in
               if atys = [] then (Some x, (q', qtop::q2))
               else
                 (match find_typing_of_eterm eterm ety te cte with
                    Some td ->
                     (elim_eterm_top eterm td;
                      (*** elim_eterm_top eterm td; ***)
               (**       print_string "redex: ";
                      print_eterm eterm;
                      print_string "\n can be eliminated\n";
                **)
                      dequeue_redex (qtop::q',q2) te cte
                      )
                  | None -> 
                     (Some x, (q',qtop::q2))
                 )
         | _ -> (Some x, (q',qtop::q2))
       else
         (Some x, (q',qtop::q2))

let rec dequeue_freezed freezed te cte =
  match freezed with
    [] -> (None, freezed)
  | _ ->
     let (redexes, freezed') = Utilities.list_last_and_rest freezed in
     let (eterm,ety,_) = fst redexes in
       if !pruning then
       match find_typing_of_eterm eterm ety te cte with
         Some td ->
            (elim_eterm_top eterm td;
(***             print_string "dequeue: ";
             print_eterm eterm;
             print_string "\n has been eliminated\n";
***)
             dequeue_freezed freezed' te cte)
       | None -> 
            (
(***             print_string "dequeue: ";
             print_eterm eterm;
             print_string "\n has been dequeued\n";
***)
            (Some(redexes), freezed')
            )
      else (Some(redexes), freezed')
          

let rec red_nsteps n redexes1 freezed  g m te cte =
  if n=0 then 
      let _ = debug "red_nsteps completed\n" in
  (** let _ = print_queues2 redexes1 in **)
 (**  let _ = print_queues redexes1 freezed in **)
     (redexes1, freezed)
  else
    let (v, redexes') =dequeue_redex redexes1 te cte in
    match v with
       None ->
        let _ = debug "red_nsteps: no redex in queue\n" in
        if (List.length(freezed)=0 || n < !loop_count/2) then
        let _ = debug "proceed to type extraction phase\n" in
        (redexes', freezed)
        else
        let _ = debug "extracting a redex from freezed\n" in
         (match dequeue_freezed freezed te cte with
          (None, freezed') -> 
           let _ = debug "nothing left in freezed\n" in
               (redexes1, freezed')
          | (Some(redex), freezed') ->
           let _ = debug "extracted a redex from freezed\n" in
           let q = enqueue_redex redex empty_queue in
            red_nsteps (n-1) q freezed' g m te cte
         )
    |  Some(eterm,visited_nodes) ->
         let (new_eterms, tmp) = red_onestep eterm g m in
         let _ = debug "red_onestep completed\n" in
         let (new_redexes1, new_freezed) = 
             divide_redexes new_eterms visited_nodes in
         let q = if tmp && !breadth2 then
                     (enqueue_redexes_tmp new_redexes1 redexes')
                 else
                    (enqueue_redexes new_redexes1 redexes')
(**              if not(breadth) then
                    (enqueue_redexes_breadth new_redexes1 redexes')
                 else
                    (enqueue_redexes new_redexes1 redexes')
**)
         in
           red_nsteps (n-1) q
              (List.rev_append new_freezed freezed) g m te cte

(*** test ***)
(***
let t = initial_redex g1 "q0";;
let redexes = red_nsteps 10 [(t, [])] [] g1 m1;;
let te = tinfomap2telist !tinfomap;;
let [t1] = red_onestep t g1 m1;;
let [t2] = red_onestep t1 g1 m1;;
let [t3;t4] = red_onestep t2 g1 m1;;
let [t5] = red_onestep t4 g1 m1;;
let [t6;t7] = red_onestep t5 g1 m1;;
let [t8] = red_onestep t6 g1 m1;;
***)

