open Utilities

exception Fatal of string

type nameNT = string (** names of non-terminal symbols **)
type nameT = string  (** names of terminal symbols **)
type nameV = string
type kind = O | Kfun of kind * kind

type nonterminals = (nameNT * kind) list
type terminals = (nameT * kind) list

type position = int list
type head = NT of nameNT | T of nameT | Var of nameV | FD of int | CASE of int
           | PAIR | DPAIR
type term = App of head * term list 
type eterm = term * (position list)
type rule = nameNT * (nameV list * term)
type rules = rule list

type gram = {nt: nonterminals; t: terminals; r: rules; s: nameNT}

let dmaprec = ref []

let vid = ref 0
let new_var() =
  let n = !vid in
  let _ = (vid := !vid+1) in
    "_v"^(string_of_int n)

let add_args vars body n =
  let vars2 = List.map (fun _->new_var()) (mk_list n ()) in
  let App(h,terms)=body in
  let terms2 = List.map (fun v -> App(Var(v),[])) vars2 in
    (vars@vars2, App(h,terms@terms2))

exception UndefinedNonterminal of nameNT
exception DuplicatedNonterminal of nameNT
exception DuplicatedVars of nameNT * nameV
let allfun = ref false
let recfuntab = Hashtbl.create 1000
let is_recfun f = 
   if !allfun then true
   else Hashtbl.mem recfuntab f
let get_def (f: nameNT) (g:gram) = 
   try
     List.assoc f (g.r)
   with Not_found -> raise (UndefinedNonterminal f)

let rec size_of_term t =
  match t with
      App(h, tl) -> (List.fold_left (fun n -> fun t -> n+(size_of_term t)) 1 tl)

let rec size_of_rule (_,(_,t)) = size_of_term t
      
let rec size_of g =
   List.fold_left (fun n -> fun r -> n+(size_of_rule r)) 0 g.r

let rec occur_nt_in_head nt head =
  match head with
    NT(n) -> n=nt
  | _ -> false
let rec occur_nt_in_term nt term =
  match term with
    App(h, terms) ->
      (occur_nt_in_head nt h) 
      || (List.exists (fun t -> occur_nt_in_term nt t) terms)

let rec mk_depend g =
  let sorted_rules = List.sort (fun (f1,_) -> fun (f2, _) -> compare f1 f2) g.r in
  let nts = List.map fst g.nt in
    List.map (fun nt-> (nt, List.map fst 
                       (List.filter 
                         (fun (f,(_,body)) -> occur_nt_in_term nt body)
                         sorted_rules)))
              nts

let rec closure_f f dmap =
  if is_recfun f then
    [f]
  else merge_and_unify compare [f] (closure_fs (List.assoc f dmap) dmap)
and closure_fs fs dmap =
  match fs with
    [] -> []
 | f::fs' -> merge_and_unify compare (closure_f f dmap) (closure_fs fs' dmap)
let mk_dmaprec dmap =
  let dmap' =
    List.map
     (fun (f, fs) -> 
        if is_recfun f then
         (f, closure_fs fs dmap)
        else (f, [])
     )
    dmap
  in dmaprec := dmap'

let print_dmap dmap =
  print_string "Dependencies:\n  ";
  List.iter (fun (f,l) ->
      print_string ("("^f^",[");
      List.iter (fun g-> print_string (g^" ")) l;
      print_string "])   ")
  dmap;
  print_string "\n"

let find_dep x dmap =
  try
    List.assoc x dmap
  with Not_found ->
     raise (UndefinedNonterminal x)
   
(* find and register recursive functions *)
(* (f, [g1,...,gk]) in dmap means that f occurs in g1,...,gk *)
let register_recfun dmap g =
   Hashtbl.add recfuntab (g.s) ();
   let nt = List.map fst g.nt in
   let graph = List.flatten 
               (List.map (fun (f,l) -> List.map (fun g->(g,f)) l) dmap)
   in
   let scc = Scc.compute_scc graph in
   let singletons = List.map List.hd
                    (List.filter list_singleton scc) in
   let rec1 = (* find self-recursive non-terminals *)
               List.filter 
               (fun x -> List.mem x (find_dep x dmap)) singletons 
   in
   let rec2 = List.filter (fun x -> not(List.mem x singletons)) nt in
   let rec2' = List.filter
             (fun f -> List.length (find_dep f dmap)>1)
              rec2
   in
     (
      List.iter (fun f -> Hashtbl.add recfuntab f ()) rec1;
      List.iter (fun f -> Hashtbl.add recfuntab f ()) rec2';
      if !debugging then
        (print_string "Recursive functions\n";
         let l = hash2list recfuntab in
          List.iter (fun (f,_)-> print_string (f^" ")) l;
          print_string "\n")
      )

let get_initial_term (g: gram) =
   App(NT(g.s), [])

let get_initial_eterm (g: gram) =
   (get_initial_term g, [[]])

       
let rec app_subst s t =
  match t with
    App(h, tl) ->
      (match h with
         Var(x) ->
          (try
            let App(h',tl1) = List.assoc x s in
            let tl2 = app_subst_tl s tl in
              App(h', tl1@tl2)
          with 
            Not_found -> App(h, app_subst_tl s tl)
          )
      | _ -> App(h, app_subst_tl s tl)
      )
and app_subst_tl s tl =
  List.map (app_subst s) tl

let rec print_vars ss =
  match ss with
     [] -> ()
    | s::ss' -> (print_string (s^" "); print_vars ss')

let rec print_term term =
  match term with
    App(head, terms) ->
      (print_string "(";
      print_head head;
      print_string " ";
      print_terms terms;
       print_string ")")
and print_head head =
  match head with
     NT(n) -> print_string n
   | T(s) -> print_string s
   | Var(s) -> print_string s
   | FD(n) -> print_string(string_of_int n)
   | CASE(n) -> print_string("_case "^(string_of_int n))
   | PAIR -> print_string "cons"
   | DPAIR -> print_string "dcons"
and print_terms terms =
  match terms with
    [] -> ()
  | term::terms' ->
      (print_term term;
       print_string " ";
       print_terms terms')

let print_rule (f,(ss,term)) =
  (print_string (f^" ");
  print_vars ss;
  print_string ("-> ");
  print_term term;
  print_string (".\n"))


let print_rules rules =
  let _ = List.map print_rule rules in ()

(*****
let rec get_redex_positions t =
  match t with
    App(h, tl) -> 
     (
      match h with
        NT(f) -> [[]]
      | T(a) -> 
          let positions_list = 
                List.map get_redex_positions tl in
          let args = fromto 0 (List.length tl) in
          let l = List.combine args positions_list in
          let positions_list' =
                List.map 
                (fun (h, positions) -> 
                   List.map (fun p -> h::p) positions)
                l
          in
            List.flatten positions_list'
      | CASE(n) -> [[]]
      | FD(n) -> []
      | Var(x) -> raise (Fatal "get_redex_positions: free variables")
      )

let reduce_redex (f, tl) (g: gram) =
  let (vars, body) = get_def f g in
  let subs = List.combine vars tl in
  let body' = app_subst subs body in
    body'

let rec reduce t position (g: gram) =
  match t with
    App(h, tl) -> 
     (
      match h with
        NT(f) -> if position=[] then 
                    let t' = reduce_redex(f, tl) g in
                    let new_redexes = get_redex_positions t' in
                       (t', new_redexes)
                 else
                    raise (Fatal "reduce: wrong_redex_position")
      | CASE(n) -> 
      | T(a) -> 
         ( match position with
             [] -> raise (Fatal "reduce: wrong_redex_position")
           | i::p' ->
               let ti = List.nth tl i in
               let (ti', new_redexes) = reduce ti p' g in
               let tl' = list_repl i ti' tl in
               let new_redexes' = 
                 List.map (fun p-> i::p) new_redexes in
                 (App(h, tl'), new_redexes')
          )
      | Var(x) -> raise (Fatal "reduce: free variables")
      )
  
exception Irreducible;;

let _ = Random.self_init()

let rewrite_random (t, positions) (g: gram) =
  let n = List.length positions in
  if n>0 then
     let i = Random.int(n) in
     let (p, positions') =  list_take_nth positions i in
     let (t', new_positions) = reduce t p g in
       (t', positions'@new_positions)
  else
    raise Irreducible

let rewrite_fair (t, positions) (g: gram) =
  match positions with
    [] -> raise Irreducible
  | p::pp -> 
     let (t', new_positions) = reduce t p g in
       (t', pp@new_positions)

let rewrite_pr prcomp (t, positions) (g: gram) =
  match positions with
    [] -> raise Irreducible
  | p::pp -> 
     let (t', new_positions) = reduce t p g in
     let new_pp = 
           List.sort prcomp new_positions 
     in
     let pp'= merge prcomp pp new_pp
     in
        (t', pp')

let rewrite_bf = rewrite_pr (fun p -> fun q -> compare (List.length p) (List.length q))

let rewrite_df = rewrite_pr compare
***)
     
(** Example 1 from LICS2009 submission **)
let nt1 = [("S", O); ("F", Kfun(O, O))]
let t1 = [("a", Kfun(O, Kfun(O, O))); ("b", Kfun(O, O)); ("c", O)]
let c = App(T("c"), [])
let x = App(Var("x"), [])
let y = App(Var("y"), [])
let r1 = [("S", ([], App(NT("F"), [c]))); 
          ("F", (["x"], App(T("a"), [x; App(NT("F"), [App(T("b"), [x])])])))]
let g1 = {nt=nt1; t=t1; r=r1; s="S"}
let s = App(NT("S"), []);;

(** Example 2 **)
let r2 = [("S", ([], App(NT("F"), [App(NT("F"), [c])]))); 
          ("F", (["x"], App(T("a"), [x; App(NT("F"), [App(T("b"), [x])])])))]
let g2 = {nt=nt1; t=t1; r=r2; s="S"}
let s = App(NT("S"), []);;

(** Example 3: order-2 recursion scheme **)
(** S -> F I,  I x -> x, F x -> a (x c) (F (H x)), H x y -> x (b y) **)
let nt3 = [("S", O); ("I", Kfun(O, O)); ("F", Kfun(Kfun(O, O), O)); 
           ("H", Kfun(Kfun(O,O), Kfun(O,O)))]
let r3 = [("S", ([], App(NT("F"), [App(NT("I"), [])])));
          ("I", (["x"], x));
          ("F", (["x"], App(T("a"), [App(Var("x"), [App(T("c"), [])]); 
                             App(NT("F"), [App(NT("H"), [x])])])));
          ("H", (["x"; "y"], App(Var("x"), [App(T("b"), [y])])))];;
let g3 = {nt=nt3; t=t1; r=r3; s="S"};;

(** Example 4: order-3 recursion scheme, taken from resource usage analysis **)
(** S -> br (new (F I d)) (F K d),  F r k -> br (r close k) (r read (F r k)),
    I x y -> x y, K x y -> y
 **)
let i = App(NT("I"), [])
let k = App(NT("K"), [])
let d = App(T("d"), [])
let c = App(T("close"), [])
let b = App(T("read"), [])
let y = App(Var("y"), [])
let r = App(Var("r"), [])
let vk = App(Var("k"), [])
let t4 = [("br", Kfun(O, Kfun(O, O))); ("read", Kfun(O, O));("close", Kfun(O, O)); ("d", O); 
          ("new", Kfun(O,O))]
let nt4 =[("S", O); ("I", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("K", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("F", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(O,O)))]

let r4 = [("S", ([], App(T("br"), [App(T("new"), [App (NT("F"), [i; d])]); 
                                  App(NT("F"), [k;d])])));
          ("F", (["r"; "k"], App(T("br"), [App(Var("r"), [c; vk]); 
                                          App(Var("r"), [b; App(NT("F"), [r; vk])])])));

          ("I", (["x"; "y"], App(Var("x"), [y])));
          ("K", (["x"; "y"], y))]
let g4 = {nt=nt4; t=t4; r=r4; s="S"}

(*** Example 5:
 *** S -> Newr C1
 *** C1 x -> Neww (C2 x)
 *** C2 x y -> F x y d
 *** F x y k -> br (Close x (Close y k)) (Read x (Write y (F x y k)))
 *** Newr k -> br (ir (k I)) (k K)
 *** Neww k -> br (iw (k I)) (k K)
 *** Close x k -> x close k
 *** Read x k -> x read k
 *** Write y k -> y write k
 ***)
let x = App(Var("x"), [])
let y = App(Var("y"), [])
let k = App(Var("k"), [])
let c = App(T("close"), [])
let r = App(T("read"), [])
let w = App(T("write"), [])
let t5 = [("br", Kfun(O, Kfun(O, O))); 
          ("read", Kfun(O, O));
          ("write", Kfun(O, O));
          ("close", Kfun(O, O)); 
          ("d", O); 
          ("newr", Kfun(O,O));
          ("neww", Kfun(O,O))]
let resty = Kfun(Kfun(O,O), Kfun(O,O))
let nt5 =[("S", O); 
          ("C1", Kfun(Kfun(resty,O),O));
          ("C2", Kfun(resty, Kfun(Kfun(resty,O),O)));
          ("Newr", Kfun(Kfun(resty,O), O));
          ("Neww", Kfun(Kfun(resty,O), O));
          ("Close", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("Read", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("Write", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("I", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("K", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("F", Kfun(resty, Kfun(resty, Kfun(Kfun(O,O), O))))
         ]
let r5 =
   [("S", ([], App(NT("Newr"), [App(NT("C1"), [])])));
    ("C1", (["x"], App(NT("Neww"), [App(NT("C2"), [App(Var("x"), [])])])));
    ("C2", (["x";"y"], 
       App(NT("F"), [App(Var("x"), []);App(Var("y"), []);App(T("d"), [])])));
    ("F",  (["x";"y";"k"], 
      App(T("br"), [App(NT("Close"), 
                     [App(Var("x"),[]);
                      App(NT("Close"), [App(Var("y"),[]); App(Var("k"),[])])]);
                    App(NT("Read"), 
                     [App(Var("x"),[]);
                      App(NT("Write"), [App(Var("y"),[]); 
                             App(NT("F"), [x;y;k])])])])));
    ("Newr", (["k"], App(T("br"),
                       [App(T("newr"), [App(Var("k"), [App(NT("I"), [])])]);
                        App(Var("k"), [App(NT("K"), [])])])));
    ("Neww", (["k"], App(T("br"),
                       [App(T("neww"), [App(Var("k"), [App(NT("I"), [])])]);
                        App(Var("k"), [App(NT("K"), [])])])));
    ("I", (["x"; "y"], App(Var("x"), [y])));
    ("K", (["x"; "y"], y));
    ("Close", (["x"; "k"],
          App(Var("x"), [c; k])));
    ("Read", (["x"; "k"],
          App(Var("x"), [r; k])));
    ("Write", (["x"; "k"],
          App(Var("x"), [w; k])))]
let g5 = {nt=nt5; t=t5; r=r5; s="S"}
    

let t6 = [("br", Kfun(O, Kfun(O, O))); 
          ("read", Kfun(O, O));
          ("write", Kfun(O, O));
          ("close", Kfun(O, O)); 
          ("d", O); 
          ("newr", Kfun(O,O));
          ("neww", Kfun(O,O))]
let resty = Kfun(Kfun(O,O), Kfun(O,O))
let nt6 =[("S", O); 
          ("C1", Kfun(Kfun(resty,O),O));
          ("C2", Kfun(resty, Kfun(Kfun(resty,O),O)));
          ("Newr", Kfun(Kfun(resty,O), O));
          ("Neww", Kfun(Kfun(resty,O), O));
          ("Close", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("Read", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("Write", Kfun(Kfun(Kfun(O, O), Kfun(O,O)), Kfun(Kfun(O,O),O)));
          ("I", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("K", Kfun(Kfun(O, O), Kfun(O,O))); 
          ("F", Kfun(resty, Kfun(resty, Kfun(Kfun(O,O), O))))
         ]
let r6 =
   [("S", ([], App(NT("Newr"), [App(NT("C1"), [])])));
    (*** Newr in the following line should be Neww ***)
    ("C1", (["x"], App(NT("Newr"), [App(NT("C2"), [App(Var("x"), [])])])));
    ("C2", (["x";"y"], 
       App(NT("F"), [App(Var("x"), []);App(Var("y"), []);App(T("d"), [])])));
    ("F",  (["x";"y";"k"], 
      App(T("br"), [App(NT("Close"), 
                     [App(Var("x"),[]);
                      App(NT("Close"), [App(Var("y"),[]); App(Var("k"),[])])]);
                    App(NT("Read"), 
                     [App(Var("x"),[]);
                      App(NT("Write"), [App(Var("y"),[]); 
                             App(NT("F"), [x;y;k])])])])));
    ("Newr", (["k"], App(T("br"),
                       [App(T("newr"), [App(Var("k"), [App(NT("I"), [])])]);
                        App(Var("k"), [App(NT("K"), [])])])));
    ("Neww", (["k"], App(T("br"),
                       [App(T("neww"), [App(Var("k"), [App(NT("I"), [])])]);
                        App(Var("k"), [App(NT("K"), [])])])));
    ("I", (["x"; "y"], App(Var("x"), [y])));
    ("K", (["x"; "y"], y));
    ("Close", (["x"; "k"],
          App(Var("x"), [c; k])));
    ("Read", (["x"; "k"],
          App(Var("x"), [r; k])));
    ("Write", (["x"; "k"],
          App(Var("x"), [w; k])))]
let g6 = {nt=nt6; t=t6; r=r6; s="S"}
    