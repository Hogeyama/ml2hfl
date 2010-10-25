open Syntax;;
open Utilities;;
(*#use "koba/utilities.ml";;*)

exception Untypable

let print_prog t defs =
   Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t defs);;

let print_defs defs =
   Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) Syntax.Unit defs);;

let print_defs2 defs =
   Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,xs,t) -> Letrec(f,xs,t,acc)) Syntax.Unit defs);;
let print_string2 s = Format.printf "%s" s

let print_ident x =
   (Format.printf "%a" print_id x; flush stdout)


let print_term t =
   Format.printf "%a" (print_term_fm ML false) t;;

let print_termlist tl =
   List.iter (fun t -> Format.printf "%a\n" (print_term_fm ML true) t) tl;;

let print_ldefs ldefs =
   List.iter
     (fun (x,n,tl,t) -> 
      (print_string2 "id: ";
       print_ident x;
       print_string2 "\ncond:";
       print_termlist tl;
       print_string2 "\nterm: ";
       print_term t;
       print_string2 "\n")) ldefs

let print_ldefs2 ldefs =
   List.iter
     (fun (x,n,tl,t) -> 
      (print_string2 "term: ";
       print_term x;
       print_string2 "\ncond:";
       print_termlist tl;
       print_string2 "\nterm: ";
       print_term t;
       print_string2 "\n")) ldefs
       

let print_c c = Format.printf "\n\n %a\n" Syntax.print_constr_list c
let print_ca c = Format.printf "%a\n" Syntax.print_constr c

let print_allc l =
  List.iter (fun (x,y) -> (print_c x; print_string2 "==>"; print_ca y; print_string2 "\n\n")) l

let print_trace l =
   List.iter (fun (l,x,y,n) ->
               (print_string2 "cond:\n";
                print_termlist l; 
                print_string2 "term1: ";
                print_term x; print_string2 "\n term2: ";print_term y; print_string2 "\n";
                print_int n;print_string2 "\n\n")) l

type tinfo = ety list ref
and ety = ETfun of tinfo * ety | ETunit of int | ETindirect of tinfo 

type aty = ATunit of int | ATfun of aty list * aty | ATint of int | ATbool of int
 (** Integers added to ATint and ATbool will be used as an identifier of the predicate **)

type myterm = MyUnit of tinfo | MyFail of tinfo | MyVar of ident * tinfo
            | MyApp of myterm * myterm * tinfo | MyTerm of Syntax.t * tinfo

let new_tinfo() =
  ref []

let get_tinfo term =
  match term with
      MyUnit(ti) -> ti
    | MyFail(ti) -> ti
    | MyVar(_,ti) -> ti
    | MyApp(_,_,ti) -> ti
    | MyTerm(_,ti) -> ti

let replace_tinfo ti term =
  match term with
      MyUnit(_) -> MyUnit(ti)
    | MyFail(_) -> MyFail(ti)
    | MyVar(x,_) -> MyVar(x,ti)
    | MyApp(t1,t2,ti) -> MyApp(t1,t2,ti) (*???*)
    | MyTerm(t,_) -> MyTerm(t,ti)

let tinfomap = ref []

let add_to_tinfo newinfo tinfo =
  let tys = !tinfo in
      tinfo := newinfo::tys

let register_tinfo f (tinfo: tinfo) =
  let r = 
        try List.assoc f !tinfomap 
        with Not_found ->
          let r1 = ref [] in
          (tinfomap := (f, r1)::!tinfomap;
           r1)
  in
  let tys = !r in
    r := tinfo::tys

let branchtab = ref []
(*let register_branch b counter =
  let r = 
        try List.assoc counter !branchtab
        with Not_found ->
          let r1 = ref [] in
          (branchtab := (counter, r1)::!branchtab;
           r1)
  in
  let trace = !r in
    r := trace@[b]*)

let register_branches trace counter =
  let r = 
        try List.assoc counter !branchtab
        with Not_found ->
          let r1 = ref [] in
          (branchtab := (counter, r1)::!branchtab;
           r1)
  in
  let traces = !r in
    r := traces@[trace]


let rec tinfolist2ty typ tinfolist =
  List.fold_left 
   (fun ty -> fun tinfo -> 
       merge_and_unify compare ty (tinfo2ty typ tinfo))
   [] tinfolist
and tinfo2ty typ tinfo =
  let ty1 =
    List.fold_left
     (fun ty -> fun ety ->
      merge_and_unify compare ty (ety2ty typ ety))
      [] (!tinfo)
  in
     if ty1=[] then
         match typ with
            TInt _ -> [ATint(0)]
          | TBool -> [ATbool(0)]
          | _ -> ty1
     else ty1
and ety2ty typ ety =
  match ety with
    ETfun(tinfo',ety1) ->
       (match typ with
           TFun((_,typ1),typ2) ->
             let ty1 = tinfo2ty typ1 tinfo' in
             let ty2 = ety2ty typ2 ety1 in
               (match ty2 with
                  [aty] -> [ATfun(ty1,aty)]
                 | _ -> raise (Fatal "ety2ty"))
         | _ -> raise (Fatal "ety2ty: does not match type annotation"))
  | ETunit(n) -> [ATunit(n)]
  | ETindirect(tinfo) -> tinfo2ty typ tinfo



let rec tinfomap2atenv tinfomap =
    match tinfomap with
    [] -> []
  | (f,r)::tinfomap' ->
      (f, tinfolist2ty f.typ !r)::(tinfomap2atenv tinfomap')

type pid = int

let current_pid = ref 0
let new_pid() =
     let x = !current_pid in
       (current_pid := !current_pid+1;
        x)

let rec rename_aty aty =
  match aty with
    ATunit(n) -> ATunit(n)
  | ATint(n) -> ATint(new_pid())
  | ATbool(n) -> ATbool(new_pid())
  | ATfun(ty1,aty1) ->
      ATfun(rename_ty ty1, rename_aty aty1)

and rename_ty ty =
  List.map rename_aty ty

let mk_atenv () =
  let ate = tinfomap2atenv !tinfomap in
  let ate' = List.map (fun (f, ty) -> (f, rename_ty ty)) ate in
      ate'

let rec print_ty ty indent=
  match ty with
    [] -> print_string2 "top"
(** (match typ with TInt _-> print_string2 "int" 
               | TBool  -> print_string2 "bool" | _ -> print_string2 "top")
**)
  | [aty] ->
       print_aty aty
  | aty::ty' ->
      (print_aty aty;
       print_string2 indent;
       print_ty ty' indent)
and print_aty aty =
  match aty with
    ATfun(ty1,aty1) ->
            (print_string2 "("; print_ty ty1 "/\\"; print_string2 "-> ";
             print_aty aty1; print_string2 ")")
  | ATunit(n) -> print_string2 ("unit("^(string_of_int n)^")")
  | ATint(n) -> print_string2 "int"
  | ATbool(n) -> print_string2 "bool"

let rec print_atenv atenv =
  match atenv with
    [] -> ()
  | (f,ty)::atenv' ->
      (print_ident f; print_string2 ": \n ";
       print_ty ty "\n";
       print_string2 "\n";
       print_atenv atenv')


let rec mk_appterm h terms =
  match terms with
    [] -> h
  | t::terms' ->
      let t1 = MyApp(h, t, new_tinfo()) in
       mk_appterm t1 terms'

exception Unsupported of Syntax.t
exception Undefined of ident

let rec process_term trace term traces env pcounter =
  match term with
      Unit -> let _ = register_branches trace pcounter in [MyUnit(new_tinfo()), trace, traces]
    | If(t1,t2,t3,t4) ->
        let tts, tfs = List.partition (function [] -> assert false | true::_ -> true | false::_ -> false) traces in
        let tts = List.map List.tl tts in
        let tfs = List.map List.tl tfs in
        (if tts = [] then [] else (process_term (trace @ [true ]) t2 tts env pcounter)) @
        (if tfs = [] then [] else (process_term (trace @ [false]) t3 tfs env pcounter))
    | App(Fail,_) -> let _ = register_branches trace pcounter in [MyFail(new_tinfo()), trace, traces]
    | Var(x) -> 
        let _ = register_branches trace pcounter in
        (try
           let myt = List.assoc x env in
             [myt, trace, traces]
         with Not_found ->
           let tinfo = new_tinfo() in
           let h = MyVar(x, tinfo) in
           let _ = register_tinfo x tinfo in
             [h, trace, traces])
    | App(Var x, ts) ->
        let _ = register_branches trace pcounter in
        let myts = List.map (fun t -> process_aterm env t) ts in
          (try
             let myt = List.assoc x env in
               [mk_appterm myt myts, trace, traces]
           with Not_found ->
             let tinfo = new_tinfo() in
             let h = MyVar(x, tinfo) in
             let _ = register_tinfo x tinfo in
               [mk_appterm h myts, trace, traces])
    | BinOp(_) -> let _ = register_branches trace pcounter in [MyTerm(term, new_tinfo()), trace, traces]
    | NInt _ -> let _ = register_branches trace pcounter in [MyTerm(term, new_tinfo()), trace, traces]
    | Int _ ->  let _ = register_branches trace pcounter in [MyTerm(term, new_tinfo()), trace, traces]
    | True -> let _ = register_branches trace pcounter in [MyUnit(new_tinfo()), trace, traces] (*???*)
    | False -> let _ = register_branches trace pcounter in [MyUnit(new_tinfo()), trace, traces] (*???*)
    | Not(_) -> let _ = register_branches trace pcounter in [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | App(t, []) -> process_term trace t traces env pcounter
    | _ -> (print_term term; print_string2 "\n"; raise (Unsupported term))
and process_aterm env t =
  let [t1,_,_] = process_term [] t [] env 0 in t1
          
let rec process_args term =
   match term with
     MyApp(t1,t2,tinfo) ->
        let r = new_tinfo() in
        let tinfo2 = get_tinfo t2 in
        let _ = add_to_tinfo (ETindirect(r)) tinfo2 in
        let t2' = replace_tinfo r t2 in (*???*)
        let t1' = process_args t1 in
          MyApp(t1',t2',tinfo)
    | _ -> term

let rec process_head term ety =
   match term with
      MyApp(t1,t2,tinfo) ->
        let _ = add_to_tinfo ety tinfo in
        let tinfo_arg = get_tinfo t2 in
        let ety_fun = ETfun(tinfo_arg, ety) in
          process_head t1 ety_fun
    | MyVar(x, tinfo) -> add_to_tinfo ety tinfo
    | MyFail(tinfo) -> add_to_tinfo ety tinfo
    | MyUnit(tinfo) -> add_to_tinfo ety tinfo
    | MyTerm _ -> assert false

exception Decomp of myterm

let rec decompose_redex term =
  match term with
    MyVar(f,_) -> (f, [])
  | MyApp(t1,t2,_) ->
       let (f,ts) = decompose_redex t1 in
         (f, ts@[t2])
  | _ -> raise (Decomp term)

(*let pc = ref (-1);;*)

let incr_counter =
  let r = ref (-1) in
  (fun () -> r := !r + 1; !r)


let trace2id = ref []

let rec eval_term t defs traces pcounter =
  (*let _ = (pc := counter) in*)
  
  match t with
    MyUnit(tinfo) | MyFail(tinfo) ->
      let counter = incr_counter () in
      trace2id := ((pcounter, []), counter)::!trace2id;
      add_to_tinfo (ETunit(counter)) tinfo
  | MyVar(f,tinfo) ->
      let (vars,body) = 
         try List.assoc f defs 
         with Not_found -> raise (Undefined(f))
      in
      let env = [] in
      List.iter
        (fun (t2, trace, traces') ->
          let counter = incr_counter () in
          trace2id := ((pcounter, trace), counter)::!trace2id;
          let t3 = process_args t2 in
          process_head t3 (ETunit(counter));
          eval_term t3 defs traces' counter)
        (process_term [] body traces env pcounter)
  | MyApp(t1, t2, tinfo) ->
      let (f, ts) = decompose_redex t in
      let (vars,body) = 
         try List.assoc f defs 
         with Not_found -> raise (Undefined(f)) in
      let env = List.combine vars ts in
      List.iter
        (fun (t2, trace, traces') ->
          let counter = incr_counter () in
          trace2id := ((pcounter, trace), counter)::!trace2id;
          let t3 = process_args t2 in
          process_head t3 (ETunit(counter));
          eval_term t3 defs traces' counter)
        (process_term [] body traces env pcounter)
  | MyTerm _ -> assert false


(** from types to refinement type templates **)
type pred = Pred of pid * Syntax.t list 
type rty = RTunit of int | RTint of (Syntax.t -> pred) | RTbool of (Syntax.t -> pred)
        | RTifun of (Syntax.t -> pred) * (Syntax.t -> rty) | RTbfun of (Syntax.t -> pred) * (Syntax.t-> rty)
        | RTfun of rty list * rty

type ac = Cpred of pred | Csub of rty * rty | Cterm of Syntax.t | Cimp of ac list * ac list | Cfalse

let rec aty2rty aty vars =
  match aty with
     ATunit(n) -> RTunit(n)
   | ATint(n) -> 
        RTint(fun x->Pred(n,x::vars))
   | ATbool(n) ->
        RTbool(fun x->Pred(n,x::vars))
   | ATfun(ATint(n)::_, aty1) ->
         RTifun((fun x-> Pred(n,x::vars)), (fun x->aty2rty aty1 (x::vars)))
   | ATfun(ATbool(n)::_, aty1) ->
         RTbfun((fun x-> Pred(n,[x])), (fun x->aty2rty aty1 (x::vars)))
   | ATfun(ty1, aty1) ->
         RTfun(ty2rtyl ty1 vars, aty2rty aty1 vars)
and ty2rtyl ty vars =
  List.map (fun aty->aty2rty aty vars) ty

(*
let rec aty2rty aty vars =
  match aty with
     ATunit(n) -> RTunit(n)
   | ATint(n) -> 
        RTint(fun x->Pred(n,[x]))
   | ATbool(n) ->
        RTbool(fun x->Pred(n,[x]))
   | ATfun(ATint(n)::_, aty1) ->
         RTifun((fun x-> Pred(n,[x])), (fun x->aty2rty aty1 [x]))
   | ATfun(ATbool(n)::_, aty1) ->
         RTbfun((fun x-> Pred(n,[x])), (fun x->aty2rty aty1 [x]))
   | ATfun(ty1, aty1) ->
         RTfun(ty2rtyl ty1 vars, aty2rty aty1 vars)
and ty2rtyl ty vars =
  List.map (fun aty->aty2rty aty vars) ty
*)

let current_vid = ref 1
(*
let new_var() =
  let vid = !current_vid in
   (current_vid := !current_vid + 1;
    Var({id=vid; origin="x"; typ=TUnit})) (** type is a dummy **)
*)
let new_var() = Var({id=new_int (); origin="x"; typ=TUnit})
let dummy_var = 
  Var({id=0; origin="x"; typ=TUnit})

let print_var x =
  match x with Var(id) -> print_ident id
    | _ -> assert false

let print_pname pid =
  (print_string2 ("P"^(string_of_int pid)))

let rec print_terms terms =
  match terms with
    [] -> ()
  | [term] -> print_term term
  | term::terms' -> print_term term; print_string2 "; "; print_terms terms'

let print_pred pred =
   match pred with
      Pred(pid, terms) ->
        (print_pname pid;
         print_string2 "(";
         print_terms terms;
         print_string2 ")")

let rec print_rty rty =
  match rty with
    RTunit(n) -> print_string2 ("Unit("^(string_of_int n)^")")
  | RTint(f) -> let x = new_var() in 
                let pred = f x in
                 (print_string2 "{";
                  print_var x;
                  print_string2 ":int | ";
                  print_pred pred;
                  print_string2 "}")
  | RTbool(f) ->let x = new_var() in 
                let pred = f x in
                 (print_string2 "{";
                  print_var x;
                  print_string2 ":bool | ";
                  print_pred pred;
                  print_string2 "}")
  | RTifun(f,g) ->
           let x = new_var() in 
           let (pred, rty1) = (f x, g x) in
              (print_var x;
               print_string2 ":{";
               print_var x;
               print_string2 ":int | ";
               print_pred pred;
               print_string2 "} -> ";
               print_rty rty1)
  
  | RTbfun(f, g) ->
           let x = new_var() in 
           let (pred, rty1) = (f x, g x) in
              (print_var x;
               print_string2 ":{";
               print_var x;
               print_string2 ":bool | ";
               print_pred pred;
               print_string2 "} -> ";
               print_rty rty1)
  | RTfun(rtyl1,rty2) ->
       (print_rtyl rtyl1; 
        print_string2 " -> ";
        print_rty rty2)
and print_rtyl rtyl =
  match rtyl with
     [] -> print_string2 "top"
   | [rty] -> 
       (print_string2 "("; print_rty rty; print_string2 ")")
   | rty0::rtyl' ->
       (print_string2 "("; print_rty rty0; print_string2 ")/\\";
        print_rtyl rtyl')

let print_rtenv rtenv =
  List.iter 
  (fun (f,ty) ->
     (print_ident f; print_string2 ":\n ";
      List.iter (fun rty -> print_rty rty; print_string2 "\n ") ty))
  rtenv

let atenv2rtenv atenv =
  List.map (fun (f,ty) -> (f, ty2rtyl ty [])) atenv

let rec id_of_rty rty =
  match rty with
   RTunit(id) -> id
 | RTifun(_, g) ->
    let rty1 = g (dummy_var) in
       id_of_rty rty1
 | RTbfun(_, g) ->
    let rty1 = g (dummy_var) in
       id_of_rty rty1
 | RTfun(_,rty1) ->
       id_of_rty rty1
 | _ -> raise (Fatal "id_of_rty: codomain is not unit")

exception PickRty

let pick_rty id rtyl =
  let rtyl1 = List.filter (fun rty -> id=(id_of_rty rty)) rtyl in
    match rtyl1 with
        [] -> raise PickRty  (**  (Fatal ("pick_rty: no type with id"^(string_of_int id)))**)
     | [rty] -> rty
     | _ -> raise (Fatal ("pick_rty: multiple types with id"^(string_of_int id)))


let rec print_ac ac =
  match ac with
    Cpred(pred) -> print_pred pred
  | Csub(rty1,rty2) -> 
        (print_string2 "(";
         print_rty rty1;
         print_string2 " <: ";
         print_rty rty2;
         print_string2 ")")
  | Cterm(t) ->
         print_term t
  | Cimp(c1, c2) ->
      (print_string2 "[";
       print_c c1 "; ";
       print_string2 "==>";
       print_c c2 "; ";
       print_string2 "]")
  | Cfalse ->
       print_string2 "false"
and print_c c sep =
  match c with
    [] -> print_string2 "true"
  | [ac] -> print_ac ac
  | ac::c' -> (print_ac ac; print_string2 sep; print_c c' sep)

let print_constraint c =
  print_c c "\n"(*"\n\n"*)

let subty rty1 rty2 = [Csub(rty1,rty2)]

let rec chk_term rtenv term id trace traces =
  match term with
    Unit -> []
  | App(Var x, ts) ->
     let id = try List.assoc (id, trace) !trace2id with Not_found -> assert false in
     let rty = RTunit(id) in
     (try
       let rtyl = try List.assoc x rtenv with Not_found -> [] in
       let rty1 = pick_rty id rtyl in
       let (c1,rty2)= chk_args rtenv rty1 ts in
       let c2 = subty rty2 rty in
          c1@c2
     with
       PickRty -> [Cfalse])
  | App(Fail, _) -> [Cfalse]
  | If(t1,t2,t3,_) ->
      let tts, tfs = List.partition (function [] -> raise (Fatal "ch_term: trace information is missing") | true::_ -> true | false::_ -> false) traces in
      let tts = List.map List.tl tts in
      let tfs = List.map List.tl tfs in
      let c1 = if tts = [] then [] else [Cimp([Cterm(t1)], chk_term rtenv t2 id (trace @ [true]) tts)] in
      let c2 = if tfs = [] then [] else [Cimp([Cterm(Not t1)], chk_term rtenv t3 id (trace @ [false]) tfs)] in
        c1 @ c2
  | Var x -> chk_term rtenv (App(Var x, [])) id trace traces
  | Fail -> [Cfalse]
  | True | False -> assert false
  | Not(t) -> (*???*)
      chk_term rtenv t id trace traces
  | App(t, []) -> chk_term rtenv t id trace traces
  | _ -> (print_term term; print_string2 "\n"; raise (Unsupported term))

and chk_args rtenv rty terms =
  match terms with
    [] -> ([], rty)
  | term::terms' ->
       match rty with
            RTifun(f, g) ->
             let (pred, rty2) = (f term, g term) in
             let (c1, rty3) = chk_args rtenv rty2 terms' in
                (Cpred(pred)::c1, rty3)
         | RTbfun(f, g) ->
             let (pred, rty2) = (f term, g term) in
             let (c1, rty3) = chk_args rtenv rty2 terms' in
                (Cpred(pred)::c1, rty3)
         | RTfun(rtyl1,rty2) ->
             let c1 = chk_term_rtyl rtenv term rtyl1 in
             let (c2, rty3) = chk_args rtenv rty2 terms' in
                (c1@c2, rty3)
         | _ -> raise (Fatal "chk_args: non-function type")

and chk_term_rty rtenv term rty =
 let id = id_of_rty(rty) in
  match term with
    Unit -> 
       (match rty with 
             RTunit(_) -> []
           | _ -> raise (Fatal "chk_term_rty: () cannot be used as a non-unit type")
        )
  | App(Var x, ts) ->
     (try
       let rtyl = try List.assoc x rtenv with Not_found -> [] in
       let rty1 = pick_rty id rtyl in
       let (c1,rty2)= chk_args rtenv rty1 ts in
       let c2 = subty rty2 rty in
          c1@c2
     with
       PickRty -> [Cfalse])
  | App(Fail, _) -> [Cfalse]
  | If(t1,t2,t3,_) -> assert false
  | Var x -> chk_term_rty rtenv (App(Var x, [])) rty
  | Fail -> [Cfalse]
  | True | False -> (*???*)
       (match rty with 
             RTbool(_) -> []
           | _ -> raise (Fatal "chk_term_rty: () cannot be used as a non-boolean type")
        )
  | Not(t) -> (*???*)
      chk_term_rty rtenv t rty
  | App(t, []) -> chk_term_rty rtenv t rty
  | _ -> (print_term term; print_string2 "\n"; raise (Unsupported term))

and chk_term_rtyl rtenv term rtyl =
  List.fold_left (fun c -> fun rty ->
     (chk_term_rty rtenv term rty)@c) [] rtyl 

let rec mk_venv vars rty =
  match vars with
    [] -> (match rty with
             RTunit(id) -> ([], id)
           | _ -> raise (Fatal "mk_venv: too few formal parameters"))
 | v::vars' ->
       (match rty with
            RTifun(f, g) ->
             let rty2 = g (Var v) in
             let (venv, id) = mk_venv vars' rty2 in
             let rty_v = RTint(f) in
               ((v,[rty_v])::venv, id)
         | RTbfun(f, g) ->
             let rty2 = g (Var v) in
             let (venv, id) = mk_venv vars' rty2 in
             let rty_v = RTbool(f) in
               ((v,[rty_v])::venv, id)
         | RTfun(rtyl1,rty2) ->
             let (venv, id) = mk_venv vars' rty2 in
               ((v, rtyl1)::venv, id)
         | _ -> raise (Fatal "mk_venv: too many formal parameters")
        )

let getc_from_rtyl v rtyl =
  match rtyl with
    [RTint(f)] -> [Cpred(f (Var v))]
  | [RTbool(f)] -> [Cpred(f (Var v))]
  | _ -> []

let getc_from_env env =
  List.fold_left
    (fun c -> fun (v,rtyl) ->
       merge_and_unify compare
         (getc_from_rtyl v rtyl) c)
     [] env

let cgen_flag = ref true (** this should be set to false if we want to propagate conditions on free variables eagerly **)

let chk_def_rty vars body rty rtenv =
  let (venv1, id) = mk_venv vars rty in
  let rtenv' = venv1@rtenv in
  let traces = try let r = List.assoc id !branchtab in !r with 
               Not_found -> []
  in
  let c = chk_term rtenv' body id [] traces in
  let assumption = getc_from_env venv1 in
    [Cimp(assumption, c)]

let chk_def vars body rtyl rtenv =
  List.fold_left 
 (fun c-> fun rty -> (chk_def_rty vars body rty rtenv)@c)
  []
  rtyl

let rec gen_constr defs rtenv =
  match defs with
    [] -> []
  | (f, (vars,body))::defs' ->
      let rtyl = try List.assoc f rtenv with Not_found -> [] in
      let c1 = chk_def vars body rtyl rtenv in
      let c2 = gen_constr defs' rtenv in
       c1@c2

exception SubTincompatible of rty * rty

let rec reduce_subty rty1 rty2 =
  match (rty1,rty2) with
    (RTunit(m), RTunit(n)) -> 
      if m=n then [] else raise (SubTincompatible(rty1,rty2))
  | (RTint(f1), RTint(f2)) ->
      let v = new_var() in
      let pred1 = f1 v in
      let pred2 = f2 v in
         [Cimp([Cpred(pred1)], [Cpred(pred2)])]
  | (RTbool(f1), RTbool(f2)) ->
      let v = new_var() in
      let pred1 = f1 v in
      let pred2 = f2 v in
         [Cimp([Cpred(pred1)], [Cpred(pred2)])]
  | (RTifun(f1,g1), RTifun(f2,g2)) ->
      let v = new_var() in
      let pred1 = f1 v in
      let pred2 = f2 v in
      let c1 = reduce_subty (g1 v) (g2 v) in
      let c2 = merge_and_unify compare [Cpred(pred1)] c1 in
        [Cimp([Cpred(pred2)], c2)]
  | (RTbfun(f1,g1), RTbfun(f2,g2)) ->
      let v = new_var() in
      let pred1 = f1 v in
      let pred2 = f2 v in
      let c1 = reduce_subty (g1 v) (g2 v) in
      let c2 = merge_and_unify compare [Cpred(pred1)] c1 in
        [Cimp([Cpred(pred2)], c2)]
  | (RTfun(rtyl11,rty12), RTfun(rtyl21,rty22)) ->
      let c1 = reduce_subtyl rtyl21 rtyl11 in
      let c2 = reduce_subty rty12 rty22 in
        merge_and_unify compare c1 c2
  | _ -> assert false
and reduce_subtyl rtyl1 rtyl2 =
  List.fold_left
   (fun c -> fun rty2 ->
      merge_and_unify compare
        (reduce_subtyl_ty rtyl1 rty2) c)
  [] rtyl2
and reduce_subtyl_ty rtyl1 rty2 =
  let id = id_of_rty(rty2) in
  let rty1 = pick_rty id rtyl1 in
    reduce_subty rty1 rty2

let rec reduce_constr c =
  List.fold_left 
  (fun c1 -> fun ac ->
     merge_and_unify compare (reduce_ac ac) c1)
   [] c
and reduce_ac ac =
  match ac with
    Cpred _ -> [ac]
  | Csub(rty1,rty2) -> reduce_subty rty1 rty2
  | Cterm _ -> [ac]
  | Cimp(c1,c2) ->
      [Cimp(reduce_constr c1, reduce_constr c2)]
  | Cfalse -> [ac]

let rec normalize_constr c =
  List.fold_left 
  (fun c1 -> fun ac ->
     merge_and_unify compare (normalize_ac ac) c1)
   [] c
and normalize_ac ac =
  match ac with
    Cpred _ -> [ac]
  | Csub(rty1,rty2) -> assert false
  | Cterm _ -> [ac]
  | Cimp(c1,c2) ->
      let c1' = normalize_constr c1 in
      let c2' = normalize_constr c2 in
      let c = List.map (fun ac -> match ac with Cimp(c3, c4) -> Cimp(c1' @ c3, c4) | _ -> Cimp(c1', [ac])) c2' in
        List.map (function Cimp(c3,[Cterm t]) -> Cimp(c3@[Cterm(Not t)], [Cfalse]) | ac -> ac) c
  | Cfalse -> [ac]



let save_as_dot filename cs =
  let es = Util.uniq (List.concat (List.map
    (fun ac ->
      match ac with
        Cimp(c1, c2) ->
          let lhs = List.map
            (function Cpred(Pred(pid, _)) -> pid | _ -> assert false)
            (List.filter (function Cpred(_) -> true | _ -> false) c1)
          in
          (match c2 with
               [Cfalse] -> List.map (fun pid -> "P" ^ string_of_int pid, "bot", "") lhs
             | [Cpred(Pred(pid2, _))] -> List.map (fun pid1 -> "P" ^ string_of_int pid1, "P" ^ string_of_int pid2, "") lhs
             | _ -> assert false)
       | _ -> assert false)
    cs))
  in
  let vs = Util.uniq (List.concat (List.map (fun (x, y, _) -> [x, y]) es)) in
  Util.save_as_dot filename vs es

let rec term_of c =
  List.fold_left 
  (fun t -> fun ac ->
     BinOp(And, t, term_of_ac ac))
   True c
and terms_of c =
  List.map term_of_ac c
and term_of_ac ac =
  match ac with
    Cpred _ -> assert false
  | Csub(rty1,rty2) -> assert false
  | Cterm t -> canonize (simplify t)
  | Cimp(c1,c2) ->
      let t1 = term_of c1 in
      let t2 = term_of c2 in
      BinOp(Or, Not t1, t2)
  | Cfalse -> False

let interpolate ids c1 c2 =
  let ts1 = terms_of c1 in
  let get_fv' = if !Flag.use_nint then get_fv else get_fv2 in
  let bv = list_diff (List.flatten (List.map get_fv' ts1)) ids in
  let ts2 = [subst_term (List.map (fun id -> id, new_var ()) bv) (Not (term_of c2))] in
  let t =
    try
      Wrapper.interpolation ts1 ts2
    with Wrapper.Satisfiable ->
      if not !Flag.use_nint
      then
        let bv = list_diff (List.flatten (List.map get_fv ts1)) ids in
        let ts2 = [subst_term (List.map (fun id -> id, new_var ()) bv) (Not (term_of c2))] in
          try
            Wrapper.interpolation ts1 ts2
          with Assert_failure _ -> raise Untypable
      else raise Untypable
  in
    (*  print_term t;*)
    t

let rec substc sub c =
  List.filter
    (fun ac ->
      match ac with
        Cterm(BinOp(Eq, t1, t2)) -> t1 <> t2
      | _ -> true)
    (List.map (subst_ac sub) c)
and subst_ac sub ac =
  match ac with
    Cpred(Pred(pid, terms)) -> Cpred(Pred(pid, List.map (subst_term sub) terms))
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> Cterm(Syntax.eval (subst_term sub term))
  | Cimp(c1,c2) -> Cimp(substc sub c1, substc sub c2)
  | Cfalse -> Cfalse

let rec subst_sol sol c =
  List.map (subst_sol_ac sol) c
and subst_sol_ac sol ac =
  match ac with
    Cpred(Pred(pid, terms)) ->
      (try
        let (ids, t) = List.assoc pid sol in
        Cterm(subst_term (List.combine ids terms) t)
      with Not_found ->
        Cterm(True))
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> Cterm(term)
  | Cimp(c1,c2) -> Cimp(subst_sol sol c1, subst_sol sol c2)
  | Cfalse -> Cfalse

let rec fv c = List.flatten (List.map fv_ac c)
and fv_ac ac =
  match ac with
    Cpred(Pred(_, terms)) -> List.flatten (List.map get_fv terms)
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> get_fv term
  | Cimp(c1,c2) -> (fv c1) @ (fv c2)
  | Cfalse -> []

let rec simplify_constr c =
  List.map simplify_ac c
and simplify_ac ac =
  match ac with
    Cimp(c, [Cpred(Pred(pid, terms))]) ->
      let fv1 = List.flatten (List.map get_fv terms) in
      let rec aux c1 c2 =
        let fv2 = fv c2 in
        let c11, c12 = List.partition (fun ac -> Util.inter (fv_ac ac) (fv1 @ fv2) <> []) c1 in
        if c11 = [] then
          c2
        else
          aux c12 (c11 @ c2)
      in
      Cimp(aux c [], [Cpred(Pred(pid, terms))])
   | Cimp(c, [Cfalse]) -> ac
   | _ -> (print_constraint [ac]; assert false)

let get_lb pid terms lbs= try List.assoc pid lbs with Not_found -> List.map (fun _ -> let Var(id) = new_var () in id) terms, [Cfalse]

let rec elim ids c =
  try
    let c1, c2 = List.partition (function Cterm(BinOp(Eq, Var(id), term)) -> not (List.mem id (ids @ get_fv term)) | _ -> false) c in
      if c1 = []
      then c2
      else
        match c1 with
            (Cterm(BinOp(Eq, Var(id), term)))::c1 -> elim ids (substc [id, term] (c1 @ c2))
          | _ -> assert false
  with Not_found ->
    c

let get_lhs c =
  List.flatten
  (List.map
    (function Cimp(c', _) ->
      List.map
       (function Cpred(Pred(pid, _)) -> pid | _ -> assert false)
       (List.filter (function Cpred(_) -> true | _ -> false) c')
    | _ -> assert false)
    c)

let solve_constr c =
  let eqs ids terms =
    let xs = List.combine ids terms in
    let xs1, xs2 = List.partition (fun (_, term) -> match term with Var(id) -> true | _ -> false) xs in
      List.map (fun (id, Var(id')) -> id', Var(id)) xs1,
    List.map (fun (id, term) -> Cterm(BinOp(Eq, Var(id), term))) xs 
  in
  let rec subst_ac lbs ac =
    match ac with
        Cpred(Pred(pid, terms)) ->
          let ids, c = get_lb pid terms lbs in
          let fv = Util.uniq (Util.diff (fv c) ids) in
            substc (List.combine ids terms) (substc (List.map (fun id -> id, new_var ()) fv) c)
              (*if pos then [Cimp(eqs ids terms, c)] else (eqs ids terms) @ c*)
      | Csub(rty1,rty2) -> assert false
      | Cterm t -> [Cterm t]
      | Cimp(c1,c2) -> [Cimp(List.flatten (List.map (fun ac -> subst_ac lbs ac) c1),
                             List.flatten (List.map (fun ac -> subst_ac lbs ac) c2))]
      | Cfalse -> [Cfalse]
  in
  let subst_constr lbs c = Util.uniq (List.flatten (List.map (fun ac -> subst_ac lbs ac) c)) in
  let rec compute_lbs c lbs =
    let c1, c2 = List.partition
      (fun ac -> match ac with
           Cimp(c1', c2') ->
             (match c2' with
                  [Cpred _] ->
                    List.for_all (function Cpred(Pred(pid, terms)) -> List.mem_assoc pid lbs | _ -> true) c1'
                | _ -> false)
         | _ -> assert false) c in
      if c1 = [] then
        lbs
      else
        let compute_lb ac =
          match ac with
              Cimp(cl, [Cpred(Pred(pid, terms))]) ->
                let ids = List.map (fun _ -> let Var(id) = new_var () in id) terms in
                let sub, eqs = eqs ids terms in
                  pid, (ids, elim ids (substc sub ((subst_constr lbs cl) @ eqs)))
            | _ -> assert false
        in
          compute_lbs c2 (lbs @ (List.map compute_lb c1))
  in
  let lbs = compute_lbs c [] in

  let _ = if Flag.debug then print_string2 "\nLower bounds:\n" in
  let _ = if Flag.debug then List.iter
    (fun (pid, (ids, c)) ->
       print_pname pid;
       print_string2 "(";
       print_terms (List.map (fun id -> Var(id)) ids);
       print_string2 ") = ";
       print_constraint c;
       print_string2 "\n")
    lbs
  in

  let rec solve_aux c solution =
    let lhs = get_lhs c in
    let c1, c2 = List.partition
      (fun ac -> match ac with
           Cimp(_, c') ->
             (match c' with
                  [Cfalse] -> true
                | [Cpred(Pred(pid, _))] -> List.mem_assoc pid solution && not (List.mem pid lhs)
                | _ -> assert false)
         | _ -> assert false)
      c
    in
      (*
        let _ = print_string2 "\nhoge: "; print_constraint c1 in
      *)
      if c1 = [] then
        let rec merge_solution sol =
          match sol with
              [] -> []
            | (pid, (ids, t))::sol' ->
                let sol1, sol2 = List.partition (fun (pid', _) -> pid = pid') sol' in
                let t = List.fold_left
                  (fun t (_, (ids', t')) ->
                    let t' = subst_term (List.combine ids' (List.map (fun id -> Var(id)) ids)) t' in
                    (*if Wrapper.equiv [] t (BinOp(And, t, t')) then t else*) BinOp(And, t, t'))
                  t sol1
                in
                  (pid, (ids, t))::(merge_solution sol2)
        in
          if c2 <> [] then begin
            print_string2 "c1:\n";
            print_constraint c1;
            print_string2 "\n";
            print_string2 "c2:\n";
            print_constraint c2;
            print_string2 "\n";
            save_as_dot "constraints_error.dot" c2;
            assert false (* call filter_constr before solve_constr *)
          end else
            merge_solution solution
      else
        let solution' = solution @
          (List.flatten
          (List.map
            (fun ac -> match ac with
                Cimp(c, [cc]) ->
                  let cub =
                    match cc with
                        Cfalse -> [Cfalse]
                      | Cpred(Pred(pid, terms)) ->
                          let ts = List.filter (fun (pid', _) -> pid = pid') solution in
                          let t =
                            List.fold_left
                              (fun t (_, (ids, t')) ->
                                 BinOp(And, t,
                                subst_term (List.combine ids terms) t'))
                              True ts in
                            [Cterm t]
                  in
                  let c1, c2 = List.partition (function Cpred(_) -> true | _ -> false) c in
                  let cub = [Cimp(c2, cub)] in
                  let rec solve_aux' cub sol = function
                      [] -> sol
                    | (Cpred(Pred(pid', terms')))::c ->
                        let ids', lb = get_lb pid' terms' lbs in
                        let sub, eqs = eqs ids' terms' in
                        let ub = substc sub [Cimp(eqs @ (subst_constr lbs c), cub)] in
                        let _ =
                          if Flag.debug then
                            begin
                              print_pname pid';
                              print_string2 "(";
                              print_terms terms';
                              print_string2 ")";
                              print_string2 "\n"
                            end
                        in
                        let t = interpolate ids' lb ub in
                        let sol = (pid', (ids', t))::sol in
                        let cub = [Cimp([Cterm(subst_term (List.combine ids' terms') t)], cub)] in
                          solve_aux' cub sol c
                  in
                    solve_aux' cub [] c1
              | _ -> print_ac ac; assert false)
          c1))
        in
          solve_aux c2 solution'
  in
  let sol = solve_aux c [] in
    List.map (fun (pid,(ids,t)) -> pid, (ids, Wrapper.simplify_bool_exp t)) sol

let add_pred pred c =
  let rec fv c = List.flatten (List.map fv_ac c)
  and fv_ac ac =
    match ac with
        Cpred(Pred(_, terms)) -> List.flatten (List.map get_fv2 terms)
      | Csub(rty1,rty2) -> assert false
      | Cterm(term) -> get_fv term
      | Cimp(c1,c2) -> (fv c1) @ (fv c2)
      | Cfalse -> []
  in
  let fv = get_fv2 pred in
  let aux = function
      Cimp(c1, c2) as ac ->
        if List.exists (fun x -> List.exists (fun y -> x.id = y.id) fv) (fv_ac ac)
        then Cimp(Cterm(pred)::c1, c2)
        else ac
    | _ -> assert false
  in
    List.map aux c

let filter_constr c =
  let c1, c2 = List.partition
    (fun ac -> match ac with
         Cimp(_, [Cfalse]) -> true
       | Cimp(_, [Cpred(Pred(pid, _))]) -> false
       | _ -> assert false) c
  in
  let rec aux c1 c2 =
    let lhs = get_lhs c1 in
    let c21, c22 = List.partition
      (fun ac -> match ac with
           Cimp(_, [Cpred(Pred(pid, _))]) ->
             List.mem pid lhs
         | _ -> assert false)
      c2
    in
    if c21 = [] then
      c1
    else
      aux (c1 @ c21) c22
  in
  aux c1 c2



let test s defs traces = 
  branchtab := [];
  tinfomap := [];
  trace2id := [];
  current_pid := 0;

  let _ = if Flag.debug then print_string2 "\n Program: \n" in
  let _ = if Flag.debug then print_defs defs in
  let _ = if Flag.debug then print_string2 "\n" in
  let ti = new_tinfo() in
  let _ = register_tinfo s ti in
  let et = MyVar(s, ti) in
  let counter = incr_counter () in
  let _ = process_head et (ETunit(counter)) in
    (eval_term et defs traces counter;
     let te = mk_atenv() in
     let _ = if Flag.debug then print_atenv te in
     let rte = atenv2rtenv te in
     let _ = if Flag.debug then print_string2 "\nType templates:\n" in
     let _ = if Flag.debug then print_rtenv rte in
     let c = gen_constr defs rte in
     let _ = if Flag.debug then print_string2 "\nConstraints:\n" in
     let _ = if Flag.debug then print_constraint c in
     let _ = if Flag.debug then print_string2 "\n" in
     let c' = reduce_constr c in
     let _ = if Flag.debug then print_string2 "\nReduced constraints:\n" in
     let _ = if Flag.debug then print_constraint c' in
     let _ = if Flag.debug then print_string2 "\n" in
     let c'' = normalize_constr c' in
     let _ = if Flag.debug then print_string2 "\nNormalized constraints:\n" in
     let _ = if Flag.debug then print_constraint c'' in
     let _ = if Flag.debug then print_string2 "\n" in
     let _ = if Flag.debug then save_as_dot "constraints.dot" c'' in
       (*
         let c''' = simplify_constr c'' in
         let _ = print_string2 "\nSimplified constraints:\n" in
         let _ = print_constraint c'' in
         let _ = print_string2 "\n" in
       *)

     let c''' = filter_constr c'' in
     let sol = solve_constr c''' in
     let _ = if Flag.debug then print_string2 "\nSolutions:\n" in
     let _ =
       if Flag.debug
       then
         List.iter
           (fun (pid, (ids, t)) ->
              print_pname pid;
              print_string2 "(";
              print_terms (List.map (fun id -> Var(id)) ids);
              print_string2 ") = ";
              print_term t;
              print_string2 "\n")
           sol in
     let _ =
       if Flag.debug then
         List.iter
           (fun ac ->
              if Wrapper.check [] (term_of_ac (subst_sol_ac sol ac))
              then ()
              else begin
                print_string2 "wrong solution for:";
                print_ac ac;
                print_string2 "\n";
                assert false
              end)
           c''
       else ()
     in
       rte, sol)
