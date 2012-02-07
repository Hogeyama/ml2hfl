open Syntax
open Type
open Utilities

exception Untypable

let pr_flag = ref false

let cgen_flag = ref true (** this should be set to false if we want to propagate conditions on free variables eagerly **)

let print_prog t defs =
  Format.printf "Not implemented"; assert false
(*
  Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t defs);;
*)

let print_defs defs =
  Format.printf "Not implemented"; assert false
(*
  Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) Syntax.Unit defs);;
*)

let print_defs2 defs =
  Format.printf "Not implemented"; assert false
(*
  Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,xs,t) -> Letrec(f,xs,t,acc)) Syntax.Unit defs);;
*)

let print_string2 s = Format.printf "%s" s

let print_ident x = Format.printf "%a@?" Id.print x


let print_term t =
   Format.printf "%a" pp_print_term t;;

let print_termlist tl =
   List.iter (fun t -> Format.printf "%a\n" pp_print_term t) tl;;

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

type myterm = MyUnit of tinfo | MyFail of tinfo | MyVar of id * tinfo
            | MyApp of myterm * myterm * tinfo | MyTerm of Syntax.typed_term * tinfo

let rec print_myterm t =
  match t with
      MyUnit(_) ->
        Format.printf "()"
    | MyFail(_) ->
        Format.printf "fail"
    | MyVar(id,_) ->
        print_ident id
    | MyApp(t1,t2,_) ->
        print_myterm t1;
        print_string2 " ";
        print_myterm t2
    | MyTerm(t,_) ->
        print_term t

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
    | MyApp(t1,t2,_) -> MyApp(t1,t2,ti)
    | MyTerm(t,_) -> MyTerm(t,ti)

let tinfomap = ref []

let add_to_tinfo newinfo tinfo =
  let tys = !tinfo in
      tinfo := newinfo::tys

let register_tinfo (f:id) (tinfo: tinfo) =
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
          | TRInt _ -> [ATint(0)]
          | TBool -> [ATbool(0)]
          | _ -> ty1
     else ty1
and ety2ty typ ety =
  match ety with
    ETfun(tinfo',ety1) ->
       (match typ with
           TFun({Id.typ=typ1},typ2) ->
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
      (f, tinfolist2ty (Id.typ f) !r)::(tinfomap2atenv tinfomap')

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

exception Unsupported of typed_term
exception Undefined of id

let invalid_counter = -10

let rec process_term trace term traces env pcounter =

  if false then
    begin
  (**)
  print_string2 ("id: " ^ (string_of_int pcounter) ^ "\n");
  print_string2 "process_term:\n";
  print_string2 "term:\n ";
  print_term term;
  print_string2 "\ntraces:\n";
  List.iter (fun trace -> print_string2 " "; List.iter (fun n -> print_string2 (string_of_node n ^ ".")) trace; print_string2 ".\n") traces;
  print_string2 "\n";
  (**)
    end;

  match term.desc with
      Unit ->
        if List.for_all (function [EventNode "unit"] -> true | _ -> false) traces then
          let trace = trace @ [EventNode "unit"] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyUnit(new_tinfo()), trace, traces]
        else if List.for_all (function [FailNode] -> true | _ -> false) traces then
          let trace = trace @ [FailNode] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyFail(new_tinfo()), trace, traces]
        else
          assert false
    | If(_,t2,t3) ->
        if List.for_all (function [EventNode"then_fail"] -> true | _ -> false) traces then
          let trace = trace @ [EventNode"then_fail"] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyFail(new_tinfo()), trace, traces]
        else if List.for_all (function [EventNode"else_fail"] -> true | _ -> false) traces then
          let trace = trace @ [EventNode"else_fail"] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyFail(new_tinfo()), trace, traces]
        else if List.mem [] traces then (*???*)
          []
        else
          let tts, tfs = List.partition (function LabNode(true)::_ -> true | LabNode(false)::_ -> false | _ -> assert false) traces in
          let tts = List.map List.tl tts in
          let tfs = List.map List.tl tfs in
            (if tts = [] then [] else (process_term (trace @ [LabNode(true) ]) t2 tts env pcounter)) @
              (if tfs = [] then [] else (process_term (trace @ [LabNode(false)]) t3 tfs env pcounter))
    | App({desc=Event("fail",false)},_)
    | Event("fail",false) ->
        let trace = trace @ [FailNode] in
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyFail(new_tinfo()), trace, traces]
    | App({desc=Event(s,false)}, [t]) ->
        if List.for_all (function EventNode(s')::_ -> s = s' | _ -> false) traces then
          let traces' = List.map List.tl traces in
            process_term (trace @ [EventNode(s)]) t traces' env pcounter
        else if List.for_all (function [FailNode] -> true | _ -> false) traces then
          let trace = trace @ [FailNode] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyFail(new_tinfo()), trace, traces]
        else
          assert false
    | Var(x) ->
        if Id.typ x = TUnit && List.for_all (function [FailNode] -> true | _ -> false) traces then
          let trace = trace @ [FailNode] in
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            [MyFail(new_tinfo()), trace, traces]
        else
          let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
            (try
               let myt = List.assoc x env in
                 [myt, trace, traces]
             with Not_found ->
               let tinfo = new_tinfo() in
               let h = MyVar(x, tinfo) in
               let _ = register_tinfo x tinfo in
                 [h, trace, traces])
    | App({desc=Var x}, ts) ->
        assert (ts <> []);
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
        let myts = List.map (fun t -> process_aterm env t) ts in
          (try
             let myt = List.assoc x env in
               [mk_appterm myt myts, trace, traces]
           with Not_found ->
             let tinfo = new_tinfo() in
             let h = MyVar(x, tinfo) in
             let _ = register_tinfo x tinfo in
               [mk_appterm h myts, trace, traces])
    | BinOp(_) ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces]
    | NInt _ ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces]
    | Int _ ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces]
    | True ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | False ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | Not(_) ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | App(t, []) -> process_term trace t traces env pcounter
    | Nil ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | Cons _ ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | Match(_,pats) ->
        let header = List.hd (List.hd traces) in
          begin
            match header with
                EventNode _ when List.for_all ((=) [header]) traces ->
                  let trace = trace @ [header] in
                  let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
                    [MyFail(new_tinfo()), trace, traces]
              | _ when List.mem [] traces -> [] (*???*)
              | _ ->
                  let aux (i,acc) (_,cond,t) =
                    let tss = List.filter (function PatNode n::_ -> n=i | _ -> assert false) traces in
                    let tss' = List.map List.tl tss in
                    let acc' = if tss' = [] then [] else process_term (trace @ [PatNode i]) t tss' env pcounter in
                      i+1, acc' @ acc
                  in
                    snd (List.fold_left aux (0,[]) pats)
          end
    | Constr _ ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | App({desc=RandInt false}, [{desc=Unit}]) ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces]
    | Bottom ->
        let _ = if pcounter <> invalid_counter then register_branches trace pcounter in
          [MyTerm(term, new_tinfo()), trace, traces] (*???*)
    | _ -> (print_string2 "process_term\n"; print_term term; print_string2 "\n"; raise (Unsupported term))
and process_aterm env t =
  match process_term [] t [] env invalid_counter with
      [t1,_,_] -> t1
    | _ -> assert false

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
(*
  (**)
  print_string2 ("id: " ^ (string_of_int pcounter) ^ "\n");
  print_string2 "eval_term:\n";
  print_string2 "term:\n";
  print_myterm t;
  print_string2 "\ntraces:\n";
  List.iter (fun trace -> print_string2 " "; List.iter (fun n -> print_string2 (string_of_node n ^ ".")) trace; print_string2 ".\n") traces;
  print_string2 "\n";
  (**)
*)
  match t with
      MyUnit(tinfo) | MyFail(tinfo) ->
(*
        let check = function
            [FailNode]
          | [EventNode "then_fail"]
          | [EventNode "else_fail"] -> ()
          | [EventNode s] -> assert (Str.string_match (Str.regexp "br[0-9]+_fail") s 0)
          | _ -> assert false
        in
          List.iter check traces;
*)
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
    | MyApp _ ->
        let (f, ts) = decompose_redex t in
        let (vars,body) = 
          try List.assoc f defs 
          with Not_found -> raise (Undefined(f))
        in
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
type pred = Pred of pid * Syntax.typed_term list 
type rty = RTunit of int | RTint of (Syntax.typed_term -> pred) | RTbool of (Syntax.typed_term -> pred)
        | RTifun of (Syntax.typed_term -> pred) * (Syntax.typed_term -> rty) | RTbfun of (Syntax.typed_term -> pred) * (Syntax.typed_term-> rty)
        | RTfun of rty list * rty

type ac = Cpred of pred | Csub of rty * rty | Cterm of Syntax.typed_term | Cimp of ac list * ac list | Cfalse

let rec aty2rty aty vars =
  match aty with
     ATunit(n) -> RTunit(n)
   | ATint(n) -> 
        RTint(fun x-> Pred(n,x::vars))
   | ATbool(n) ->
        RTbool(fun x-> Pred(n,x::vars))
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
let new_id() = {Id.id=Id.new_int (); Id.name="x"; Id.typ=TUnit}
let new_var typ = make_var {(new_id()) with Id.typ=typ}
let dummy_var = 
  {desc=Var({Id.id=0; Id.name="x"; Id.typ=TUnit}); typ=TUnit}

let print_var x =
  match x.desc with Var(id) -> print_ident id
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
  | RTint(f) -> let x = new_var (TInt[]) in
                let pred = f x in
                 (print_string2 "{";
                  print_var x;
                  print_string2 ":int | ";
                  print_pred pred;
                  print_string2 "}")
  | RTbool(f) ->let x = new_var TBool in
                let pred = f x in
                 (print_string2 "{";
                  print_var x;
                  print_string2 ":bool | ";
                  print_pred pred;
                  print_string2 "}")
  | RTifun(f,g) ->
           let x = new_var (TInt[]) in
           let (pred, rty1) = (f x, g x) in
              (print_var x;
               print_string2 ":{";
               print_var x;
               print_string2 ":int | ";
               print_pred pred;
               print_string2 "} -> ";
               print_rty rty1)

  | RTbfun(f, g) ->
           let x = new_var (TInt[]) in
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
     (print_ident f; print_string2 ":\n";
      List.iter (fun rty -> print_string2 " "; print_rty rty; print_string2 "\n") ty))
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
(*
  (**)
  print_string2 ("id: " ^ (string_of_int id) ^ "\n");
  print_string2 "term:\n";
  print_term term;
  print_string2 "\ntraces:\n";
  List.iter (fun trace -> print_string2 " "; List.iter (fun n -> print_string2 (string_of_node n ^ ".")) trace; print_string2 ".\n") traces;
  print_string2 "\n";
  (**)
*)
  match term.desc with
      Unit ->
        if List.for_all (function [EventNode "unit"] -> true | _ -> false) traces then
          []
        else if List.for_all (function [FailNode] -> true | _ -> false) traces then
          [Cfalse]
        else
          assert false
    | App({desc=Var x}, ts) ->
        if List.for_all (function [FailNode] -> true | _ -> false) traces then
          [Cfalse]
        else
          let id = try List.assoc (id, trace) !trace2id with Not_found -> assert false in
          let rty = RTunit(id) in
            (try
               let rtyl = try List.assoc x rtenv with Not_found -> [] in
               let rty1 = pick_rty id rtyl in
               let (c1,rty2)= chk_args rtenv rty1 (List.map init_rand_int ts) in
               let c2 = subty rty2 rty in
                 c1@c2
             with
                 PickRty -> [Cfalse])
    | App({desc=Event("fail",false)}, _) ->
        if List.for_all (function [FailNode] -> true | _ -> false) traces then
          [Cfalse]
        else
          assert false
    | App({desc=Event(s,false)}, [t]) ->
        if List.for_all (function EventNode(s')::_ -> s = s' | _ -> false) traces then
          let traces' = List.map List.tl traces in
            chk_term rtenv t id (trace @ [EventNode(s)]) traces'
        else if List.for_all (function [FailNode] -> true | _ -> false) traces then
          [Cfalse]
        else
          assert false
    | If(t1,t2,t3) ->
        if List.for_all (function [EventNode"then_fail"] -> true | _ -> false) traces then
          [Cimp([Cterm(t1)], [Cfalse])]
        else if List.for_all (function [EventNode"else_fail"] -> true | _ -> false) traces then
          [Cimp([Cterm({desc=Not t1;typ=TBool})], [Cfalse])]
        else
          let aux = function
              [] -> raise (Fatal "chk_term: trace information is missing")
            | LabNode(true)::_ -> true
            | LabNode(false)::_ -> false
            | _ -> assert false
          in
          let tts, tfs = List.partition aux traces in
          let tts = List.map List.tl tts in
          let tfs = List.map List.tl tfs in
          let c1 = if tts = [] then [] else
            if !cgen_flag then
              [Cimp([Cterm(t1)], chk_term rtenv t2 id (trace @ [LabNode(true)]) tts)]
            else
              (Cterm(t1)) :: (chk_term rtenv t2 id (trace @ [LabNode(true)]) tts)
          in
          let c2 = if tfs = [] then [] else
            if !cgen_flag then
              [Cimp([Cterm({desc=Not t1;typ=TBool})], chk_term rtenv t3 id (trace @ [LabNode(false)]) tfs)]
            else
              (Cterm({desc=Not t1;typ=TBool})) :: (chk_term rtenv t3 id (trace @ [LabNode(false)]) tts)
          in
            c1 @ c2
    | Match(t,pats) ->
        let header = List.hd (List.hd traces) in
          begin
            match header with
                EventNode _ when List.for_all ((=) [header]) traces -> [Cfalse] (***)
              | _ ->
                  let aux (i,acc) (_,cond,t) =
                    let tss = List.filter (function PatNode n::_ -> n=i | _ -> assert false) traces in
                    let tss' = List.map List.tl tss in
                    let acc' =
                      if tss' = []
                      then []
                      else chk_term rtenv t id (trace @ [PatNode i]) tss' (***)
                    in
                      i+1, acc'@acc
                  in
                    snd (List.fold_left aux (0,[]) pats)
          end
    | Var x -> chk_term rtenv ({desc=App({desc=Var x;typ=Id.typ x}, []);typ=Id.typ x}) id trace traces
    | True | False -> assert false
    | Not(t) -> (*???*)
        chk_term rtenv t id trace traces
    | App(t, []) -> chk_term rtenv t id trace traces
    | RandInt _ -> assert false
    | _ -> (print_string2 "chk_term\n"; print_term term; print_string2 "\n"; raise (Unsupported term))

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
    match term.desc with
        Unit -> 
          (match rty with 
               RTunit(_) -> []
             | _ -> raise (Fatal "chk_term_rty: () cannot be used as a non-unit type")
          )
      | App({desc=Var x}, ts) ->
          (try
             let rtyl = try List.assoc x rtenv with Not_found -> [] in
             let rty1 = pick_rty id rtyl in
             let (c1,rty2)= chk_args rtenv rty1 ts in
             let c2 = subty rty2 rty in
               c1@c2
           with
               PickRty -> [Cfalse])
      | App({desc=Event("fail",false)}, _) -> [Cfalse]
      | If(t1,t2,t3) -> assert false
      | Var x -> chk_term_rty rtenv ({desc=App({desc=Var x;typ=Id.typ x}, []);typ=Id.typ x}) rty
      | Event("fail",false) -> [Cfalse]
      | True | False -> (*???*)
          (match rty with 
               RTbool(_) -> []
             | _ -> raise (Fatal "chk_term_rty: () cannot be used as a non-boolean type")
          )
      | Not(t) -> (*???*)
          chk_term_rty rtenv t rty
      | App(t, []) -> chk_term_rty rtenv t rty
      | _ -> (print_string2 "chk_term_rty\n"; print_term term; print_string2 "\n"; raise (Unsupported term))

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
             let rty2 = g ({desc=Var v;typ=Id.typ v}) in
             let (venv, id) = mk_venv vars' rty2 in
             let rty_v = RTint(f) in
               ((v,[rty_v])::venv, id)
         | RTbfun(f, g) ->
             let rty2 = g ({desc=Var v;typ=Id.typ v}) in
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
    [RTint(f)] -> [Cpred(f ({desc=Var v;typ=Id.typ v}))]
  | [RTbool(f)] -> [Cpred(f ({desc=Var v;typ=Id.typ v}))]
  | _ -> []

let getc_from_env env =
  List.fold_left
    (fun c -> fun (v,rtyl) ->
       merge_and_unify compare
         (getc_from_rtyl v rtyl) c)
     [] env

let chk_def_rty vars body rty rtenv =
  let (venv1, id) = mk_venv vars rty in
  let rtenv' = venv1@rtenv in
  let traces =
    try
      let r = List.assoc id !branchtab in !r
    with Not_found ->
      []
  in
  let c = chk_term rtenv' body id [] traces in
  let assumption = getc_from_env venv1 in
    [Cimp(assumption, c)]

let chk_def vars body rtyl rtenv =
  List.fold_left
    (fun c-> fun rty -> (chk_def_rty vars body rty rtenv) @ c)
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
      let v = new_var (TInt[]) in
      let pred1 = f1 v in
      let pred2 = f2 v in
         [Cimp([Cpred(pred1)], [Cpred(pred2)])]
  | (RTbool(f1), RTbool(f2)) ->
      let v = new_var TBool in
      let pred1 = f1 v in
      let pred2 = f2 v in
         [Cimp([Cpred(pred1)], [Cpred(pred2)])]
  | (RTifun(f1,g1), RTifun(f2,g2)) ->
      let v = new_var (TInt[]) in
      let pred1 = f1 v in
      let pred2 = f2 v in
      let c1 = reduce_subty (g1 v) (g2 v) in
      let c2 = merge_and_unify compare [Cpred(pred1)] c1 in
        [Cimp([Cpred(pred2)], c2)]
  | (RTbfun(f1,g1), RTbfun(f2,g2)) ->
      let v = new_var TBool in
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
        List.map (function Cimp(c3,[Cterm t]) -> Cimp(c3@[Cterm({desc=Not t;typ=TBool})], [Cfalse]) | ac -> ac) c
  | Cfalse -> [ac]


let get_pid c = List.map
  (function Cpred(Pred(pid, _)) -> pid | _ -> assert false)
  (List.filter (function Cpred(_) -> true | _ -> false) c)

let save_as_dot filename cs =
  let es = Utilities.uniq (List.concat (List.map
    (fun ac ->
      match ac with
        Cimp(c1, c2) ->
          let lhs = get_pid c1 in
          (match c2 with
               [Cfalse] -> List.map (fun pid -> "P" ^ string_of_int pid, "bot", "") lhs
             | [Cpred(Pred(pid2, _))] -> List.map (fun pid1 -> "P" ^ string_of_int pid1, "P" ^ string_of_int pid2, "") lhs
             | _ -> print_constraint c2; assert false)
       | _ -> assert false)
    cs))
  in
  let vs = Utilities.uniq (List.concat (List.map (fun (x, y, _) -> [x, y]) es)) in
  Utilities.save_as_dot filename vs es
let save_as_dot _ _ = ()

let rec term_of c =
  List.fold_left 
  (fun t -> fun ac ->
     {desc=BinOp(And, t, term_of_ac ac);typ=TBool})
   true_term c
and terms_of c =
  List.map term_of_ac c
and term_of_ac ac =
  match ac with
    Cpred _ -> assert false
  | Csub(rty1,rty2) -> assert false
  | Cterm t -> Trans.canonize ((*simplify*) t)
  | Cimp(c1,c2) ->
      let t1 = term_of c1 in
      let t2 = term_of c2 in
      {desc=BinOp(Or, {desc=Not t1;typ=TBool}, t2);typ=TBool}
  | Cfalse -> false_term

let int_gen ts =
  let ints = (*List.filter (fun x -> x <> 0)*) (Utilities.rev_map_flatten get_int ts) in
  if Flag.print_refine_log then Format.printf "ints: ";
  if Flag.print_refine_log then List.iter (fun int -> Format.printf "%d," int) ints;
  if Flag.print_refine_log then Format.printf "@.";
  match ints with
    [] -> ts
  | int::ints ->
      let minint = List.fold_left (fun i1 i2 -> min i1 i2) int ints in
      let ts1, ts2 = List.partition (function {desc=BinOp(Eq, t, {desc=Int(n)})} -> minint = n | _ -> false) ts in
      match ts1 with
        [] -> ts
      | ({desc=BinOp(Eq, t, {desc=Int(n)})} as tt)::ts -> 
          tt :: List.map (subst_int n t) (ts @ ts2)
      | _ -> assert false

let interpolate ids c1 c2 =
  let ts1 = terms_of c1 in
  let get_fv' = (*if !Flag.use_nint then*) get_fv (*else get_fv2*) in
  let bv = list_diff (List.flatten (List.map get_fv' ts1)) ids in
  let tmp = {desc=Not (term_of c2);typ=TBool} in
  if not (Wrapper.checksat tmp) then
    true_term
  else
  let ts2 = [tmp] in
(*
  let ts1 = if Flag.gen_int then int_gen ts1 else ts1 in
  let ts2 = if Flag.gen_int then int_gen ts2 else ts2 in
*)
  let ts2 = List.map (subst_map (List.map (fun id -> id, new_var (Id.typ id)) bv)) ts2 in
  let t =
    try
      Wrapper.interpolation ts1 ts2
    with Wrapper.Satisfiable ->
      (*if not !Flag.use_nint
      then
        let bv = list_diff (List.flatten (List.map get_fv ts1)) ids in
        let ts2 = [subst_map (List.map (fun id -> id, new_var ()) bv) (Not (term_of c2))] in
          try
            Wrapper.interpolation ts1 ts2
          with Assert_failure _ -> raise Untypable
      else*)
      raise Untypable
  in
    (*  print_term t;*)
    if Wrapper.check [] t then true_term else t

let rec substc sub c =
  List.filter
    (fun ac ->
      match ac with
        Cterm({desc=BinOp(Eq, t1, t2);typ=TBool}) -> t1 <> t2
      | _ -> true)
    (List.map (subst_ac sub) c)
and subst_ac sub ac =
  match ac with
    Cpred(Pred(pid, terms)) -> Cpred(Pred(pid, List.map (subst_map sub) terms))
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> Cterm(Trans.eval (subst_map sub term))
  | Cimp(c1,c2) -> Cimp(substc sub c1, substc sub c2)
  | Cfalse -> Cfalse

let rec subst_sol pids sol c =
  List.map (subst_sol_ac pids sol) c
and subst_sol_ac pids sol ac =
  match ac with
    Cpred(Pred(pid, terms)) ->
      if List.mem pid pids then
        Cfalse
      else
        (try
          let (ids, t) = List.assoc pid sol in
          Cterm(subst_map (List.combine ids terms) t)
        with Not_found ->
          Cterm(true_term))
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> Cterm(term)
  | Cimp(c1,c2) -> Cimp(subst_sol pids sol c1, subst_sol pids sol c2)
  | Cfalse -> Cfalse

let rec subst_sol_ sol c =
  List.map (subst_sol_ac_ sol) c
and subst_sol_ac_ sol ac =
  match ac with
    Cpred(Pred(pid, terms)) ->
      (try
        let (ids, t) = List.assoc pid sol in
        Cterm(subst_map (List.combine ids terms) t)
      with Not_found ->
        Cpred(Pred(pid, terms)))
  | Csub(rty1,rty2) -> assert false
  | Cterm(term) -> Cterm(term)
  | Cimp(c1,c2) -> Cimp(subst_sol_ sol c1, subst_sol_ sol c2)
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
        let c11, c12 = List.partition (fun ac -> Utilities.inter (fv_ac ac) (fv1 @ fv2) <> []) c1 in
        if c11 = [] then
          c2
        else
          aux c12 (c11 @ c2)
      in
      Cimp(aux c [], [Cpred(Pred(pid, terms))])
   | Cimp(c, [Cfalse]) -> ac
   | _ -> (print_constraint [ac]; assert false)

let get_lb pid terms lbs =
  try
    List.assoc pid lbs
  with Not_found ->
    List.map (fun _ -> Id.new_int ()) terms,
    [Cfalse]


let get_lhs c =
  List.flatten
  (List.map
    (function Cimp(c', _) ->
      List.map
       (function Cpred(Pred(pid, _)) -> pid | _ -> assert false)
       (List.filter (function Cpred(_) -> true | _ -> false) c')
    | _ -> print_constraint c; print_string2 "\n"; assert false)
    c)


(*
let eqs ids terms =
  let xs = List.combine ids terms in
  let xs1, xs2 = List.partition (fun (_, term) -> match term with Var(id) -> true | _ -> false) xs in
    List.map (fun (id, Var(id')) -> id', Var(id)) xs1,
  List.map (fun (id, term) -> Cterm(BinOp(Eq, Var(id), term))) xs 
*)

let rec subst_ac lbs ac =
  match ac with
      Cpred(Pred(pid, terms)) -> begin
        try
          let cond, eqs, terms' = List.assoc pid lbs in
          let ts = List.map2 (fun t1 t2 -> Wrapper.simplify_bool_exp false {desc=BinOp(Eq, t1, t2);typ=TBool}) terms terms' in
(*
          let ts_cond = if Flag.gen_int then int_gen (ts @ cond) else ts @ cond in
          ts_cond @ (List.map (fun (t1, t2) -> BinOp(Eq, t1, t2)) eqs)
*)
          let ts = if Flag.gen_int then int_gen ts else ts in
          cond @ (List.map (fun (t1, t2) -> {desc=BinOp(Eq, t1, t2);typ=TBool}) eqs) @ ts
        with Not_found ->
          [false_term]
      end
(*
      Cpred(Pred(pid, terms)) -> begin
        let ids = List.map (function Var(id) -> id | _ -> let Var(id) = new_var () in id) terms in
        let eqs = List.concat (List.map2 (fun id term -> if Var(id) = term then [] else [Var(id), term]) ids terms) in
        try
          let cond, eqs', terms' = List.assoc pid lbs in
          and_list (cond @ (List.map (fun (t1, t2) -> BinOp(Eq, t1, t2)) (eqs @ eqs')) @ (List.map2 (fun id term -> BinOp(Eq, Var(id), term)) ids terms'))
        with Not_found ->
          False
      end
        let ids, c = get_lb pid terms lbs in
        let fv = Utilities.uniq (Utilities.diff (fv c) ids) in
          substc (List.combine ids terms) (substc (List.map (fun id -> id, new_var ()) fv) c)
            (*if pos then [Cimp(eqs ids terms, c)] else (eqs ids terms) @ c*)
*)
    | Csub(rty1,rty2) -> assert false
    | Cterm t -> [t]
    | Cimp(c1,c2) -> assert false
                   (*imply (and_list (Utilities.uniq (List.map (fun ac -> subst_ac lbs ac) c1)))
                           (and_list (Utilities.uniq (List.map (fun ac -> subst_ac lbs ac) c2)))*)
    | Cfalse -> [false_term]

(*let subst_constr lbs c = Utilities.uniq (List.concat (List.map (fun ac -> subst_ac lbs ac) c))*)

let rec compute_lbs c lbs =
  let c1, c2 = List.partition
    (function Cimp(c1', [Cpred _]) ->
       List.for_all (function Cpred(Pred(pid, _)) -> List.mem_assoc pid lbs | _ -> true) c1'
       | Cimp _ -> false
       | _ -> assert false) c
  in
    if c1 = [] then
      lbs
    else
      let compute_lb = function
          Cimp(cl, [Cpred(Pred(pid, terms))]) ->
            let c1, c2 = List.partition (function Cpred(_) -> true | _ -> false) cl in
            let conds, eqss = List.split (List.map
                                            (function Cpred(Pred(pid', terms')) ->
                                               let cond, eqs, terms'' = List.assoc pid' lbs in
                                                 cond, eqs @ (List.combine terms' terms'')
                                               | _ -> assert false)
                                            c1)
            in
            let eqs = Utilities.uniq (List.filter (fun (t1, t2) -> t1 <> t2) (List.concat eqss)) in
              (if Flag.debug then
                 (*id,t1 in eqs and id,t2 in eqs => t1=t2*)
                 let tmp = Utilities.classify (fun (t11, t12) (t21, t22) -> t11 = t21) eqs in
                   List.iter (fun l ->
                                let tmp = Utilities.uniq (List.map snd l) in
                                  if List.length tmp <> 1 then
                                    let _ = List.iter (fun t -> Format.printf "%a@." Syntax.pp_print_term t) tmp in
                                      assert false)
                     tmp);
              let eqs1, eqs2 = List.partition (function ({desc=Var(_)}, _) -> true | _ -> false) eqs in
              let sub = List.map (function ({desc=Var(id)}, t) -> id, t | _ -> assert false) eqs1 in
              let cond = (List.map (function Cterm(t) -> subst_map sub t | _ -> assert false) c2) @
                (List.concat conds) in
              let cond = Utilities.uniq (List.filter (fun t -> t.desc <> True) (List.map (fun t -> Wrapper.simplify_bool_exp true t) cond)) in
              let eqs2 = List.map (fun (t1, t2) -> subst_map sub t1, t2) eqs2 in
              let eqs2 = List.map (fun (t1, t2) -> Wrapper.simplify_exp t1, Wrapper.simplify_exp t2) eqs2 in
              let eqs2 = List.filter (fun (t1, t2) -> t1 <> t2) eqs2 in
              let terms = List.map (subst_map sub) terms in
              let terms = List.map Wrapper.simplify_exp terms in 
                pid, (cond, eqs2, terms)
                  (*
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
                    in
                    let sub, eqs = eqs ids terms in
                    pid, (ids, elim ids (Utilities.uniq (substc sub ((subst_constr lbs cl) @ eqs))))
                  *)
        | _ -> assert false
      in
        compute_lbs c2 (lbs @ (List.map compute_lb c1))

let rec solve_aux' lbs ac ubs nubs sol = function
    [] -> sol
  | (Cpred(Pred(pid', terms')))::c ->
      let ids' = List.fold_left
        (fun ids -> function
             {desc=Var(id)} when not (List.mem id ids) -> ids @ [id]
           | t ->
               let id = {(new_id ()) with Id.typ = t.typ} in ids @ [id])
        [] terms'
      in
      let _ = if Flag.debug then assert (List.length ids' = List.length (Utilities.uniq ids')) in
      let eqs = List.concat (List.map2 (fun id term -> if Var(id) = term.desc then [] else [{desc=Var(id);typ=Id.typ id}, term]) ids' terms') in
        (if not !pr_flag && Flag.debug && Flag.print_refine_log then
           begin
             print_string2 "eqs: ";
             List.iter (fun (t1, t2) -> print_term {desc=BinOp(Eq, t1, t2);typ=TBool}; print_string2 ", ") eqs;
             print_string2 "\n"
           end);
        let (_lbs:typed_term list) =
          try
            let cond, eqs', terms = List.assoc pid' lbs in
            let ts = List.map2 (fun id t -> {desc=BinOp(Eq, {desc=Var(id);typ=Id.typ id}, t);typ=TBool}) ids' terms in
            let ts = cond @ (List.map (fun (t1, t2) -> {desc=BinOp(Eq, t1, t2);typ=TBool}) eqs') @ ts in
              (* this slows down verification of mult.ml, zip_unzip.ml, zip_map.ml but necessary for zip_map.ml?
                 normalization remedies the above problem?
                 let ts = if Flag.gen_int then int_gen ts else ts in
              *)
              ts
          with Not_found ->
            [false_term]
        in

        let dubs = List.map
          (fun ac ->
             let ts = Utilities.uniq (subst_ac lbs ac) in
               (*without this recursive.ml could not be verified*)
               and_list ts)
          c
        in
        let nubs' = Utilities.uniq ((List.map (fun (t1, t2) -> {desc=BinOp(Eq, t1, t2);typ=TBool}) eqs) @ nubs) in
          (*
            let ids', lb = get_lb pid' terms' lbs in
            let sub, eqs = eqs ids' terms' in
            let ub = substc sub [Cimp(eqs @ (subst_constr lbs c), cub)] in
          *)
        let _ =
          if not !pr_flag && Flag.debug && Flag.print_refine_log then
            begin
              print_string2 "solving ";
              print_pname pid';
              print_string2 "(";
              print_terms terms';
              print_string2 ")";
              print_string2 ":\n";
              print_constraint [ac];
              print_string2 "\nids:";
              print_terms (List.map make_var ids');
              print_string2 "\n";
            end
        in
        let t =
          (*if not !Flag.split_free_var then*)
          let rename_nint t =
            subst_map (List.map (fun id -> id, new_var (TInt[])) (get_nint t)) t
          in
            try
              let aux t ts =
                match t.desc with
                    (BinOp(Eq, t, {desc=Var(id)}))
                  | (BinOp(Eq, t, {desc=NInt(id)})) ->
                      List.mem id (ids' @ Utilities.rev_map_flatten get_fv2 (t::ts))
                  | _ -> true
              in
              let _lbs = Utilities.filterwo aux _lbs in
              let lb = [Cterm(and_list _lbs)] in
              let dubs = List.map rename_nint dubs in
              let nubs' = List.map rename_nint nubs' in
              let ub = [Cimp(List.map (fun t -> Cterm(t)) (dubs @ nubs'), List.map (fun t -> Cterm(t)) ubs)] in
              let ti = interpolate ids' lb ub in
              let _ =
                if not !pr_flag && Flag.debug && Flag.print_refine_log then begin
                  List.iter2
                    (fun t c ->
                       match c with Cpred(Pred(pid, terms)) ->
                         print_pname pid;
                         print_string2 "(";
                         print_terms terms;
                         print_string2 ")";
                         print_string2 ":\n";
                         print_constraint [Cterm(t)];
                         print_string2 "\n"
                         | _ -> assert false)
                    dubs c;
                  print_string2 "lb: ";
                  print_constraint lb;
                  print_string2 "\n";
                  print_string2 "ub: ";
                  print_constraint ub;
                  print_string2 "\nsolution1: ";
                  print_term ti;
                  print_string2 "\n\n"
                end
              in
                ti
            with Untypable -> raise Untypable(*;
              (*try
                                               let ub = [Cterm(rename_nint (imply (and_list dubs) cub))] in
                                               interpolate ids' lb ub
                                               with Untypable ->*)
                                               let lb = [Cterm(and_list _lbs)] in
                                               let ub = [Cimp(List.map (fun t -> Cterm(t)) (dubs @ nubs'), List.map (fun t -> Cterm(t)) ubs)] in
                                               let ti = interpolate ids' lb ub in
                                               let _ =
                                               if Flag.debug(* && Flag.print_interpolant*) then begin
                                               List.iter2
                                               (fun t c ->
                                               match c with
                                               Cpred(Pred(pid, terms)) ->
                                               print_pname pid;
                                               print_string2 "(";
                                               print_terms terms;
                                               print_string2 ")";
                                               print_string2 ":\n";
                                               print_constraint [Cterm(t)];
                                               print_string2 "\n"
                                               | _ -> assert false)
                                               dubs c;
                                               print_string2 "lb: ";
                                               print_constraint lb;
                                               print_string2 "\n";
                                               print_string2 "ub: ";
                                               print_constraint ub;
                                               print_string2 "\nsolution2: ";
                                               print_term ti;
                                               print_string2 "\n\n"
                                               end
                                               in
                                               ti
              (*else
                                               try
                                               let tmp = !Flag.use_nint in
                                               let _ = Flag.use_nint := true in
                                               let t1 = interpolate [] lb (substc (List.map (fun id -> id, new_var ()) ids') [Cimp(lb, ub)]) in
                                               let _ = Flag.use_nint := tmp in
                                               let _ = if Flag.debug && t1 <> True then
                                               let _ = print_string2 "hoge: " in
                                               let _ = print_term t1 in
                                               print_string2 "\n"
                                               in
                                               let t2 = interpolate ids' lb [Cimp([Cterm(t1)], ub)] in
                                               BinOp(And, t1, t2)
                                               with Untypable ->
                                               interpolate ids' lb ub*)*)
        in
        let sol = (pid', (ids', t))::sol in
          (*
            solve_aux' (imply (and_list (t::(List.map2 (fun id term -> BinOp(Eq, Var(id), term)) ids' terms'))) cub) sol c
          *)
        let nubs = Utilities.uniq ((subst_map (List.combine ids' terms') t)::nubs) in
          solve_aux' lbs ac ubs nubs sol c
  | _ -> assert false

let rec merge_solution sol =
  match sol with
      [] -> []
    | (pid, (ids, t))::sol' ->
        let sol1, sol2 = List.partition (fun (pid', _) -> pid = pid') sol' in
        let t = List.fold_left
          (fun t (_, (ids', t')) ->
(*
            let t' = imply (and_list (List.map2 (fun id1 id2 -> BinOp(Eq, Var(id1), Var(id2))) ids' ids)) t' in
*)
(**)
            let t' = subst_map (List.combine ids' (List.map make_var ids)) t' in
(**)
            (*if Wrapper.equiv [] t (BinOp(And, t, t')) then t else*) {desc=BinOp(And, t, t');typ=TBool})
          t sol1
        in
          (pid, (ids, t))::(merge_solution sol2)

let rec solve_aux lbs c solution =
  let lhs = get_lhs c in
  let c1, c2 = List.partition
    (fun ac -> match ac with
         Cimp(_, c') ->
           (match c' with
                [Cfalse] -> true
              | [Cpred(Pred(pid, _))] -> (*List.mem_assoc pid solution &&*) not (List.mem pid lhs)
              | _ -> assert false)
       | _ -> assert false)
    c
  in
    (*
      let _ = print_string2 "\nhoge: "; print_constraint c1 in
    *)
    if c1 = [] then
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
                let ubs =
                  match cc with
                      Cfalse -> [false_term]
                    | Cpred(Pred(pid, terms)) ->
                        let ts = List.filter (fun (pid', _) -> pid = pid') solution in
                        List.map (fun (_, (ids, t)) -> subst_map (List.combine ids terms) t) ts
                    | _ -> assert false
                in
                let c1, c2 = List.partition (function Cpred(_) -> true | _ -> false) c in
                (if List.length c1 = 1 && c2 = [] then
                  (* ToDo: optimization *)
                  ()
                  (*let [Cpred(Pred(pid, terms))] = c1 in
                            print_pname pid;
                            print_string2 "(";
                            print_terms terms;
                            print_string2 ")";
                            print_string2 ":\n"*));
                let nubs = List.map (function (Cterm(t)) -> t | c -> print_constraint [c]; assert false) c2 in
                let tmp = imply (and_list nubs) (and_list ubs) in
                if Wrapper.equiv [] tmp true_term then
                  []
                else if c1 = [] && Wrapper.equiv [] tmp false_term then
                  raise Untypable
                else
                  solve_aux' lbs ac ubs nubs [] c1
            | _ -> print_ac ac; assert false)
          c1))
      in
        solve_aux lbs c2 solution'

let solve_constr c =
  let lbs = compute_lbs c [] in

  let _ = if not !pr_flag && Flag.debug && Flag.print_lower_bound then
    begin
      print_string2 "\nLower bounds:\n";
      List.iter
        (fun (pid, (cond, eqs, terms)) ->
           print_pname pid;
           print_string2 "(";
           print_terms terms;
           print_string2 ") = ";
           print_terms (List.map (fun (t1, t2) -> {desc=BinOp(Eq, t1, t2);typ=TBool}) eqs);
           print_string2 ": ";
           print_terms cond;
           print_string2 "\n")
        lbs;
      print_string2 "@."
    end
  in
  let sol = solve_aux lbs c [] in
    List.map (fun (pid,(ids,t)) -> pid, (ids, Wrapper.simplify_bool_exp true t)) sol

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
        if List.exists (fun x -> List.exists (Id.same x) fv) (fv_ac ac)
        then Cimp(Cterm(pred)::c1, c2)
        else ac
    | _ -> assert false
  in
    List.map aux c

let filter_backward c =
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
  let c' = aux c1 c2 in
  (if not !pr_flag && Flag.debug && Flag.print_refine_log then Format.printf "filter_backward: %d -> %d@." (List.length c) (List.length c'));
  c'

let filter_forward c =
  let lbs = compute_lbs c [] in
  let pids = ref [] in
  let rec aux c1 c2 =
    let rhs = List.concat (List.map (function Cimp(_, [Cpred(Pred(pid, _))]) -> [pid] | _ -> []) c1) in
    let c21, c22 = List.partition
      (fun ac -> match ac with
           Cimp(c', _) ->
             List.for_all (fun pid -> List.mem pid rhs) (get_pid c')
         | _ -> assert false)
      c2
    in
    if c21 = [] then
      let pids = (List.concat (List.map (function Cimp(_, [Cpred(Pred(pid, _))]) -> [pid] | _ -> []) c22)) @ !pids in
      let _ = if Flag.print_refine_log then List.iter (fun pid -> Format.printf "P%d has a solution \"false\"@." pid) (List.rev pids) in
      c1, pids
    else
      aux (c1 @ (List.map
        (function Cimp(c, [Cpred(Pred(pid, terms))]) ->
          let cond = 
            List.fold_left 
              (fun t1 t2 -> {desc=BinOp(And, t1, t2);typ=TBool})
              true_term (let cond, _ , _ = List.assoc pid lbs in cond)
          in
(*
          let cond = term_of (snd (List.assoc pid lbs)) in
*)
          if Wrapper.checksat cond then
            Cimp(c, [Cpred(Pred(pid, terms))])
          else
(*
let _ = print_term cond; print_string2 "\n" in
*)
            let _ = pids := pid::!pids in Cimp(c, [Cfalse])
        | ac -> ac) c21)) c22
  in
  let c', pids = aux [] c in
  (if not !pr_flag && Flag.debug && Flag.print_refine_log then Format.printf "filter_forward: %d -> %d@." (List.length c) (List.length c'));
  c', pids


let rec filter2 c =
  let aux = function
      Cimp(c1,[ac]) -> c1, ac
    | _ -> assert false
  in
  let c' = List.map aux c in
  let rec loop (c:(ac list*ac) list) : (ac list*ac) list =
    let aux (c,ac) =
      if c = []
      then
        match ac with
            Cpred(Pred(pid,_)) -> [pid]
          | ac -> assert false
      else []
    in
    let pids = rev_flatten_map aux c in
      if pids = []
      then c
      else
        let aux = function
            Cpred(Pred(pid,_)) -> not (List.mem pid pids)
          | _ -> true
        in
        let c' = List.filter (fun (_,ac) -> aux ac) c in
        let c'' = List.map (fun (c,ac) -> List.filter aux c, ac) c' in
          loop c''
  in
    List.map (fun (c,ac) -> Cimp(c,[ac])) (loop c')



let rec get_sol typ1 typ2 =
  match typ1, typ2 with
      TUnit, RTunit _ -> []
    | TAbsBool, RTbool _ -> []
    | TBool, RTbool _ -> []
    | TInt _, _ -> assert false
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun({Id.typ=TInt ps} as x,rtyp1), RTifun(pred, rtyp2) ->
        get_sol rtyp1 (rtyp2 (make_var x))
    | TFun({Id.typ=TRInt p},rtyp1), RTifun(pred, rtyp2) ->
        let Pred(pid, terms) = pred (make_var abst_var) in
        [pid, (List.map (function {desc=Var(id)} -> id | _ -> assert false) terms, p)]
    | TFun(x,rtyp1), RTbfun(_, rtyp2) ->
        get_sol rtyp1 (rtyp2 (make_var x))
    | TFun(x,rtyp1), RTfun(typs, rtyp2) ->
        let typ = Id.typ x in
        (Utilities.rev_flatten_map (fun typ' -> get_sol typ typ') typs) @
        (get_sol rtyp1 rtyp2)
    | TUnknown, _ -> []
    | _, _ ->
        Format.printf "%a and " print_typ typ1;
        print_rty typ2;
        assert false

let test tdefs s defs traces pred = 
  branchtab := [];
  tinfomap := [];
  trace2id := [];
  current_pid := 0;

  (*
    let _ = if Flag.debug then print_string2 "\n Program: \n" in
    let _ = if Flag.debug then print_defs defs in
    let _ = if Flag.debug then print_string2 "\n" in
  *)
  let ti = new_tinfo() in
  let _ = register_tinfo s ti in
  let et = MyVar(s, ti) in
  let counter = incr_counter () in
  let _ = process_head et (ETunit(counter)) in
    (eval_term et defs traces counter;
     let te = mk_atenv() in
       (*
         let _ = if Flag.debug && Flag.print_constraints then print_string2 "ML Types:\n" in
         let _ = if Flag.debug && Flag.print_constraints then print_atenv te in
       *)
     let rte = atenv2rtenv te in
     let _ = if !pr_flag || Flag.debug && Flag.print_constraints then print_string2 "Type templates:\n" in
     let _ = if !pr_flag || Flag.debug && Flag.print_constraints then print_rtenv rte in
     let c = gen_constr defs rte in

         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\nConstraints:\n" in
         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_constraint c in
         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\n" in

     let c' = reduce_constr c in

         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\nReduced constraints:\n" in
         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_constraint c' in
         let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\n" in

       (**)
     let sol = Utilities.rev_flatten_map
       (fun (x, rtys) ->
          try
            let _, ty = List.find (fun (y, ty) -> Id.same x y) tdefs in
              Utilities.rev_flatten_map (get_sol ty) rtys
          with Not_found -> [])
       rte
     in
     let _ = if not !pr_flag then List.iter
       (fun (pid, (ids, t)) ->
          print_pname pid;
          print_string2 "(";
          print_terms (List.map make_var ids);
          print_string2 ") = ";
          print_term t;
          print_string2 "\n")
       sol
     in
     let c' = subst_sol_ sol c' in
       (**)
     let c'' = normalize_constr c' in
       (**)
     let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\nNormalized constraints:\n" in
     let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_constraint c'' in
     let _ = if not !pr_flag && Flag.debug && Flag.print_constraints then print_string2 "\n" in
       (**)
     let _ = if not !pr_flag && Flag.debug then save_as_dot ("constraints" ^ (string_of_int !Flag.cegar_loop) ^ ".dot") c'' in

     let c'' =
       if not !cgen_flag then
         match pred with
             None -> assert false
           | Some pred -> add_pred pred c''
       else
         c''
     in

     (*
       let c''' = simplify_constr c'' in
       let _ = print_string2 "\nSimplified constraints:\n" in
       let _ = print_constraint c''' in
       let _ = print_string2 "\n" in
     *)
     let c''', pids = (if !Flag.filter_forward then filter_forward else (fun x -> x, [])) (filter_backward c'') in
     let c''' = List.rev (Utilities.uniq (filter_backward c''')) in
     let _ = if !pr_flag || Flag.debug && Flag.print_constraints then print_string2 "\nFiltered constraints:\n" in
     let _ = if !pr_flag || Flag.debug && Flag.print_constraints then print_constraint c''' in
     let _ = if !pr_flag || Flag.debug && Flag.print_constraints then print_string2 "\n" in
     let _ = if Flag.debug then save_as_dot ("constraints_filtered" ^ (string_of_int !Flag.cegar_loop) ^ ".dot") c''' in
     let sol = solve_constr c''' in



     let c_test = filter2 c''' in
     let _ = if !pr_flag then print_string2 "\nFiltered constraints2:\n" in
     let _ = if !pr_flag then print_constraint c_test in
     let _ = if !pr_flag then print_string2 "\n" in



     let _ = if !pr_flag || Flag.debug then print_string2 "\nSolutions::\n" in
     let _ =
       if Flag.debug
       then
         List.iter
           (fun (pid, (ids, t)) ->
              print_pname pid;
              print_string2 "(";
              print_terms (List.map make_var ids);
              print_string2 ") = ";
              print_term (Trans.merge_geq_leq (Trans.normalize_bool_exp t));
              print_string2 "\n")
           sol in
     let _ =
       if Flag.debug then
         List.iter
           (fun ac ->
              let ac' = subst_sol_ac pids sol ac in
              let () = ignore ac' in
              let t = term_of_ac (subst_sol_ac pids sol ac) in
              if Wrapper.check [] t
              then ()
              else begin
                print_string2 "wrong solution for:";
                print_ac ac;
                print_string2 "\n";
                assert false
              end)
           c''
     in
     let _ = if Flag.debug then print_string2 "\n\n" in
       rte, sol)






module CS = CEGAR_syntax
module CT = CEGAR_type
module CP = CEGAR_print
module CU = CEGAR_util

let make_temp = List.fold_right (function (CS.Event s) -> fun t -> CS.App(CS.Const (CS.Temp s), t) | _ -> assert false)

let to_if_exp (defs:CS.fun_def list) : CS.fun_def list =
  let merge = function
      [f,xs,t1,e,t2] -> assert (t1 = CS.Const CS.True); f, xs, t1, e, t2
    | [f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22] when f1=f2 && xs1=xs2 && t11=CS.make_not t21 ->
        f1, xs1, CS.Const CS.True, [], CS.make_if t21 (make_temp e2 t22) (make_temp e1 t12)
    | [f1,xs1,t11,e1,t12; f2,xs2,t21,e2,t22] when f1=f2 && xs1=xs2 && CS.make_not t11=t21 ->
        f1, xs1, CS.Const CS.True, [], CS.make_if t11 (make_temp e1 t12) (make_temp e2 t22)
    | _ -> assert false
  in
  let rec aux = function
      [] -> []
    | (f,xs,t1,e,t2)::defs ->
        let defs1,defs2 = List.partition (fun (g,_,_,_,_) -> f = g) defs in
        let def' = merge ((f,xs,t1,e,t2)::defs1) in
          def' :: aux defs2
  in
    aux defs

let rec trans_typ = function
    CT.TBase(CT.TUnit,_) -> TUnit
  | CT.TBase(CT.TInt,_) -> TInt[]
  | CT.TBase(CT.TBool,_) -> TBool
  | CT.TBase _ -> assert false
  | CT.TFun(typ1,typ2) ->
      let typ2 = typ2 (CS.Const CS.Unit) in
        TFun(Id.new_var "x" (trans_typ typ1), trans_typ typ2)
  | CT.TApp _ -> assert false
  | CT.TAbs _ -> assert false

let trans_var env x =
  let name,id = CS.decomp_id x in
    Id.make id name (List.assoc x env)

let rec trans_term env = function
    CS.Const CS.Unit -> unit_term
  | CS.Const CS.True -> true_term
  | CS.Const CS.False -> false_term
  | CS.Const (CS.Int n) -> make_int n
  | CS.Const CS.Bottom -> make_bottom TUnknown
  | CS.Const (CS.Temp s) -> make_event s
  | CS.Var x -> make_var (trans_var env x)
  | CS.App(CS.Const CS.RandInt, t) -> make_app (trans_term env t) [make_app randint_term [unit_term]]
  | CS.App(CS.App(CS.App(CS.Const CS.If, t1), t2), t3) -> make_if (trans_term env t1) (trans_term env t2) (trans_term env t3)
  | CS.App(CS.App(CS.Const CS.And, t1), t2) -> make_and (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Or, t1), t2) -> make_or (trans_term env t1) (trans_term env t2)
  | CS.App(CS.Const CS.Not, t) -> make_not (trans_term env t)
  | CS.App(CS.App(CS.Const CS.Lt, t1), t2) -> make_lt (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Gt, t1), t2) -> make_gt (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Leq, t1), t2) -> make_leq (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Geq, t1), t2) -> make_geq (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const (CS.EqUnit|CS.EqInt|CS.EqBool), t1), t2) -> make_eq (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Add, t1), t2) -> make_add (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Sub, t1), t2) -> make_sub (trans_term env t1) (trans_term env t2)
  | CS.App(CS.App(CS.Const CS.Mul, t1), t2) -> make_mul (trans_term env t1) (trans_term env t2)
  | CS.App(t1,t2)  -> make_app (trans_term env t1) [trans_term env t2]
  | CS.Let _ -> assert false
  | t -> Format.printf "trans_term: %a@." CP.print_term t; assert false

let trans_env env = List.map (fun (x,typ) -> x, trans_typ typ) env

let trans_def env_cegar env (f,xs,t1,e,t2) =
  let env' = trans_env (CU.get_arg_env (List.assoc f env_cegar) xs) in
  let env'' = env' @@ env in
  let t2' =
    match e with
        [] -> trans_term env'' t2
      | [CS.Event "fail"] -> make_app fail_term [unit_term]
      | _ -> assert false
  in
    trans_var env'' f, (List.map (trans_var env'') xs, t2')

let trans ((env,defs,main):CS.prog) =
  let defs = to_if_exp defs in
  let env' = trans_env env in
    List.map (trans_def env env') defs, trans_var env' main




let rec add_preds_typ sol typ1 typ2 =
  (*
    Format.printf "%a and " (print_typ ML) typ1;
    Infer.print_rty typ2;
    Format.printf "@.";
  *)
  match typ1, typ2 with
      TUnit, RTunit _ -> TUnit
    | TAbsBool, RTbool _ -> TAbsBool
    | TBool, RTbool _ -> TBool
    | TInt ps, RTint _ -> TInt ps (***)
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun({Id.typ=TInt ps} as x,rtyp1), RTifun(pred, rtyp2) ->
        let Pred(pid, terms) = pred (make_var abst_var) in
        let typ =
          try
            let p =
              let ids, t = List.assoc pid sol in
                (*Format.printf "%a,%a,%a@." (print_term_fm ML true) t (print_ids) ids  (print_termlist ML 0 false) terms; *)
                subst_map (List.combine ids terms) t
            in
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] (make_not p)) ps ||
                  Wrapper.equiv [] p true_term || Wrapper.equiv [] p false_term
                then ps
                else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
              let p' = Trans.merge_geq_leq (Trans.normalize_bool_exp p) in
                List.fold_left aux ps [p']
            in
              TInt ps
          with Not_found -> (*assert false*)
            TInt ps
        in
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (make_var x)) in
          TFun(Id.set_typ x typ, rtyp)
    | TFun(x,rtyp1), RTbfun(_, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (make_var x)) in
          TFun(x, rtyp)
    | TFun(x,rtyp1), RTfun(typs, rtyp2) ->
        let typ = List.fold_left (add_preds_typ sol) (Id.typ x) typs in
        let rtyp = add_preds_typ sol rtyp1 rtyp2 in
          TFun(Id.set_typ x typ, rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and " print_typ typ1;
        assert false



let infer ces prog =
  let ces =  List.map (fun ce -> List.map (fun b -> LabNode b) (Feasibility.trans_ce ce prog) @ [FailNode]) ces in
  let defs,main = trans prog in
  let () =
    if false
    then
      Format.printf "%a@."
        pp_print_term (List.fold_right (fun (f,(xs,t)) t' -> make_let [f,xs,t] t') defs (make_var main))
  in
  let rte,sol = test [] main defs ces None in
  let fs = List.map fst defs in
  let aux f =
    try
      let _,rty = List.find (fun (x, _) -> Id.same f x) rte in
      let typ = List.fold_left (add_preds_typ sol) (Id.typ f) rty in
        if true || Flag.print_refine_log then Format.printf "%a: %a@." Id.print f print_typ typ;
        [CU.trans_var f, CU.trans_typ' typ]
    with Not_found -> []
  in
    rev_flatten_map aux fs



let infer_and_print ces prog =
  pr_flag := true;
  ignore (infer ces prog);
  pr_flag := false

let infer ces prog = infer ces prog

