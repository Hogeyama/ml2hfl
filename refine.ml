


open Util
open Syntax



let new_tvar () = TVar (ref None)

let rec expand t = function
    TFun((x,typ1),typ2) ->
      expand (app2app t [Var x]) typ2
  | _ -> t

let rec conj = function
    [] -> False
  | [t] -> t
  | t::ts -> BinOp(And, t, conj ts)





let reduce_trace ps scope =
  let rec aux ps1 t ps2 =
    match t,ps2 with
        t, [] -> t::ps1
      | BinOp(Eq, Var x, Var y), p::ps ->
          if List.mem x scope || List.mem y scope
          then aux (t::ps1) p ps
          else
            let ps1' = List.map (subst x (Var y)) ps1 in
            let ps' = List.map (subst x (Var y)) ps in
              aux ps1' p ps'
      | _, p::ps -> aux (t::ps1) p ps
  in
    aux [] (List.hd ps) (List.tl ps)









(*
let rec get_scmap_typ scope = function
  | TUnit -> []
  | TAbsBool -> []
  | TBool -> []
  | TInt _ -> []
  | TVar _ -> assert false
  | TFun((x,TInt ps),typ2) ->
      let scope' = x::scope in
      let scmap2 = get_scmap_typ scope' typ2 in
        (x,scope')::scmap2
  | TFun((x,typ1),typ2) ->
      let scmap1 = get_scmap_typ scope typ1 in
      let scmap2 = get_scmap_typ scope typ2 in
        scmap1@@scmap2
  | TUnknown -> []


let rec get_scmap scope = function
    Unit -> []
  | True -> []
  | False -> []
  | Unknown -> []
  | Int n -> []
  | NInt x -> []
  | Var x -> []
  | App(t, ts) ->
      List.flatten (List.map (get_scmap scope) (t::ts))
  | If(t1, t2, t3, _) ->
      get_scmap scope t1 @@ get_scmap scope t2 @@ get_scmap scope t3
  | Branch(t1, t2) -> get_scmap scope t1 @@ get_scmap scope t2
  | Let(f, xs, t1, t2) ->
      let xs' = List.filter (fun x -> Wrapper.isTInt x.typ) xs in
      let scope' = xs'@@scope in
      let scmap1 = get_scmap scope' t1 in
      let scmap2 = get_scmap scope t2 in
        scmap1 @@ scmap2 @@ get_scmap_typ scope f.typ
  | Letrec(f, xs, t1, t2) ->
      let xs' = List.filter (fun x -> Wrapper.isTInt x.typ) xs in
      let scope' = xs'@@scope in
      let scmap1 = get_scmap scope' t1 in
      let scmap2 = get_scmap scope t2 in
        scmap1 @@ scmap2 @@ get_scmap_typ scope f.typ
  | BinOp(op, t1, t2) -> get_scmap scope t1 @@ get_scmap scope t2
  | Not t -> get_scmap scope t
  | Fail -> []
  | Fun _ -> assert false
  | Label(_,t) -> get_scmap scope t

let get_scmap = get_scmap []




let rec replace_vars_typ map = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt _ -> assert false
  | TVar _ -> assert false
  | TFun((x,TInt _),typ2) ->
      let typ2' = replace_vars_typ map typ2 in
      let x' = assoc_exn x map x in
        TFun((x', x'.typ), typ2')
  | TFun((x,typ1),typ2) ->
      let typ1' = replace_vars_typ map typ1 in
      let typ2' = replace_vars_typ map typ2 in
      let x' = {x with typ = typ1'} in
        TFun((x',typ1'), typ2')
  | TUnknown -> TUnknown

let rec replace_vars map = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var x
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = replace_vars map t in
      let ts' = List.map (replace_vars map) ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = replace_vars map t1 in
      let t2' = replace_vars map t2 in
      let t3' = replace_vars map t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = replace_vars map t1 in
      let t2' = replace_vars map t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let typ = replace_vars_typ map f.typ in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = replace_vars map t1 in
      let t2' = replace_vars map t2 in
      let t1'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1' in
      let t2'' = subst f (Var f') t2' in
        Let(f', xs', t1'', t2'')
  | Letrec(f, xs, t1, t2) ->
      let typ = replace_vars_typ map f.typ in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = replace_vars map t1 in
      let t2' = replace_vars map t2 in
      let t1'' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1' in
      let t2'' = subst f (Var f') t2' in
        Letrec(f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = replace_vars map t1 in
      let t2' = replace_vars map t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = replace_vars map t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false





let elim_typ_preds_id x =
  let typ =
    match x.typ with
        TInt _ -> TInt []
      | TUnknown -> TUnknown
      | _ -> assert false
  in
    {x with typ = typ}
let rec elim_typ_preds = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt (elim_typ_preds_id x)
  | Var x -> Var (elim_typ_preds_id x)
  | BinOp(op, t1, t2) ->
      let t1' = elim_typ_preds t1 in
      let t2' = elim_typ_preds t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = elim_typ_preds t in
        Not t'
  | _ -> assert false




let rec split_app = function
    Var f -> Var f,[]
  | App(f,ts) -> f,ts
  | Let(_,_,_,t) -> split_app t
  | Letrec(_,_,_,t) -> split_app t
  | _ -> assert false


let rec get_head = function
    Var f -> Var f
  | App(f,_) -> get_head f
  | Fun(_,t) -> get_head t
  | Fail -> Fail
  | Let(_,_,_,t) -> get_head t
  | Letrec(_,_,_,t) -> get_head t
  | _ -> assert false
let get_head' t =
  match get_head t with
      Var f -> f
    | _ -> assert false
*)

(*
let rec infer_template env trace tenv =
  match trace with
      [] -> tenv
    | (_,t,_)::trace' ->
        let f,ts = split_app t in
          match f with
              Fail -> infer_template env trace' tenv
            | Var f ->
                let aux x t =
                  match x.typ with
                      TInt _ -> new_tvar ()
                    | TFun _ -> (try List.assoc (fst (split_app t)) tenv with Not_found -> TUnknown)
                    | _ -> x.typ
                in
                let dummy = {id= -1; origin="dummy"; typ=TUnknown} in
                let typ = List.fold_right (fun typ1 typ2 -> TFun((dummy,typ1),typ2)) (List.map2 aux (get_args f.typ) ts) TUnit in
                  infer_template env trace' ((Var f, typ)::tenv)
            | _ -> assert false
let infer_template env trace = infer_template env trace []



let rec get_trace ce env trace defs t n =
  match t,ce with
      Var x, _ -> get_trace ce env trace defs (App(Var x, [])) n
    | App(Fail, _), [FailNode,0] -> trace
    | App(Var x, ts), _ ->
        let _,t' = try List.assoc x defs with Not_found -> List.assoc x env in
        let xs = get_args x.typ in
        let xs' = List.map (fun x -> {(new_var' x.origin) with typ = x.typ}) xs in
        let t'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t' in
        let t''' = List.fold_right2 subst xs' ts t'' in
        let env' =
          let aux env x t =
            match x.typ with
                TFun _ -> (x, ([], expand t x.typ))::env
              | _ -> env
          in
            List.fold_left2 aux env xs' ts
        in
          get_trace ce env' (([],t'',t''',n)::trace) defs t'' (n+1)
    | If(t1, t2, _, _), (LabNode true, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,If(_,t,_,_),n)::trace' -> (t1::constr,t2,t,n)::trace'
            | _ -> assert false
        in
          get_trace ce' env trace' defs t2 n
    | If(t1, _, t3, _), (LabNode false, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,If(_,_,t,_),n)::trace' -> (Not t1::constr,t3,t,n)::trace'
            | _ -> assert false
        in
          get_trace ce' env trace' defs t3 n
    | _ -> assert false

let get_constr ce scmap defs t =
  let constr = get_trace ce [] [[],t,t,0] defs t 1 in
    constr

let rec get_trace ce trace defs t n =
  match t,ce with
      Var x, _ -> get_trace ce trace defs (App(Var x, [])) n
    | App(Fail, _), [FailNode,0] -> trace
    | App(Var x, ts), _ ->
        let _,t = List.assoc x defs in
        let xs = get_args x.typ in
        let t' = List.fold_right2 subst xs ts t in
          get_trace ce (([],t',t,n)::trace) defs t' (n+1)
    | If(_, t2, _, _), (LabNode true, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,If(t1,t,_,_),n)::trace' -> (t1::constr,t2,t,n)::trace'
            | _ -> assert false
        in
          get_trace ce' trace' defs t2 n
    | If(_, _, t3, _), (LabNode false, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,If(t1,_,t,_),n)::trace' -> (Not t1::constr,t3,t,n)::trace'
            | _ -> assert false
        in
          get_trace ce' trace' defs t3 n
    | _ -> assert false

let get_constr ce scmap defs t =
  let trace = get_trace ce [[],t,t,0] defs t 1 in
  let l_defs =
    let s = {(new_var' "S") with typ = TUnit} in
    let aux (prev,defs) (cond,f,t,n) = get_head f, (prev,n,cond,t)::defs in
      snd (List.fold_left aux (Var s,[]) (List.rev trace))
  in
  let map,l_defs' =
    let aux (f,n,cond,t) =
      match f with
          Var f ->
            let xs = List.filter (fun x -> match x.typ with TBool | TInt _ -> true | _ -> false) (get_argvars f.typ) in
            let xs' = List.map new_var_id xs in
            let cond' = List.map (fun c -> List.fold_left2 (fun c x x' -> subst x (Var x') c) c xs xs') cond in
              List.map2 (fun x x' -> (x,n),x') xs xs', (Var f,n,cond',t)
        | _ -> [], (f,n,cond,t)
    in
    let map,l_defs' = List.split (List.map aux l_defs) in
      List.flatten map, l_defs'
  in
  let get_args' n typ =
    let aux x =
      match x.typ with
          TBool
        | TInt _ -> assoc_exn (x,n) map x
        | _ -> x
    in
      List.map aux (get_args typ)
  in
  let () =
    let aux (f,n,_,t) =
      Format.printf "%a --%d--> %a@." (print_term_fm ML false) f n (print_term_fm ML false) t
    in
      List.iter aux l_defs
  in
  let constr =
    let check_exists g m = List.exists (fun (f,n,_,t) -> (f=Var g || get_head t=Var g) && n=m) l_defs' in
    let get_pred n x = List.map (fun y -> if x=y then Var abst_var else Var (List.assoc (y,n) map)) (assoc_exn x scmap []) in
    let aux (f,k,cond,t) =
      match f with
          Var f ->
            let xs = get_args' k f.typ in
            let env =
              let aux x =
                match x.typ with
                    TInt _ -> [Pred (f, k, x, get_pred k x)]
                  | _ -> []
              in
                rev_map_flatten aux xs
            in
            let cond' = List.map (fun t -> Cond t) cond in
            let rec gen_constr f n x t =
              let rec subtyp_constr g t x m =
                if check_exists g m
                then
                  match x.typ, t with
                      TUnit, _
                    | TBool, _ -> [[], Cond t]
                    | TInt _, _ ->
                        [[Cond (BinOp(Eq, Var abst_var, t))], Pred (g, m, x, get_pred m x)]
                    | TFun _, Var f ->
                        let xs1 = get_args' m f.typ in
                        let xs2 = get_args' m x.typ in
                        let aux (cond,constr) x1 x2 =
                          BinOp (Eq, Var x1, Var x2) :: cond, subtyp_constr g (Var x2) x1 m @ constr
                        in
                          snd (List.fold_left2 aux ([],[]) xs1 xs2)
                    | _ -> assert false
                else []
              in
                match t with
                    Unit
                  | True
                  | False
                  | Int _
                  | NInt _
                  | BinOp _
                  | Not _ -> subtyp_constr f t x n
                  | Var x ->
                      begin
                        match x.typ with
                            TFun _ -> gen_constr f n x (App(t, []))
                          | _ -> subtyp_constr f t x n
                      end
                  | App(Fail, _) -> [[], Cond False]
                  | App(Var g, ts) ->
                      if List.length (get_args g.typ) = List.length ts
                      then
                        let xs = if is_uppercase g.origin.[0] then get_args' n g.typ else get_args' k g.typ in
                          List.flatten (List.map2 (gen_constr g n) xs ts)
                      else
                        let rec aux cond ts xs m =
                          if check_exists g m
                          then
                            match xs,ts with
                                _,[] ->
                                  let typ = List.fold_right (fun x typ -> TFun((x,x.typ),typ)) xs TUnit in
                                    subtyp_constr g (Var {id=0; origin=""; typ=typ}) x n
                              | x::xs,t::ts ->
                                  let constr = List.map (fun (pre,post) -> cond@@pre,post) (gen_constr g m x t) in
                                  let cond' = Cond (BinOp(Eq, Var x, t)) :: cond in
                                    constr @@ aux cond' ts xs m
                              | _ -> assert false
                          else []
                        in
                        let xs = if is_uppercase g.origin.[0] then get_args' n g.typ else get_args' k g.typ in
                          List.flatten (mapi (fun n _ -> aux [] ts xs n) l_defs')
                  | _ -> assert false
            in
            let constr = gen_constr f (k+1) f t in
              List.map (fun (pre,post) -> env@@cond'@@pre,post) constr
        | _ -> assert false
    in
      rev_map_flatten aux (List.rev l_defs')
  in
  let () =
    let print_constr fm = function
        Cond t -> print_term_fm ML false fm t
      | Pred(f,n,x,ts) -> Format.printf "P_{%a_%d}^%a(%a)" print_id f n print_id x (print_termlist ML 0 false) ts
    in
    let print_constr_list fm = List.iter (fun c -> Format.fprintf fm "@;(%a)" print_constr c) in
      List.iter (fun (pre,post) -> Format.printf "%a => %a@." print_constr_list pre print_constr post) constr
  in
    constr,l_defs


let refine ce t =
  let map,(t',defs) = Lift.lift t in
  let map = List.filter (fun (x,_) -> match x.typ with TInt _ -> true | _ -> false) map in
  let () = Format.printf "\n\n%a\n@." (print_term_fm ML true) (List.fold_left (fun acc (f,xs,t) -> Letrec(f,xs,t,acc)) t' defs) in
  let scmap = get_scmap t in
  let defs = List.map (fun (f,xs,t) -> f,(xs,t)) defs in
  let _,l_defs = get_constr ce scmap defs t' in
  let () = ignore l_defs in
  let () = failwith "End of the program" in
  let t' = replace_vars map t in
    t',map
      *)















(*
type dag = Top of (t * t list * dag ref list) | Node of ((ident * int * ident * t list) * t list * dag ref list)

let solve_constr constr =
  let size = List.length constr in
    (*
      let constr' =
      let get_fv_lit = function
      Cond t -> get_fv t
      | Pred(_,_,_,ts) -> uniq_flatten_map get_fv ts
      in
      let subst_lit x1 t1 = function
      Cond t -> Cond(subst x1 t1 t)
      | Pred(f,n,x,ts) -> Pred(f,n,x,List.map (subst x1 t1) ts)
      in
      let aux (pre,post) =
      let fv = uniq_flatten_map get_fv_lit (post::pre) in
      let fv' = List.map new_var_id fv in
      let aux lit = List.fold_left2 (fun lit x x' -> subst_lit x (Var x') lit) lit fv fv' in
      let pre' = List.map aux pre in
      let post' = aux post in
      pre',post'
      in
      List.map aux constr
      in
      let () =
      List.iter (fun (pre,post) -> Format.printf "%a => %a@." print_constr_list pre print_constr post) constr';
      Format.printf "\n\n@."
      in
    *)
  let get_node dag =
    match !dag with
        Top _ -> assert false
      | Node x -> x
  in
  let rec make_dag rest dags =
    if rest = []
    then dags
    else
      let is_bottom (pre,_) =
        let check = function
            Cond t -> true
          | Pred(f,n,x,ts) -> not (List.exists (function _,Pred(f',n',x',_) -> f=f' && n=n' && x=x' | _ -> false) rest)
        in
          List.for_all check pre
      in
      let bot,rest' = List.partition is_bottom rest in
      let dags' =
        let aux (pre,post) =
          let cond,pred = List.partition (function Cond _ -> true | Pred _ -> false) pre in
          let cond = List.map (function Cond t -> t | _ -> assert false) cond in
          let dags =
            let aux f n x dag = match !dag with Node((f',n',x',_),_,_) -> f=f' && n=n' && x=x' | _ -> false in
              List.flatten (List.map (function Pred(f,n,x,_) -> List.filter (aux f n x) dags | _ -> assert false) pred)
          in
            match post with
                Cond t -> ref (Top (t, cond, dags))
              | Pred(f,n,x,ts) -> ref (Node((f,n,x,ts), cond, dags))
        in
          List.map aux bot
      in
        make_dag rest' (dags'@@dags)
  in
  let dags = make_dag constr [] in
  let dag = List.hd (List.filter (fun dag -> match !dag with Node _ -> false | Top _ -> true) dags) in
  let () = Format.printf "ZZZ@." in
  let lower_bound = Hashtbl.create size in
(*
  let get_lower dag =
    let get_node_fv dag = 
      match !dag with
          Top _ -> assert false
        | Node((_,_,_,ts),_,_) -> uniq_flatten_map get_fv ts
    in
    let rec aux map dag =
      match !dag with
          Top _ -> assert false
        | Node((_,_,_,ts1),ts2,dags) ->
            let fv = uniq (diff (rev_map_flatten get_fv (ts1@@ts2) @@ rev_map_flatten get_node_fv dags) (List.map fst map)) in
            let fv' = List.map new_var_id fv in
            let map' = List.combine fv fv' @@ map in
            let ts2' = List.map (fun t -> List.fold_left (fun t (x,x') -> subst x (Var x') t) t map') ts2 in
            let aux dag =
              match !dag with
                  Top _ -> assert false
                | Node((_,_,_,ts),_,_) ->
                    let fv = uniq_flatten_map get_fv ts in
                    let map'' = List.filter (fun (x,_) -> List.mem x fv) map' in
                      aux map'' dag
            in
              ts2' @@ rev_map_flatten aux dags
    in
    let fv = get_node_fv dag in
    let map = List.combine fv fv in
      aux map dag
  in
*)
  let rec get_lower2 dag =
    if Hashtbl.mem lower_bound dag
    then Hashtbl.find lower_bound dag
    else
      let aux dag =
        let (_,_,_,ts1),_,_ = get_node dag in
        let xs,lb = get_lower2 dag in
          List.map (fun t -> List.fold_left2 (fun t x t' -> subst x t' t) t xs ts1) lb
      in
      let (_,_,_,ts1),ts2,dags = get_node dag in
      let xs = List.map (function Var x -> x | _ -> assert false) ts1 in
      let fv = diff (uniq_flatten_map get_fv ts2) xs in
      let fv' = List.map new_var_id fv in
      let subst_aux t = List.fold_left2 (fun t f f' -> subst f (Var f') t) t fv fv' in
      let lb = ts2 @@ rev_map_flatten aux dags in
      let lb' = List.map subst_aux lb in
        Hashtbl.add lower_bound dag (xs,lb');
        xs, lb'
  in
  let rec solve dag =
    match !dag with
        Node _ -> []
      | Top(t,ts,dags) ->
          let () = Format.printf "XXX@." in
          let rec aux ts rest acc = function
              [] -> rest, acc
            | dag::dags ->
                let () = Format.printf "YYY@." in
                  begin
                    match get_lower2 dag with
                        _,[] -> aux ts rest acc dags
                      | _,lb ->
                          let () = Format.printf "TTT@." in
                          let ts1 = lb in
(*
                          let ts2 = Not t::ts @@ rev_map_flatten get_lower dags in
*)
                          let ts2 = Not t::ts @@ snd (get_lower2 (ref(Node((dummy_var,0,dummy_var,[]),[],dags)))) in
                          let ts2' = uniq ts2 in
                          let () = Format.printf "DDD(%d,%d)@." (List.length ts1) (List.length ts2') in
                          let p = Wrapper.interpolation ts1 ts2' in
                          let () = Format.printf "KKK@." in
                          let rest',acc' =
                            match !dag with
                                Top _ -> assert false
                              | Node((f,n,x,ts),ts',dags) ->
                                  ref (Top(p,ts',dags))::rest, ((f,n,x,ts),p)::acc
                          in
                          let () = Format.printf "UUU@." in
                            aux (p::ts) rest' acc' dags
                  end
          in
          let dags',sol' = aux ts [] [] dags in
            if dags' = []
            then sol'
            else sol' @@ rev_map_flatten solve dags'
  in
  let () = Format.printf "WWW@." in
    solve dag
*)










let rec rename_map map =
  function
      Unit -> Unit
    | True -> True
    | False -> False
    | Unknown -> Unknown
    | Int n -> Int n
    | NInt x -> NInt x
    | Var x -> Var (assoc_exn x map x)
    | Fun(x,t) -> assert false
    | App(t,ts) ->
        let t' = rename_map map t in
        let ts' = List.map (rename_map map) ts in
          App(t', ts')
    | If(t1,t2,t3,t4) ->
        let t1' = rename_map map t1 in
        let t2' = rename_map map t2 in
        let t3' = rename_map map t3 in
        let t4' = rename_map map t4 in
          If(t1', t2', t3', t4')
    | Branch(t1,t2) ->
        let t1' = rename_map map t1 in
        let t2' = rename_map map t2 in
          Branch(t1', t2')
    | Let _
    | Letrec _ -> assert false
    | BinOp(op,t1,t2) ->
        let t1' = rename_map map t1 in
        let t2' = rename_map map t2 in
          BinOp(op, t1', t2')
    | Not t ->
        let t' = rename_map map t in
          Not t'
    | Fail -> Fail
    | Label(b,t) ->
        let t' = rename_map map t in
          Label(b,t')
    | Event s -> Event s

let rec rename_typ = function
    TUnit -> [], TUnit
  | TBool -> [], TBool
  | TAbsBool -> [], TAbsBool
  | TInt _ -> [], TInt []
  | TRInt p -> [], TRInt p
  | TVar _ -> assert false
  | TFun((x,typ1),typ2) ->
      let map1,typ1' = rename_typ typ1 in
      let x' = {(new_var_id x) with typ=typ1'} in
      let map2,typ2' = rename_typ typ2 in
        (x,x')::map1@@map2, TFun((x',typ1'),typ2')
  | TUnknown -> [], TUnknown

let rename defs t =
  let aux (f,(xs,t)) =
    let map, typ = rename_typ f.typ in
    let f' = {f with typ=typ} in
    let xs' = List.map (fun x -> assoc_exn x map x) xs in
    let t' = rename_map map t in
      (f,f'), (f',(xs',t'))
  in
  let map,defs' = List.split (List.map aux defs) in
  let aux t = List.fold_left (fun t (f,f') -> subst f (Var f') t) t map in
  let defs'' = List.map (fun (f,(xs,t)) -> f,(xs,aux t)) defs' in
  let t' = aux t in
  let map' = rev_map_flatten (fun (f,f') -> List.combine (get_argvars f.typ) (get_argvars f'.typ)) map in
    map',aux,defs'',t'





(*
let rec get_branch b = function
    Let(f,xs,t1,t2) ->
      let t_g,t_b = get_branch b t2 in
        t_g, Let(f,xs,t1,t_b)
  | If(t1,t2,t3,_) -> t1, if b then t2 else t3
  | _ -> assert false

let rec get_trace ce map fmap trace defs t n =
  match t,ce with
      Var x, _ -> get_trace ce map fmap trace defs (App(Var x, [])) n
    | App(Fail, _), [FailNode,0] -> trace, map
    | App(Var x, ts), _ ->
        let _,t1 = List.assoc x defs in
        let xs = get_args x.typ in
        let map',typ = rename_typ x.typ in
        let x' = {(new_var_id x) with typ=typ} in
        let fmap' = (x,x')::fmap in
        let def =
          let xs,t = List.assoc x defs in
          let t' = List.fold_left (fun t (x,x') -> subst x (Var x') t) t1 ((x,x')::map') in
            x', (xs, t')
        in
        let defs' = List.map (fun (f,(xs,t)) -> f,(xs,subst x (Var x') t)) defs in
        let t1' = List.fold_right2 subst xs ts t1 in
        let t1'' = subst x (Var x') t1' in
        let t2 = List.fold_left (fun t (x,x') -> subst x (Var x') t) t1 fmap' in
          get_trace ce (map'@@map) fmap' (([],t1'',t2,n)::trace) (def::defs') t1'' (n+1)

(*
        let _,t1 = List.assoc x defs in
        let fs = List.filter (fun f -> List.exists (fun (g,_) -> f=g) defs) (get_fv t1) in
        let map',fs' =
          let aux f (map,fs) =
            let map',typ = rename_typ f.typ in
            let f' = {(new_var_id f) with typ=typ} in
              map'@@map, f'::fs
          in
            List.fold_right aux fs ([],[])
        in
        let map'' = List.combine fs fs' @@ map' in
        let map_term t = List.fold_left (fun t (f,f') -> subst f (Var f') t) t map'' in
        let defs' =
          let aux f f' =
            let xs,t = List.assoc f defs in
            let xs' = get_args f'.typ in
            let t' = map_term t in
              f', (xs', t')
          in
            List.map2 aux fs fs'
        in
        let ts' = List.map map_term ts in
        let t1' = map_term t1 in
        let xs = get_args x.typ in
        let t2 = List.fold_right2 subst xs ts t1 in
          get_trace ce (map''@@map) (([],t2,t1,n)::trace) (defs'@@defs) t2 (n+1)
*)
(*
*)
    | If(_, t2, _, _), (LabNode true, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,t,n)::trace' ->
                let t_g,t_t = get_branch true t in
                  (t_g::constr,t2,t_t,n)::trace'
            | _ -> assert false
        in
          get_trace ce' map fmap trace' defs t2 n
    | If(_, _, t3, _), (LabNode false, _)::ce' ->
        let trace' =
          match trace with
              (constr,_,t,n)::trace' ->
                let t_g,t_e = get_branch false t in
                  (Not t_g::constr,t3,t_e,n)::trace'
            | _ -> assert false
        in
          get_trace ce' map fmap trace' defs t3 n
    | _ -> assert false



    


let get_constr ce defs t =
  let trace,map = get_trace ce [] [] [[],t,t,0] defs t 1 in
  let l_defs =
    let s = {(new_var' "s") with typ = TUnit} in
    let aux (prev,defs) (cond,f,t,n) =
      get_head f, (prev,n,cond,t)::defs
    in
      List.map (fun (f,n,cond,t) -> get_head' f,n,cond,t) (snd (List.fold_left aux (Var s,[]) (List.rev trace)))
  in
  let scmap = rev_map_flatten (fun (f,_,_,_) -> get_scmap_typ [] f.typ) l_defs in
  let () =
    let aux (f,n,_,t) =
      Format.printf "%a --%d--> %a@." print_id_typ f n (print_term_fm ML false) t
    in
      List.iter aux l_defs
  in
  let constr =
    let check_exists g m = List.exists (fun (f,n,_,_) -> (f=g || List.mem g (get_argvars f.typ)) && n=m) l_defs in
    let get_pred f n x = Pred (f, n, x, List.map (fun x -> Var x) (List.assoc x scmap)) in
    let get_env f k xs =
        let aux x =
          match x.typ with
              TInt _ -> [get_pred f k x]
            | TBool -> [Pred(f,k,x,[])]
            | _ -> []
        in
          rev_map_flatten aux xs
    in
    let aux (f,k,cond,t) =
      let xs = get_args f.typ in
      let env = get_env f k xs in
      let cond' = List.map (fun t -> Cond t) cond in
      let rec subtyp_constr f m typ1 g n typ2 =
        assert (match typ1,typ2 with TFun _,TFun _ -> true | _ -> false);
        let aux (cond,constr) x1 x2 =
          match x1.typ with
              TUnit -> cond, constr
            | TBool ->
                let cond' = Cond (BinOp (Eq, Var x1, Var x2)) :: cond in
                let constr' = (Pred(g,n,x2,[])::cond', Pred(f,m,x1,[])) :: constr in
                  cond', constr'
            | TInt _ ->
                let cond' = Cond (BinOp (Eq, Var x1, Var x2)) :: cond in
                let constr' = (get_pred g n x2::cond', get_pred f m x1) :: constr in
                  cond', constr'
            | TFun _ ->
                let constr' = subtyp_constr x2 n x2.typ x1 m x1.typ @@ constr in
                  cond, constr'
            | _ -> assert false
        in
        let xs1 = get_args typ1 in
        let xs2 = get_args typ2 in
          snd (List.fold_left2 aux ([],[]) xs1 xs2)
      in
      let rec gen_constr t n =
        match t with
            Unit
          | True
          | False
          | BinOp((Eq|Lt|Gt|Leq|Geq|And|Or),_,_)
          | Not _
          | Int _
          | NInt _
          | BinOp((Add|Sub|Mult),_,_) -> assert false
          | Var x -> [], x.typ, []
          | App(Fail, ts) ->
              assert (List.length ts = 2);
              [[], Cond False], TUnit, []
          | App(Var g, ts) ->
              let rec aux cond ts xs acc =
                match xs,ts with
                    _,[] -> acc, List.fold_right (fun x typ -> TFun((x,x.typ),typ)) xs TUnit, cond
                  | x::xs,t::ts ->
                      let constr =
                        match x.typ with
                            TUnit -> []
                          | TBool -> [Cond t::cond, Pred(g,n,x,[])]
                          | TInt _ -> [Cond (BinOp(Eq, Var x, t))::cond, get_pred g n x]
                          | TFun _ ->
                              let f = get_head' t in
                              let rec aux2 m =
                                let constr,typ,cond' = gen_constr t m in
                                let constr' = List.map (fun (pre,post) -> cond@@cond'@@pre,post) (subtyp_constr f m typ x n x.typ) in
                                  constr' @@ constr
                              in
                                List.flatten (mapi (fun n _ -> if check_exists f n then aux2 n else []) l_defs)
                          | _ -> assert false
                      in
                      let cond' =
                        if Wrapper.isTInt x.typ
                        then Cond (BinOp(Eq, Var x, t)) :: cond
                        else cond
                      in
                        aux cond' ts xs (constr@@acc)
                  | _ -> assert false
              in
                aux [] ts (get_args g.typ) []
          | _ -> assert false
      in
      let k' = if List.mem (get_head t) (List.map (fun x -> Var x) xs) then k else k+1 in
      let constr,typ,_ = gen_constr t k' in
        assert (typ = TUnit);
        List.map (fun (pre,post) -> env@@cond'@@pre,post) constr
    in
      rev_map_flatten aux l_defs
  in
  let () =
      List.iter (fun (pre,post) -> Format.printf "%a => %a@." print_constr_list pre print_constr post) constr;
      Format.printf "\n\n@."
  in
    constr, map









let add_preds map t =
  let xs = List.map fst map in
  let map' =
    let aux x =
      let new_preds = List.map snd (List.filter (fun (y,_) -> x=y) map) in
      let old_preds = match x.typ with TInt preds -> preds | _ -> assert false in
      let preds =
        let aux ps p =
          if List.exists (Wrapper.equiv [] p) ps ||
            List.exists (Wrapper.equiv [] (Not p)) ps ||
            Wrapper.equiv [] p True || Wrapper.equiv [] p False
          then ps
          else p::ps
        in
          List.fold_left aux old_preds new_preds
      in
      let preds' = List.map elim_typ_preds preds in
        x, {x with typ = TInt preds'}
    in
      List.map aux xs
  in
    replace_vars map' t
*)





let rec get_subterm = function
    True -> [True]
  | False -> [False]
  | BinOp((And|Or), t1, t2) ->
      get_subterm t1 @@ get_subterm t2
  | BinOp((Eq|Lt|Gt|Leq|Geq) as op, t1, t2) ->
      [BinOp(op, t1, t2)]
  | Not(BinOp((And|Or), t1, t2)) ->
      get_subterm (Not t1) @@ get_subterm (Not t2)
  | Not t -> [Not t]
  | _ -> assert false


let rec add_preds_typ sol typ1 typ2 =
(*
  Format.printf "%a and " (print_typ ML) typ1;
  Infer.print_rty typ2;
  Format.printf "@.";
*)
  match typ1, typ2 with
      TUnit, Infer.RTunit _ -> TUnit
    | TAbsBool, Infer.RTbool _ -> TAbsBool
    | TBool, Infer.RTbool _ -> TBool
    | TInt _, _ -> assert false
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun((x,TInt ps),rtyp1), Infer.RTifun(pred, rtyp2) ->
        let Infer.Pred(pid, terms) = pred (Var abst_var) in
        let typ =
          try
	    let p =
	      let ids, t = List.assoc pid sol in
		(*Format.printf "%a,%a,%a@." (print_term_fm ML true) t (print_ids) ids  (print_termlist ML 0 false) terms; *)
		subst_term (List.combine ids terms) t
	    in
	    let ps =
              let aux ps p =
	        if List.exists (Wrapper.equiv [] p) ps ||
		  List.exists (Wrapper.equiv [] (Not p)) ps ||
		  Wrapper.equiv [] p True || Wrapper.equiv [] p False
	        then ps
	        else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
              let p' = Syntax.normalize_bool_exp p in
              let () = assert (false || Wrapper.equiv [] p p') in
                List.fold_left aux ps (if Flag.use_subterm then get_subterm p' else [p'])
	    in
	      TInt ps
          with Not_found -> (*assert false*)
            TInt ps
        in
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TRInt p),rtyp1), Infer.RTifun(pred, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun((x, TRInt p), rtyp)
    | TFun((x,typ),rtyp1), Infer.RTbfun(_, rtyp2) ->
        let rtyp = add_preds_typ sol rtyp1 (rtyp2 (Var x)) in
          TFun((x, typ), rtyp)
    | TFun((x,typ),rtyp1), Infer.RTfun(typs, rtyp2) ->
        let typ = List.fold_left (add_preds_typ sol) typ typs in
        let rtyp = add_preds_typ sol rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and " (print_typ ML) typ1;
        Infer.print_rty typ2;
        assert false

let rec add_preds rte sol = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x ->
      (try 
        let typ = List.fold_left (add_preds_typ sol) x.typ
          (snd (List.find (fun (x', _) -> x'.id = x.id) rte)) in
        Var({x with typ = typ})
      with Not_found -> Var x)
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = add_preds rte sol t in
      let ts' = List.map (add_preds rte sol) ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t3' = add_preds rte sol t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
(*
      Format.printf "%a," (print_term_fm ML true) (Var f);
*)
      let typ = List.fold_left (add_preds_typ sol) f.typ
        (try snd (List.find (fun (f', _) -> f'.id = f.id) rte) with Not_found ->
          [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
          Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in

      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t1'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1' in
      let t2'' = subst f (Var f') t2' in
        Let(f', xs', t1'', t2'')
  | Letrec(f, xs, t1, t2) ->
(*
      Format.printf "%a," (print_term_fm ML true) (Var f);
*)
      let typ = List.fold_left (add_preds_typ sol) f.typ
        (try snd (List.find (fun (f', _) -> f'.id = f.id) rte) with Not_found ->
          [](*List.iter (fun (f, _) -> Format.printf "%a," (print_term_fm ML true) (Var f)) rte;
          Format.printf "%a@." (print_term_fm ML true) (Var f); assert false*)) in

      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
      let t1'' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1' in
      let t2'' = subst f (Var f') t2' in
        Letrec(f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = add_preds rte sol t1 in
      let t2' = add_preds rte sol t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_preds rte sol t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false

let rec remove_preds_typ typ =
  match typ with
      TUnit -> TUnit
    | TAbsBool -> TAbsBool
    | TBool -> TBool
    | TInt _ -> TInt []
    | TRInt p -> TRInt p
    | TVar _ -> assert false
    | TFun((x,typ1),typ2) ->
        TFun(({x with typ = remove_preds_typ x.typ},remove_preds_typ typ1), remove_preds_typ typ2)
    | TUnknown -> TUnknown

let rec remove_preds = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x -> Var({x with typ = remove_preds_typ x.typ})
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = remove_preds t in
      let ts' = List.map remove_preds ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
      let t3' = remove_preds t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let f' = {f with typ = remove_preds_typ f.typ} in
      let xs' = List.map (fun x -> {x with typ = remove_preds_typ x.typ}) xs in
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = {f with typ = remove_preds_typ f.typ} in
      let xs' = List.map (fun x -> {x with typ = remove_preds_typ x.typ}) xs in
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        Letrec(f', xs', t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = remove_preds t1 in
      let t2' = remove_preds t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = remove_preds t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false

let refine tdefs ces t0 =
(*
  let () = Format.printf "%a@." (print_term_fm ML true) t in
*)
  let defs,t = lift t0 in
(*
  let () = Format.printf "%a@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t defs) in
*)
  let map,_,defs',t' = rename defs t in
(*
  let scmap = get_scmap (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
  let () = Format.printf "\n\n%a\n\n@." (print_term_fm ML true) (List.fold_left (fun acc (f,(xs,t)) -> Letrec(f,xs,t,acc)) t' defs') in
*)

  let s = {id = 0; origin = "Main"; typ = TUnit} in
  let defs1 = (s,([], t'))::defs' in
  let rte, sol = Infer.test tdefs s defs1 ces in
  add_preds rte sol t0

(*
  let () = Format.printf "AAA@." in
  let constr,map2 = get_constr ce defs' t' in
  let () = Format.printf "BBB@." in
  let solution = solve_constr constr in
  let () = Format.printf "CCCn@." in
  let () = List.iter (fun (c,p) -> Format.printf "%a = [%a]@." print_constr (Pred c) (print_term_fm ML false) p) solution in

  let pred_map =
    let rev_map = List.map (fun (x,x') -> x',x) map in
    let rev_map2 = List.map (fun (x,x') -> x',x) map2 in
    let aux ((_,_,x,_),p) =
      let x' = assoc_exn (assoc_exn x rev_map2 x) rev_map x in
      let p' = List.fold_left (fun p (y,y') -> if x=y then subst y (Var abst_var) p else subst y (Var y') p) p rev_map2 in
      let p'' = List.fold_left (fun p (y,y') -> if x=y then subst y (Var abst_var) p else subst y (Var y') p) p' rev_map in
        x', p''
    in
      List.map aux solution
  in
    add_preds pred_map t0
*)



let rec add_preds_typ_ typ1 typ2 =
(*
  Format.printf "%a and %a@." (print_typ ML) typ1 (print_typ ML) typ2;
*)
  match typ1, typ2 with
      TUnit, TUnit -> TUnit
    | TAbsBool, TAbsBool -> TAbsBool
    | TBool, TBool -> TBool
    | TInt _, _ -> assert false
    | TRInt _, _ -> assert false
    | TVar _, _ -> assert false
    | TFun((x,TInt ps),rtyp1), TFun((y,TInt qs),rtyp2) ->
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let typ =
          try
            let ps =
              let aux ps p =
                if List.exists (Wrapper.equiv [] p) ps ||
                  List.exists (Wrapper.equiv [] (Not p)) ps ||
                  Wrapper.equiv [] p True || Wrapper.equiv [] p False
                then ps
                else ((*Format.printf "adding %a@." (print_term_fm ML true) p;*) p::ps)
              in
              List.fold_left (fun ps p -> aux ps ((*Syntax.normalize_bool_exp*) p)) ps qs
            in
            TInt ps
          with Not_found -> (*assert false*)
            TInt ps
        in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TInt _),rtyp1), TFun((y,TRInt p),rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,TRInt p),rtyp1), TFun((y,TInt _),rtyp2)
    | TFun((x,TRInt p),rtyp1), TFun((y,TRInt _),rtyp2) ->
        let typ = TRInt p in
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TFun((x,typ1),rtyp1), TFun((y,typ2),rtyp2) ->
        let rtyp2 = subst_type y (Var x) rtyp2 in
        let typ = add_preds_typ_ typ1 typ2 in
        let rtyp = add_preds_typ_ rtyp1 rtyp2 in
          TFun(({x with typ = typ}, typ), rtyp)
    | TUnknown, _ -> TUnknown
    | _, _ ->
        Format.printf "%a and %a" (print_typ ML) typ1 (print_typ ML) typ2;
        assert false

let rec add_preds_ typedefs = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x -> NInt x
  | Var x ->
      let typ = 
        try add_preds_typ_ x.typ (snd (List.find (fun (x', _) -> x'.id = x.id) typedefs))
        with Not_found ->
          x.typ
      in
      Var({x with typ = typ})
  | Fun _ -> assert false
  | App(t, ts) ->
      let t' = add_preds_ typedefs t in
      let ts' = List.map (add_preds_ typedefs) ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
      let t3' = add_preds_ typedefs t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let typ = 
        try add_preds_typ_ f.typ (snd (List.find (fun (f', _) -> f'.id = f.id) typedefs))
        with Not_found ->
          f.typ
      in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
      let t1'' = List.fold_right2 subst xs (List.map (fun x -> Var x) xs') t1' in
      let t2'' = subst f (Var f') t2' in
        Let(f', xs', t1'', t2'')
  | Letrec(f, xs, t1, t2) ->
      let typ = 
        try add_preds_typ_ f.typ (snd (List.find (fun (f', _) -> f'.id = f.id) typedefs))
        with Not_found ->
          f.typ
      in
      let f' = {f with typ = typ} in
      let xs' = get_args typ in
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
      let t1'' = List.fold_right2 subst (f::xs) (List.map (fun x -> Var x) (f'::xs')) t1' in
      let t2'' = subst f (Var f') t2' in
        Letrec(f', xs', t1'', t2'')
  | BinOp(op, t1, t2) ->
      let t1' = add_preds_ typedefs t1 in
      let t2' = add_preds_ typedefs t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_preds_ typedefs t in
        Not t'
  | Fail -> Fail
  | Label _ -> assert false
  | Event s -> Event s
