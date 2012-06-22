
open Format
open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_util

type node = UnitNode | BrNode | LineNode of int | EventNode of string







let make_line_spec n q =
  let rec aux i spec =
    if i < 0
    then spec
    else aux (i-1) ((q, "l" ^ string_of_int i, [q])::spec)
  in
    aux n []

let make_file_spec () =
  [0, "unit", [];
   0, "event_newr", [1];
   1, "event_read", [1];
   1, "event_close", [4];
   0, "event_neww", [2];
   2, "event_write", [2];
   2, "event_close", [4];
   2, "event_newr", [3];
   1, "event_neww", [3];
   3, "unit", [];
   3, "event_read", [3];
   3, "event_write", [3];
   3, "event_close", [3];
   4, "unit", [];]


let make_base_spec n q = (q, "br", [q;q])::make_line_spec 1 q

let make_spec n =
  let spec =
    match !Flag.mode with
        Flag.Reachability -> (0,"unit",[])::(0,"event_fail",[1])::make_base_spec n 0
      | Flag.FileAccess ->
          let spec = make_file_spec () in
          let qm = List.fold_left (fun acc (n,_,_) -> max acc n) 0 spec in
          let spec' = rev_flatten_map (fun i -> make_base_spec n i) (Array.to_list (Array.init (qm+1) (fun i -> i))) in
            spec @@ spec'
  in
    List.sort compare spec


let capitalize_var = String.capitalize

let capitalize {env=env;defs=defs;main=main} =
  let env' = List.map (fun (f,typ) -> capitalize_var f, typ) env in
  let map = List.map (fun (f,_) -> f, Var (capitalize_var f)) env in
  let aux (f,xs,t1,e,t2) = capitalize_var f, xs, subst_map map t1, e, subst_map map t2 in
  let defs' = List.map aux defs in
  let main' = capitalize_var main in
    {env=env'; defs=defs'; main=main'}


let elim_non_det {defs=defs;main=main} =
  let check f (g,_,_,_,_) = f = g in
  let mem f defs = List.exists (check f) defs in
  let rec elim_non_det_def = function
      [] -> []
    | (f,xs,t1,e,t2)::defs when mem f defs ->
        let f' = rename_id f in
        let defs1,defs2 = List.partition (check f) defs in
        let defs1' = List.map (fun (f,xs,t1,e,t2) -> rename_id f,xs,t1,e,t2) defs1 in
        let ts = List.map (fun x -> Var x) xs in
        let aux f = make_app (Var f) ts in
        let t = List.fold_left (fun t (f,_,_,_,_) -> make_br (aux f) t) (aux f') defs1' in
          (f,xs,Const True,[],t)::(f',xs,t1,e,t2)::defs1' @ elim_non_det_def defs2
    | def::defs -> def :: elim_non_det_def defs
  in
    Typing.infer {env=[]; defs=elim_non_det_def defs; main=main}

let make_bottom {env=env;defs=defs;main=main} =
  let bottoms = ref [] in
  let aux_def (f,xs,t1,e,t2) =
    let f_typ = List.assoc f env in
    let env' = get_arg_env f_typ xs @@ env in
    let make_bottom n =
      let x = "Bottom" ^ string_of_int n in
        bottoms := (x,n)::!bottoms;
        Var x
    in
    let rec aux_term t typ =
      match t,typ with
        Const Bottom, typ -> make_bottom (get_arg_num typ)
      | Const c, _ -> Const c
      | Var x, _ -> Var x
      | App(App(App(Const If, t1), t2), t3), typ ->
          let t1' = aux_term t1 (TBase(TBool,fun _ -> [])) in
          let t2' =
            try
              aux_term t2 typ
            with TypeBottom -> make_bottom 0
          in
          let t3' =
            try
              aux_term t3 typ
            with TypeBottom -> make_bottom 0
          in
            App(App(App(Const If, t1'), t2'), t3')
      | App(Const (Label n), t), typ -> App(Const (Label n), aux_term t typ)
      | App(t1,t2), _ ->
          let typ = get_typ env' t1 in
          let typ' =
            match typ with
                TFun(typ,_) -> typ
              | _ -> assert false
          in
            App(aux_term t1 typ, aux_term t2 typ')
      | Let _, _ -> assert false
      | Fun _, _ -> assert false
    in
    let app_typ x = function
        TFun(_,typ2) -> typ2 (Var x)
      | _ -> assert false
    in
    let typ = List.fold_right app_typ xs f_typ in
    let t2' = aux_term t2 typ in
      f, xs, t1, e, t2'
  in
  let make (x,n) =
    let xs = Array.to_list (Array.init n (fun _ -> "x")) in
      x, xs, Const True, [], Const Unit
  in
  let defs' = List.map aux_def defs in
  let bottom_defs = List.map make (uniq !bottoms) in
    {env=env; defs=bottom_defs@@defs'; main=main}


let rec eta_expand_term env = function
    Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      let typ = get_typ env t2 in
      let xs = Array.to_list (Array.init (arg_num typ) (fun _ -> new_id "x")) in
      let aux t = List.fold_left (fun t x -> App(t, Var x)) t xs in
      let t = make_if (Const RandBool) (aux t2) (aux t3) in
        List.fold_right (fun x t -> Fun(x,None,t)) xs t
  | App(t1, t2) -> App(eta_expand_term env t1, eta_expand_term env t2)
  | Fun _ -> assert false
  | Let _ -> assert false


let eta_expand_def env ((f,xs,t1,e,t2):fun_def) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = eta_expand_term (get_arg_env (List.assoc f env) xs @@ env) t2 in
  let t2'' = List.fold_left (fun t x -> App(t, Var x)) t2' ys in
    f, xs@ys, t1, e, t2''

let eta_expand prog = lift2 {prog with defs = List.map (eta_expand_def prog.env) prog.defs}



let trans_ce ce =
  let take s n = snd (split_string s n) in
  let aux (s,_) =
    match s with
        "unit" -> []
      | "br" -> []
      | s when s.[0] = 'l' -> [int_of_string (take s 1)]
      | s when is_prefix_string "event_" s -> []
      | _ -> assert false
  in
    flatten_map aux ce


let true_var = "True"
let false_var = "False"
let rec church_encode_term = function
    Const True -> Var true_var
  | Const False -> Var false_var
  | Const If -> assert false
  | Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      let t2' = church_encode_term t2 in
      let t3' = church_encode_term t3 in
        make_app (Const If) [Const RandBool; t2'; t3']
  | App(App(App(Const If, Var b), t2), t3) ->
      let t2' = church_encode_term t2 in
      let t3' = church_encode_term t3 in
        make_app (Var b) [t2'; t3']
  | App(t1, t2) -> App(church_encode_term t1, church_encode_term t2)
  | Let _ -> assert false
  | Fun _ -> assert false
let church_encode {env=env;defs=defs;main=main} =
  let true_def = true_var, ["x"; "y"], Const True, [], Var "x" in
  let false_def = false_var, ["x"; "y"], Const True, [], Var "y" in
  let defs' = List.map (apply_body_def church_encode_term) defs @ [true_def; false_def] in
  let prog = {env=[];defs=defs';main=main} in
    if false then Format.printf "CHURCH ENCODE:\n%a@." CEGAR_print.prog prog;
    Typing.infer prog


let rec full_app f n = function
    Const _ -> true
  | Var x when f = x -> false
  | Var x -> true
  | App _ as t ->
      let t1,ts = decomp_app t in
      let b1 = if t1 = Var f then n = List.length ts else true in
      let b2 = List.for_all (full_app f n) ts in
        b1 && b2
  | Let _ -> assert false
  | Fun _ -> assert false

let should_reduce (f,xs,t1,es,t2) env defs =
  let n = arg_num (List.assoc f env) in
    t1 = Const True && es = [] &&
    List.length (List.filter (fun (g,_,_,_,_) -> f=g) defs) = 1 &&
    List.length (rev_flatten_map (fun (_,_,_,_,t) -> List.filter ((=) f) (get_fv t)) defs) = 1 &&
    List.for_all (fun (_,_,_,_,t2) -> full_app f n t2) defs

let rec get_head_count f = function
    Const _ -> 0
  | Var x -> 0
  | App _ as t ->
      let t1,ts = decomp_app t in
      let n = List.fold_left (fun n t -> n + get_head_count f t) 0 ts in
        if t1 = Var f
        then 1 + n
        else n
  | Let _ -> assert false
  | Fun _ -> assert false

let rec beta_reduce_term flag (f,xs,t1,es,t2) = function
    Const c -> Const c
  | Var x -> Var x
  | App _ as t ->
      let t1,ts = decomp_app t in
      let ts' = List.map (beta_reduce_term flag (f,xs,t1,es,t2)) ts in
        if t1 = Var f
        then
          if List.for_all (function Const _ | Var _ -> true | App _ -> false | _ -> assert false) ts'
          then List.fold_right2 subst xs ts' t2
          else (flag := true; make_app t1 ts')
        else make_app t1 ts'
  | Let _ -> assert false
  | Fun _ -> assert false

let beta_reduce_term flag ((f,_,_,_,_) as def) t =
  let n = get_head_count f t in
    if n = 1
    then beta_reduce_term flag def t
    else (if n >= 2 then flag := true; t)

let beta_reduce_aux {env=env;defs=defs;main=main} =
  let rec aux defs1 = function
      [] -> defs1
    | ((f,_,_,_,_) as def)::defs2 when should_reduce def env (defs1@@def::defs2) ->
        let flag = ref false in
        let reduce_def (f',xs',t1',es',t2') = f', xs', t1', es', beta_reduce_term flag def t2' in
        let defs1' = List.map reduce_def defs1 in
        let defs2' = List.map reduce_def defs2 in
          if !flag
          then aux (defs1'@[def]) defs2'
          else aux defs1' defs2'
    | def::defs2 -> aux (defs1@[def]) defs2
  in
    {env=env; defs = aux [] defs; main=main}

let rec beta_reduce prog =
  let prog' = beta_reduce_aux prog in
    if prog.defs = prog'.defs
    then prog
    else beta_reduce prog'



let model_check_aux (prog,spec) =
  let prog = Typing.infer prog in
  let env = prog.env in
  let prog = if Flag.useless_elim then Useless_elim.elim prog else prog in
  let prog = if Flag.beta_reduce then beta_reduce prog else prog in
  let prog = if Flag.church_encode then church_encode prog else prog in
    match TrecsInterface.check env (prog,spec) with
        None -> None
      | Some ce -> Some (trans_ce ce)
