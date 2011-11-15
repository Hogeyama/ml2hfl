
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
        Flag.Reachability -> (0,"unit",[])::make_base_spec n 0
      | Flag.FileAccess ->
          let spec = make_file_spec () in
          let qm = List.fold_left (fun acc (n,_,_) -> max acc n) 0 spec in
          let spec' = rev_flatten_map (fun i -> make_base_spec n i) (Array.to_list (Array.init (qm+1) (fun i -> i))) in
            spec @@ spec'
  in
    List.sort compare spec


let capitalize_var = String.capitalize

let capitalize ((env,defs,main):prog) : prog =
  let env' = List.map (fun (f,typ) -> capitalize_var f, typ) env in
  let map = List.map (fun (f,_) -> f, Var (capitalize_var f)) env in
  let aux (f,xs,t1,e,t2) = capitalize_var f, xs, subst_map map t1, e, subst_map map t2 in
  let defs' = List.map aux defs in
  let main' = capitalize_var main in
    env', defs', main'


let elim_non_det ((env,defs,main):prog) : prog =
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
    Typing.infer ([], elim_non_det_def defs, main)

let make_bottom ((env,defs,main):prog) : prog =
  let bottoms = ref [] in
  let aux_def (f,xs,t1,e,t2) =
    let env' = get_env (List.assoc f env) xs @@ env in
    let make_bottom n =
      let x = "Bottom" ^ string_of_int n in
        bottoms := (x,n)::!bottoms;
        Var x
    in
    let rec aux_term = function
        Const Bottom, typ ->
          let n = get_arg_num typ in
            make_bottom n
      | Const c, _ -> Const c
      | Var x, _ -> Var x
      | App(App(App(Const If, t1), t2), t3), typ ->
          let t1' = aux_term (t1,TBase(TBool,fun _ -> [])) in
          let t2' = aux_term (t2,typ) in
          let t3' = aux_term (t3,typ) in
            App(App(App(Const If, t1'), t2'), t3')
      | App(t1,t2), _ ->
          let typ = get_typ env' t1 in
          let typ' =
            match typ with
                TFun(typ,_) -> typ
              | _ -> assert false
          in
            App(aux_term (t1,typ), aux_term (t2,typ'))
      | Let _, _ -> assert false
      | Fun _, _ -> assert false
    in
    let t2' =
      try
        aux_term (t2, get_typ env' t2)
      with TypeBottom -> make_bottom 0
    in
      f, xs, t1, e, t2'
  in
  let make (x,n) =
    let xs = Array.to_list (Array.init n (fun _ -> "x")) in
      x, xs, Const True, [], make_app (Var x) (List.map (fun x -> Var x) xs)
  in
  let defs' = List.map aux_def defs in
  let bottom_defs = List.map make (uniq compare !bottoms) in
    env, bottom_defs@@defs', main


let rec eta_expand_term env = function
    Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, Const RandBool), t2), t3) ->
      let typ = get_typ env t2 in
      let xs = Array.to_list (Array.init (arg_num typ) (fun _ -> new_id "x")) in
      let aux t = List.fold_left (fun t x -> App(t, Var x)) t xs in
      let t = make_if (Const RandBool) (aux t2) (aux t3) in
        List.fold_right (fun x t -> Fun(x,t)) xs t
  | App(t1, t2) -> App(eta_expand_term env t1, eta_expand_term env t2)
  | Fun _ -> assert false
  | Let _ -> assert false


let eta_expand_def env ((f,xs,t1,e,t2):fun_def) =
  let d = arg_num (List.assoc f env) - List.length xs in
  let ys = Array.to_list (Array.init d (fun _ -> new_id "x")) in
  let t2' = eta_expand_term (get_env (List.assoc f env) xs @@ env) t2 in
  let t2'' = List.fold_left (fun t x -> App(t, Var x)) t2' ys in
    f, xs@ys, t1, e, t2''

let eta_expand ((env,defs,main) : prog) : prog=
  lift2 (env, List.map (eta_expand_def env) defs, main)



let trans_ce ce =
  let take s n = snd (split_string s n) in
  let aux (s,_) =
    match s with
        "unit" -> [CEGAR_syntax.EventNode "unit"]
      | "br" -> []
      | s when s.[0] = 'l' -> [CEGAR_syntax.BranchNode (int_of_string (take s 1))]
      | s when is_prefix_string "event_" s -> [CEGAR_syntax.EventNode (take s 6)]
      | _ -> assert false
  in
    flatten_map aux ce


let model_check_aux target =
    match TrecsInterface.check target with
        None -> None
      | Some ce -> Some (trans_ce ce)
        



