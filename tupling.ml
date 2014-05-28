open Util
open Type
open Syntax
open Term_util


let debug = false


type form =
  | FSimpleRec
  | FNonRec
  | FOther

exception Cannot_compose
exception Not_recursive


let is_none_term t =
  match t.desc with
  | Pair(t1,t2) -> t1 = true_term && t2.desc = Const (Int 0)
  | _ -> false

let is_some_term t =
  match t.desc with
  | Pair(t1,t2) -> t1 = false_term
  | _ -> false


let pair_let = make_trans ()

let pair_let_desc desc =
  match desc with
  | Pair(t1, t2) ->
      let t1' = pair_let.tr_term t1 in
      let t2' = pair_let.tr_term t2 in
      let lhs = Id.new_var "l" t1'.typ in
      let rhs = Id.new_var "r" t2'.typ in
      (make_lets [rhs,[],t2'; lhs,[],t1'] @@ make_pair (make_var lhs) (make_var rhs)).desc
  | _ -> pair_let.tr_desc_rec desc

let () = pair_let.tr_desc <- pair_let_desc

let pair_let = pair_let.tr_term



let assoc_env f env =
  let _,xs,t = Id.assoc f env in
  assert (xs = []);
  let ys,t' = decomp_fun t in
  ys, t'

let rec decomp_let t =
  match t.desc with
    Let(flag, [f,xs,t1], t2) ->
      let bindings,t2' = decomp_let t2 in
      (flag,(f,xs,t1))::bindings, t2'
  | _ ->
    let r = Id.new_var "r" t.typ in
    [Nonrecursive, (r,[],t)], make_var r

let partition_bindings x t =
  if debug then Format.printf "PB: x:%a@." Id.print x;
  let bindings,t' = decomp_let t in
  let check t =
    if List.mem x (get_fv t)
    then (raise Cannot_compose)
  in
  let aux (flag,(f,xs,t)) (before,app_x,after) =
    match app_x, xs, t with
      None, [], {desc=App({desc=Var y}, ts)} when Id.same x y ->
        assert (flag = Nonrecursive);
        before, Some (f,ts), after
    | None, _, _ ->
        if debug then Format.printf "CHECK: %a@." pp_print_term t;
        check t;
        before, app_x, (flag,(f,xs,t))::after
    | Some _, _, {desc=App({desc=Var y}, ts)} when Id.same x y ->
        raise Cannot_compose
    | Some _, _, _ ->
        check t;
        (flag,(f,xs,t))::before, app_x, after
  in
  let before,app_x,after = List.fold_right aux bindings ([],None,[]) in
  match app_x with
    None -> raise Not_recursive
  | Some xts -> before, xts, after, t'

let classify f t =
  try
    ignore (partition_bindings f t); FSimpleRec
  with
    Not_recursive -> FNonRec
  | Cannot_compose -> FOther

let compose_let_same_arg map fg f t1 g t2 =
  if debug then Format.printf "compose_let_same_arg@.";
  let before1,(x1,ts1),after1,t1' = partition_bindings f t1 in
  let before2,(x2,ts2),after2,t2' = partition_bindings g t2 in
  let aux ts_small ts_big map =
    assert (List.length map = List.length ts_big);
    if List.exists2 (fun t -> function None -> false | Some i -> t <> List.nth ts_small i) ts_big map
    then raise Cannot_compose;
    ts_big
  in
  let ts =
    match map with
      `Subset map' -> aux ts1 ts2 map'
    | `Supset map' -> aux ts2 ts1 map'
  in
  let before = before1 @ before2 in
  let after = after1 @ after2 in
  let p = Id.new_var "p" (TPair(x1, Id.typ x2)) in
  let pat =
    [p,  [], make_app (make_var fg) ts;
     x1, [], make_fst @@ make_var p;
     x2, [], make_snd @@ make_var p]
  in
  make_lets_f before @@ make_lets pat @@ make_lets_f after @@ make_pair t1' t2'

let compose_non_recursive first t1 t2 =
  let bindings,t = decomp_let (if first then t1 else t2) in
  let r = Id.new_var "r" (if first then t1.typ else t2.typ) in
  let t' =
    if first
    then make_pair (make_var r) t2
    else make_pair t1 (make_var r)
  in
  make_lets_f (bindings @ [Nonrecursive,(r,[],t)]) t'

let compose_typ typ1 typ2 =
  match typ1, typ2 with
    TFun(x1,typ1'), TFun(x2,typ2') ->
      TFun(x1, TFun(x2, TPair(Id.new_var "r" typ1', typ2')))
  | _ -> assert false

let compose_let_diff_arg fg f t1 g t2 =
  let before1,(x1,ts1),after1,t1' = partition_bindings f t1 in
  let before2,(x2,ts2),after2,t2' = partition_bindings g t2 in
  let before = before1 @ before2 in
  let after = after1 @ after2 in
  let p = Id.new_var "p" (TPair(x1, Id.typ x2)) in
  let pat =
    [p,  [], make_app (make_var fg) (ts1 @ ts2);
     x1, [], make_fst @@ make_var p;
     x2, [], make_snd @@ make_var p]
  in
  make_lets_f before @@ make_lets pat @@ make_lets_f after @@ make_pair t1' t2'

let compose_let map f t1 g t2 =
  if debug then Format.printf "compose_let@.%a:%a@.@.%a:%a@.@." Id.print f pp_print_term t1 Id.print g pp_print_term t2;
  match classify f t1, classify g t2, map with
    FNonRec,    _,          _ -> compose_non_recursive true t1 t2
  | _,          FNonRec,    _ -> compose_non_recursive false t1 t2
  | FOther,     _,          _
  | _,          FOther,     _ -> raise Cannot_compose
  | FSimpleRec, FSimpleRec, Some (map',fg) -> compose_let_same_arg map' fg f t1 g t2
  | FSimpleRec, FSimpleRec, None ->
      let fg = Id.new_var (Id.name f ^ "_" ^ Id.name g) @@ compose_typ (Id.typ f) (Id.typ g) in
      compose_let_diff_arg fg f t1 g t2


let tupling = make_trans2 ()

let is_wrapped t =
  match t.desc with
  | If(t1,t2,t3) when is_none t2 -> Option.map (fun t1' -> t1', t3) @@ decomp_is_none t1
  | _ -> None

let inline_wrapped = make_trans ()

let inline_wrapped_desc desc =
  match desc with
  | Pair(t1,t2) ->
      let t1' = inline_wrapped.tr_term t1 in
      let t2' = inline_wrapped.tr_term t2 in
      begin match is_wrapped t1', is_wrapped t2' with
        Some(t11, t12), Some(t21, t22) ->
          (make_if (make_is_none t11)
            (make_pair (make_none @@ get_opt_typ t1'.typ) t2')
            (make_if (make_is_none t21)
               (make_pair t12 (make_none @@ get_opt_typ t2'.typ))
               (make_pair t12 t22))).desc
      | _ -> inline_wrapped.tr_desc_rec desc
      end
  | _ -> inline_wrapped.tr_desc_rec desc

let () = inline_wrapped.tr_desc <- inline_wrapped_desc







let classify f t =
  try
    ignore (partition_bindings f t); FSimpleRec
  with
    Not_recursive -> FNonRec
  | Cannot_compose -> FOther

let compose_let_same_arg map fg f t1 g t2 =
  if debug then Format.printf "compose_let_same_arg@.";
  let before1,(x1,ts1),after1,t1' = partition_bindings f t1 in
  let before2,(x2,ts2),after2,t2' = partition_bindings g t2 in
  let aux ts_small ts_big map =
    assert (List.length map = List.length ts_big);
    if List.exists2 (fun t -> function None -> false | Some i -> t <> List.nth ts_small i) ts_big map
    then raise Cannot_compose;
    ts_big
  in
  let ts =
    match map with
      `Subset map' -> aux ts1 ts2 map'
    | `Supset map' -> aux ts2 ts1 map'
  in
  let before = before1 @ before2 in
  let after = after1 @ after2 in
  let p = Id.new_var "p" (TPair(x1, Id.typ x2)) in
  let pat =
    [p,  [], make_app (make_var fg) ts;
     x1, [], make_fst @@ make_var p;
     x2, [], make_snd @@ make_var p]
  in
  make_lets_f before @@ make_lets pat @@ make_lets_f after @@ make_pair t1' t2'

let compose_non_recursive first t1 t2 =
  let bindings,t = decomp_let (if first then t1 else t2) in
  let r = Id.new_var "r" (if first then t1.typ else t2.typ) in
  let t' =
    if first
    then make_pair (make_var r) t2
    else make_pair t1 (make_var r)
  in
  make_lets_f (bindings @ [Nonrecursive,(r,[],t)]) t'

let rec compose f t1 g t2 =
  if debug then Format.printf "compose@.";
  match t1.desc, t2.desc with
    If(t11, t12, t13), _ ->
    make_if t11 (compose f t12 g t2) (compose f t13 g t2)
  | _, If(t21, t22, t23) ->
      make_if t21 (compose f t1 g t22) (compose f t1 g t23)
  | _ -> raise Cannot_compose

let rec compose_same_arg map fg f t1 g t2 =
  if debug then Format.printf "compose_same_arg@.";
  match t1.desc, t2.desc with
    If(t11, t12, t13), If(t21, t22, t23) when t11 = t21 ->
    let t2' = compose_same_arg map fg f t12 g t22 in
    let t3' = compose_same_arg map fg f t13 g t23 in
    make_if t11 t2' t3'
  | If(t11, t12, t13), _ ->
      let t2' = compose_same_arg map fg f t12 g t2 in
      let t3' = compose_same_arg map fg f t13 g t2 in
      make_if t11 t2' t3'
  | _, If(t21, t22, t23) ->
      make_if t21 (compose_same_arg map fg f t1 g t22) (compose_same_arg map fg f t1 g t23)
  | _ -> compose_let (Some (map, fg)) f t1 g t2


let same_arg_map xs1 xs2 =
  let rec find i x xs =
    match xs with
      [] -> None
    | x'::xs' when Id.same x x' -> Some i
    | _::xs' -> find (i+1) x xs'
  in
  let find x xs = find 0 x xs in
  let make_map xs1 xs2 = List.map (fun x -> find x xs1) xs2 in
  if subset xs1 xs2 then
    Some (`Subset (make_map xs1 xs2))
  else if subset xs2 xs1 then
    Some (`Supset (make_map xs2 xs1))
  else
    None

let assoc_env f env =
  if debug then Color.printf Color.Reverse "%a@." Id.print f;
  let _,xs,t = Id.assoc f env in
  let ys,t' = decomp_fun t in
  match xs@ys with
    x::xs' -> x, List.fold_right make_fun xs' t'
  | _ -> raise Not_found

















































let compose_non_recursive first t1 t2 =
  if debug then Format.printf "compose_non_recursive@.";
  let bindings,t = decomp_let (if first then t1 else t2) in
  let r = Id.new_var "r" (if first then t1.typ else t2.typ) in
  let t' =
    if first
    then make_pair (make_var r) t2
    else make_pair t1 (make_var r)
  in
  make_lets_f (bindings @ [Nonrecursive,(r,[],t)]) t'

let compose_simple_rec fg f t1 g t2 =
  let before1,(x1,ts1),after1,t1' = partition_bindings f @@ Trans.alpha_rename t1 in
  let before2,(x2,ts2),after2,t2' = partition_bindings g @@ Trans.alpha_rename t2 in
  (*
  let x1' = Id.new_var_id x1 in
  let x2' = Id.new_var_id x2 in
  let sbst1 = subst x1 @@ make_var x1' in
  let sbst2 = subst x2 @@ make_var x2' in
  let aux sbst (flag,(f,xs,t)) = flag, (f, xs, sbst t) in
  let after1' = List.map (aux sbst1) after1 in
  let after2' = List.map (aux sbst2) after1 in
  let t1'' = sbst1 t1' in
  let t2'' = sbst2 t2' in
   *)
  let before = before1 @ before2 in
  let after = after1 @ after2 in
  let p = Id.new_var "p" (TPair(x1, Id.typ x2)) in
  let pat =
    [p,  [], make_app (make_var fg) (ts1 @ ts2);
     x1, [], make_fst @@ make_var p;
     x2, [], make_snd @@ make_var p]
  in
  make_lets_f before @@ make_lets pat @@ make_lets_f after @@ make_pair t1' t2'

let compose_let fg f t1 g t2 =
  if debug then Format.printf "compose_let@.%a:%a@.@.%a:%a@.@." Id.print f pp_print_term t1 Id.print g pp_print_term t2;
  match classify f t1, classify g t2 with
  | FNonRec,    _          -> compose_non_recursive true t1 t2
  | _,          FNonRec    -> compose_non_recursive false t1 t2
  | FOther,     _
  | _,          FOther     -> raise Cannot_compose
  | FSimpleRec, FSimpleRec -> compose_simple_rec fg f t1 g t2

let rec compose fg f t1 g t2 =
  if debug then Format.printf "compose@.";
  match t1.desc, t2.desc with
    If(t11, t12, t13), _ ->
    make_if t11 (compose fg f t12 g t2) (compose fg f t13 g t2)
  | _, If(t21, t22, t23) ->
      make_if t21 (compose fg f t1 g t22) (compose fg f t1 g t23)
  | _ -> compose_let fg f t1 g t2


let new_funs = ref ([] : (id list * (id * id list * typed_term)) list)

let tupling_term env t =
  match t.desc with
  | Pair(t1, t2) when decomp_some t1 <> None && decomp_some t2 <> None ->
      begin
        try
          if debug then Format.printf "PAIR: %a, %a@." pp_print_term t1 pp_print_term t2;
          begin match (Option.get @@ decomp_some t1).desc, (Option.get @@ decomp_some t2).desc with
                  App({desc = Var f}, [{desc = Snd tx}]),
                  App({desc = Var g}, [{desc = Snd ty}]) ->
                  let z1,t1 = assoc_env f env in
                  let z2,t2 = assoc_env g env in
                  let x' = Id.new_var "x" @@ get_opt_typ @@ tx.typ in
                  let y' = Id.new_var "y" @@ get_opt_typ @@ ty.typ in
                  let t1' = subst z1 (make_var x') @@ pair_let t1 in
                  let t2' = subst z2 (make_var y') @@ pair_let t2 in
                  let typ =
                    match t.typ with
                      TPair(x,typ2) -> TPair(Id.new_var "x" @@ get_opt_typ @@ Id.typ x, get_opt_typ typ2)
                    | _ -> assert false
                  in
                  let fg = Id.new_var (Id.name f ^ "_" ^ Id.name g) @@ TFun(x', TFun(y', typ)) in
                  let t_body = (*subst_map [x, make_var x'; y, make_var y'] @@*) compose fg f t1' g t2' in
                  let r = Id.new_var "r" typ in
                  let t_app = make_app (make_var fg) [make_snd @@ tx; make_snd @@ ty] in
                  let t_pair = make_pair (make_some @@ make_fst @@ make_var r) (make_some @@ make_snd @@ make_var r) in
                  new_funs := ([f;g], (fg, [x';y'], t_body)) :: !new_funs;
                  if debug then Format.printf "ADD: %a@." Id.print fg;
                  make_let [r, [], t_app] t_pair
                  | _ -> tupling.tr2_term_rec env t
          end
        with Not_found -> tupling.tr2_term_rec env t
      end
  | Let(flag, bindings, t) ->
      let bindings' = List.map (fun (f,xs,t) -> f, xs, tupling.tr2_term env t) bindings in
      let env' = List.map (fun (f,xs,t) -> f,(f,xs,t)) bindings' @ env in
      make_let_f flag bindings' @@ tupling.tr2_term env' t
  | _ -> tupling.tr2_term_rec env t

let () = tupling.tr2_term <- tupling_term

let add_funs = make_trans ()

let add_funs_desc desc =
  match desc with
    Let(flag, bindings, t) ->
    let bindings' = List.map (fun (f,xs,t) -> add_funs.tr_var f, List.map add_funs.tr_var xs, add_funs.tr_term t) bindings in
    let funs1,funs2 =
      let aux (fs,_) = List.exists (fun (f,_,_) -> Id.mem f fs) bindings in
      List.partition aux !new_funs
    in
    let funs1' =
      let aux (fs,def) =
        List.filter (fun f -> not @@ List.exists (fun (g,_,_) -> Id.same f g) bindings) fs,
        def
      in
      List.map aux funs1 in
    let funs11,funs12 = List.partition (fun (fs,_) -> fs = []) funs1' in
    new_funs := funs12 @ funs2;
    let t' =
      let t' = add_funs.tr_term t in
      List.fold_left (fun t (_,def) -> make_letrec [def] t) t' funs11
    in
    Let(flag, bindings', t')
  | _ -> add_funs.tr_desc_rec desc

let () = add_funs.tr_desc <- add_funs_desc

let tupling t =
  new_funs := [];
  let t' = tupling.tr2_term [] t in
  add_funs.tr_term t'









let compose_app = make_trans ()

let compose_app_term t =
  match t.desc with
  | Let _ ->
      let bindings,t' = decomp_let t in
      begin
        match bindings with
        | (Nonrecursive,(x,[],{desc=Snd({desc=App({desc=Var f},[{desc=Pair(t11,t12)}])})}))::
            (Nonrecursive,(y,[],{desc=Fst({desc=App({desc=Var g},[{desc=Pair(t21,t22)}])})}))::bindings'
             when Id.same f g && is_none t11 && is_some_term t12 && is_some_term t21 && is_none t22 ->
            if debug then Format.printf "%a, %a@." Id.print f Id.print g;
            let p = Id.new_var "p" (TPair(x, (Id.typ y))) in
            let bindings'' =
              [p, [], make_app (make_var f) [make_pair t12 t21];
               x, [], make_snd (make_var p);
               y, [], make_fst (make_var p)]
            in
            make_lets bindings'' @@ compose_app.tr_term @@ make_lets_f bindings' t'
        | _ -> compose_app.tr_term_rec t
      end
  | _ -> compose_app.tr_term_rec t

let () = compose_app.tr_term <- compose_app_term

let compose_app = compose_app.tr_term








let rec decomp_let_app t =
  match t.desc with
  | Let(Nonrecursive, [x,[], ({desc=App _} as t1)], t2) ->
      let bindings,t' = decomp_let_app t2 in
      (x,[],t1)::bindings, t'
  | _ -> [], t

let is_depend t x = Id.mem x @@ get_fv t

let let_normalize = make_trans ()

let let_normalize_desc desc =
  match desc with
    Let(Nonrecursive, [x,[],{desc=App _}], _) -> let_normalize.tr_desc_rec desc
  | Let(Nonrecursive, [x,[],t1], t2) ->
      let t1' = let_normalize.tr_term t1 in
      let t2' = let_normalize.tr_term t2 in
      let bindings,t2'' = decomp_let_app t2' in
      let rec aux acc bindings =
        match bindings with
          [] -> acc,[]
        | (_,_,t)::bindings' when is_depend t x -> acc, bindings
        | (y,_,t)::bindings' -> aux (acc@[y,[],t]) bindings'
      in
      let bindings1,bindings2 = aux [] bindings in
      if bindings1 = []
      then Let(Nonrecursive, [x,[],t1'], t2')
      else
        let t2''' = make_lets bindings2 t2'' in
        if debug then Color.printf Color.Yellow "NORMALIZE: %a@." Id.print x;
        if debug then Color.printf Color.Reverse "[%a]@." (print_list Id.print ";") @@ List.map (fun (x,_,_) -> x) bindings;
        (make_lets bindings1 @@ make_lets [x,[],t1'] t2''').desc
  | _ -> let_normalize.tr_desc_rec desc

let () = let_normalize.tr_desc <- let_normalize_desc

let let_normalize = let_normalize.tr_term



let rec tree_of_pair t =
  match t.desc with
    Pair({desc=Const n},_) -> Tree.Leaf t
  | Pair(t1,t2) -> Tree.Node(tree_of_pair t1, tree_of_pair t2)
  | _ -> Tree.Leaf t

let elim_check t1 t2 =
  if false && debug then Color.printf Color.Yellow "%a, %a@." pp_print_term t1 pp_print_term t2;
  match t1.desc, t2.desc with
    App({desc=Var f},ts1), App({desc=Var g},ts2) when Id.same f g ->
    let check t1 t2 =
      try
        let tree1 = tree_of_pair t1 in
        let tree2 = tree_of_pair t2 in
        let tts = Tree.flatten @@ Tree.zip tree1 tree2 in
        List.for_all (fun (t1, t2) -> same_term t1 t2 || is_none t1) tts
      with Invalid_argument "Tree.zip" -> false
    in
    List.for_all2 check ts1 ts2
  | _ -> false

let elim_sub_app = make_trans2 ()

let elim_sub_app_desc env desc =
  match desc with
  | Let(Nonrecursive, [x,[],t1], t2) ->
      let env' = (x,t1)::env in
      let t2' =
        try
          let y,_ = List.find (fun (y,t2) -> not (is_depend t1 y) && elim_check t2 t1) env in
          if debug then Format.printf "%a |-> %a@." Id.print y Id.print x;
          make_label (InfoId y) @@ subst y (make_var x) t2
        with Not_found -> t2
      in
      let t2'' = elim_sub_app.tr2_term env' t2' in
      Let(Nonrecursive, [x,[],t1], t2'')
  | _ -> elim_sub_app.tr2_desc_rec env desc

let () = elim_sub_app.tr2_desc <- elim_sub_app_desc

let elim_substed_let = make_trans2 ()

let elim_substed_let_term xs t =
  match t.desc with
  | Let(Nonrecursive, [x,[],t1], t2) when Id.mem x xs && not (is_depend t2 x) ->
      elim_substed_let.tr2_term xs t2
  | _ -> elim_substed_let.tr2_term_rec xs t

let () = elim_substed_let.tr2_term <- elim_substed_let_term

let elim_sub_app t =
  let t' = elim_sub_app.tr2_term [] t in
  let xs = col_info_id t' in
  if debug then Format.printf "%a@." (print_list Id.print "; ") xs;
  let t'' = elim_substed_let.tr2_term xs t' in
  Trans.remove_label t''



let is_option t = is_none t || decomp_some t <> None

let is_option_type typ =
  match typ with
  | TPair(x, _) when Id.typ x = none_flag.typ -> true
  | _ -> false

let rec decomp_option_tuple t =
  match t.desc with
  | _ when is_option t -> [t]
  | Pair(t1,t2) when is_option t1 -> t1 :: decomp_option_tuple t2
  | _ -> raise (Invalid_argument "decomp_option_tuple")

let rec decomp_option_ttuple typ =
  match typ with
  | _ when is_option_type typ -> [typ]
  | TPair(x,typ2) when is_option_type @@ Id.typ x -> Id.typ x :: decomp_option_ttuple typ2
  | _ -> raise (Invalid_argument "decomp_option_tuple")

let make_option_proj i t =
  let n = List.length @@ decomp_option_ttuple t.typ in
  let t' = repeat make_snd (i-1) t in
  if i = n then t' else make_fst t'

let elim_same_app = make_trans2 ()

let check t =
  try
    match t.desc with
    | App({desc=Var _}, [{desc=Pair _} as t2]) ->
        ignore (decomp_option_tuple t2);
        true
    | _ -> false
  with Invalid_argument _ -> false

let elim_same_app_term env t =
  match t.desc with
  | Let(Nonrecursive, [x,[],t1], t2) ->
      if check t1 then
        try
          let y,_ = List.find (fun (_,t2) -> same_term t2 t1) env in
          if debug then Format.printf "%a |-> %a@." Id.print x Id.print y;
          elim_same_app.tr2_term env @@ subst x (make_var y) t2
        with Not_found ->
          let env' = (x,t1)::env in
          let t2' = elim_same_app.tr2_term env' t2 in
          make_let [x,[],t1] t2'
      else
        elim_same_app.tr2_term_rec env t
  | _ -> elim_same_app.tr2_term_rec env t

let () = elim_same_app.tr2_term <- elim_same_app_term

let elim_same_app = elim_same_app.tr2_term []



let replace_app = make_trans2 ()

let is_used_in t1 t2 = col_same_term t1 t2 <> []

let rec decomp_let_app_option f t =
  match t.desc with
  | Let(Nonrecursive, [x, [], {desc=App({desc=Var g}, [t])} as binding], t2) when Id.same f g ->
      let ts = decomp_option_tuple t in
      let ts' = List.map decomp_some ts in
      let args = List.flatten @@ List.mapi (fun i t -> match t with None -> [] | Some t' -> [i+1, x, t']) ts' in
      let bindings,args',t' = decomp_let_app_option f t2 in
      binding::bindings, args@@@args', t'
  | _ -> [], [], t

let replace_app_term env t =
  match t.desc with
  | Let(Nonrecursive, [x, [], {desc=App({desc=Var f},[_])}], _) ->
      begin
        try
          let bindings,apps1,t2 = decomp_let_app_option f t in
          let env1,env2 = List.partition (fun (g,_) -> Id.same f g) env in
          let apps2 =
            match env1 with
            | [] -> []
            | [_,apps2] -> apps2
            | _ -> assert false
          in
          let cmp (i,_,t1) (j,_,t2) =
            if i = j then
              if same_term t1 t2 then 0 else 1
            else
              compare i j
          in
          let must = diff ~cmp apps1 apps2 in
          let apps' = apps1 @@@ apps2 in
          let env' = (f,apps')::env2 in
          let used = List.filter (fun (i,x,_) -> is_used_in (make_option_proj i @@ make_var x) t2) apps' in
          let must_but_not_used = diff ~cmp must used in
          let t2' = replace_app.tr2_term env' t2 in
          if List.length used < 2 (* negligence *)
          then raise (Invalid_argument "");
          if debug then
            begin
              Format.printf "replace[%d]: %a@." (List.length apps1) Id.print x;
              List.iter (fun (i,x,t) -> Format.printf "APPS: %a = %a ...%d... %a ...@." Id.print x Id.print f i pp_print_term t) apps';
              List.iter (fun (i,x,t) -> Format.printf "USED: %a = %a ...%d... %a ...@." Id.print x Id.print f i pp_print_term t) used;
              List.iter (fun (i,x,t) -> Format.printf "MUST: %a = %a ...%d... %a ...@." Id.print x Id.print f i pp_print_term t) must;
              List.iter (fun (i,x,t) -> Format.printf "MBNU: %a = %a ...%d... %a ...@." Id.print x Id.print f i pp_print_term t) must_but_not_used
            end;
          let y = Id.new_var_id x in
          let sbst, arg =
            try
              let used' = List.sort used in
              List.iteri (fun i (j,_,_) -> if i+1 <> j then raise (Invalid_argument "")) used';
              let aux sbst (i,x,_) = fun t -> replace_term (make_option_proj i @@ make_var x) (make_option_proj i @@ make_var y) @@ sbst t in
              let sbst =  List.fold_left aux Std.identity used' in
              sbst, make_tuple @@ List.map (fun (_,_,t) -> make_some t) used'
            with Not_found -> raise (Invalid_argument "")
          in
          let t1 = make_app (make_var f) [arg] in
          make_lets bindings @@ make_let [y,[],t1] @@ sbst t2'
        with Invalid_argument _ -> replace_app.tr2_term_rec env t
      end
  | _ -> replace_app.tr2_term_rec env t

let () = replace_app.tr2_term <- replace_app_term

let replace_app = replace_app.tr2_term []




let trans t =
  t
  |> inline_wrapped.tr_term
  |> Trans.flatten_let
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "flatten_let" pp_print_term
  |> let_normalize
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "normalize let" pp_print_term
  |> elim_sub_app
  |> elim_same_app
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "elim_same_app" pp_print_term
  |> Trans.elim_unused_branch
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "elim_unused_branch" pp_print_term
  |> Trans.elim_unused_let
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "elim_unused_let" pp_print_term
  |> tupling
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "tupled" pp_print_term
  |> Trans.normalize_let
  |> Trans.flatten_let
  |> Trans.inline_no_effect
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "normalize" pp_print_term
  |> replace_app
  |> elim_same_app
  |@debug&> Format.printf "%a:@.%a@.@." Color.s_red "replace_app" pp_print_term
  |@> flip Type_check.check Type.TUnit
