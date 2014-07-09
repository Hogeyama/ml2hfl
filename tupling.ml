open Util
open Type
open Syntax
open Term_util


let debug () = List.mem "Tupling" !Flag.debug_module


type form =
  | FSimpleRec
  | FNonRec
  | FOther

exception Cannot_compose
exception Not_recursive



let normalize_tuple = make_trans ()

let normalize_tuple_term t =
  match t.desc with
  | Tuple ts ->
      let ts' = List.map normalize_tuple.tr_term ts in
      let xs = List.mapi (fun i t -> Id.new_var ~name:("x" ^ string_of_int (i+1)) t.typ) ts' in
      make_lets (List.map2 (fun x t -> x,[],t) xs ts') @@ make_tuple @@ List.map make_var xs
  | _ -> normalize_tuple.tr_term_rec t

let () = normalize_tuple.tr_term <- normalize_tuple_term
let normalize_tuple = normalize_tuple.tr_term



let rec decomp_let t =
  match t.desc with
  | Let(flag, [f,xs,t1], t2) ->
      let bindings,t2' = decomp_let t2 in
      (flag,(f,xs,t1))::bindings, t2'
  | _ ->
    let r = Id.new_var ~name:"r" t.typ in
    [Nonrecursive, (r,[],t)], make_var r

let partition_bindings x t =
  if debug() then Format.printf "PB: x:%a@." Id.print x;
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
        if debug() then Format.printf "CHECK: %a@." print_term t;
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
  | Not_recursive -> FNonRec
  | Cannot_compose -> FOther







let tupling = make_trans2 ()

let is_wrapped t =
  match t.desc with
  | If(t1,t2,t3) when is_none t2 -> Option.map (fun t1' -> t1', t3) @@ decomp_is_none t1
  | _ -> None

let inline_wrapped = make_trans ()

let inline_wrapped_term t =
  match t.desc with
  | Tuple ts ->
      let ts' = List.map inline_wrapped.tr_term ts in
      let tts = List.map is_wrapped ts' in
      if List.for_all Option.is_some tts
      then
        let tts' = List.map Option.get tts in
        let make i =
          let aux j _ =
            if j < i then
              snd @@ List.nth tts' j
            else if i = j then
              make_none @@ get_opt_typ (List.nth ts' j).typ
            else
              List.nth ts' j
          in
          make_tuple @@ List.mapi aux ts
        in
        let n = List.length ts in
        let i,t = List.fold_right (fun (t1,_) (i,t) -> i-1, make_if (make_is_none t1) (make i) t) tts' (n-1, make n) in
        assert (i = -1);
        t
      else
        inline_wrapped.tr_term_rec t
  | _ -> inline_wrapped.tr_term_rec t

let () = inline_wrapped.tr_term <- inline_wrapped_term
let inline_wrapped = inline_wrapped.tr_term

let classify f t =
  try
    ignore (partition_bindings f t); FSimpleRec
  with
    Not_recursive -> FNonRec
  | Cannot_compose -> FOther

let assoc_env f env =
  if debug() then Color.printf Color.Reverse "%a@." Id.print f;
  let _,xs,t = Id.assoc f env in
  let ys,t' = decomp_funs t in
  match xs@ys with
  | x::xs' -> x, List.fold_right make_fun xs' t'
  | _ -> raise Not_found

let compose_non_recursive first t1 t2 =
  if debug() then Format.printf "compose_non_recursive@.";
  let bindings,t = decomp_let (if first then t1 else t2) in
  let r = Id.new_var ~name:"r" (if first then t1.typ else t2.typ) in
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
  let p = Id.new_var ~name:"p" @@ TTuple [x1; x2] in
  let pat =
    [p,  [], make_app (make_var fg) (ts1 @ ts2);
     x1, [], make_fst @@ make_var p;
     x2, [], make_snd @@ make_var p]
  in
  make_lets_f before @@ make_lets pat @@ make_lets_f after @@ make_pair t1' t2'

let compose_let fg fts =
  let forms = List.map (uncurry classify) fts in
  if debug() then Format.printf "compose_let@.";
  if debug() then List.iter (fun (f,t) -> Format.printf "%a:%a@.@." Id.print f print_term t) fts;
  match List.nth forms 0, List.nth forms 1 with
  | FNonRec,    _          -> compose_non_recursive true t1 t2
  | _,          FNonRec    -> compose_non_recursive false t1 t2
  | FOther,     _
  | _,          FOther     -> raise Cannot_compose
  | FSimpleRec, FSimpleRec -> compose_simple_rec fg fts

let rec compose fg fts =
  if debug() then Format.printf "compose@.";
  let decomp_if i t =
    match t.desc with
    | If(t1,t2,t3) -> Some (i,t1,t2,t3)
    | _ -> None
  in
  let ts' = List.mapi (fun i -> decomp_if i -| snd) fts in
  try
    let i,t1,t2,t3 = Option.get @@ List.find is_some ts' in
    let fts2 = List.replace_nth fts i t2 in
    let fts3 = List.replace_nth fts i t3 in
    make_iff t1 (compose fg fts2) (compose fg fts3)
  with Not_found -> compose_let fg fts



let new_funs = ref ([] : (id list * (id * id list * typed_term)) list)

let decomp_some t =
  match t.desc with
  | Tuple [t1;t2] when t1 = some_flag -> Some t2
  | _ -> None

let tupling_term env t =
  match t.desc with
  | Tuple[t1; t2] when decomp_some t1 <> None && decomp_some t2 <> None ->
      begin
        try
          if debug() then Format.printf "PAIR: %a, %a@." print_term t1 print_term t2;
          begin
            match (Option.get @@ decomp_some t1).desc, (Option.get @@ decomp_some t2).desc with
            | App({desc = Var f}, [{desc = Proj(1, tx)}]),
              App({desc = Var g}, [{desc = Proj(1, ty)}]) when tuple_num tx.typ = 2 && tuple_num ty.typ = 2 ->
                let z1,t1 = assoc_env f env in
                let z2,t2 = assoc_env g env in
                let x' = Id.new_var ~name:"x" @@ get_opt_typ @@ tx.typ in
                let y' = Id.new_var ~name:"y" @@ get_opt_typ @@ ty.typ in
                let t1' = subst z1 (make_var x') @@ normalize_tuple t1 in
                let t2' = subst z2 (make_var y') @@ normalize_tuple t2 in
                let typ =
                  match t.typ with
                  | TTuple xs -> TTuple (List.map (Id.map_typ get_opt_typ) xs)
                  | _ -> assert false
                in
                let fg = Id.new_var ~name:(Id.name f ^ "_" ^ Id.name g) @@ TFun(x', TFun(y', typ)) in
                let t_body = (*subst_map [x, make_var x'; y, make_var y'] @@*) compose fg [f, t1'; g, t2'] in
                let r = Id.new_var ~name:"r" typ in
                let t_app = make_app (make_var fg) [make_snd @@ tx; make_snd @@ ty] in
                let t_pair = make_pair (make_some @@ make_fst @@ make_var r) (make_some @@ make_snd @@ make_var r) in
                new_funs := ([f;g], (fg, [x';y'], t_body)) :: !new_funs;
                if debug() then Format.printf "ADD: %a@." Id.print fg;
                make_let [r, [], t_app] t_pair
            | _ -> tupling.tr2_term_rec env t
          end
        with Not_found -> tupling.tr2_term_rec env t
      end
  | Tuple ts when List.for_all (Option.is_some -| decomp_some) ts ->
      begin
        try
          if debug() then Format.printf "TUPLE: %a@." (print_list print_term ", ") ts;
          begin
            let aux t =
              match t.desc with
              | App({desc = Var f}, [{desc = Proj(1, t1)}]) -> f, t1
              | _ -> raise Not_found
            in
            let fs,tfs = List.split_map (aux -| Option.get -| decomp_some) ts in
            let zts = List.map (flip assoc_env env) fs in
            let xs = List.map (fun t -> Id.new_var @@ get_opt_typ t.typ) tfs in
            let ts' = List.map2 (fun (z,t) x -> subst_var z x @@ normalize_tuple t) zts xs in
            let typ =
              match t.typ with
              | TTuple xs -> TTuple (List.map (Id.map_typ get_opt_typ) xs)
              | _ -> assert false
            in
            let name = List.fold_left (fun s f -> s ^ "_" ^ Id.name f) (Id.name @@ List.hd fs) (List.tl fs) in
            let fg = Id.new_var ~name @@ List.fold_right (fun x typ -> TFun(x,typ)) xs typ in
            let t_body = compose fg @@ List.combine fs ts' in
            let r = Id.new_var ~name:"r" typ in
            let t_app = make_app (make_var fg) @@ List.mapi make_proj tfs in
            new_funs := (fs, (fg, xs, t_body)) :: !new_funs;
            if debug() then Format.printf "ADD: %a@." Id.print fg;
            make_let [r, [], t_app] @@ make_tuple @@ List.mapi (fun i _ -> make_some @@ make_proj i @@ make_var r) ts
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
        if debug() then Color.printf Color.Yellow "NORMALIZE: %a@." Id.print x;
        if debug() then Color.printf Color.Reverse "[%a]@." (print_list Id.print ";") @@ List.map (fun (x,_,_) -> x) bindings;
        (make_lets bindings1 @@ make_lets [x,[],t1'] t2''').desc
  | _ -> let_normalize.tr_desc_rec desc

let () = let_normalize.tr_desc <- let_normalize_desc

let let_normalize = let_normalize.tr_term



let rec tree_of_tuple t =
  match t.desc with
  | Tuple ts -> Rose_tree.Node (List.map tree_of_tuple ts)
  | _ -> Rose_tree.Leaf t

let elim_check t1 t2 =
  if false && debug() then Color.printf Color.Yellow "%a, %a@." print_term t1 print_term t2;
  match t1.desc, t2.desc with
    App({desc=Var f},ts1), App({desc=Var g},ts2) when Id.same f g ->
    let check t1 t2 =
      try
        let tree1 = tree_of_tuple t1 in
        let tree2 = tree_of_tuple t2 in
        let tts = Rose_tree.flatten @@ Rose_tree.zip tree1 tree2 in
        List.for_all (fun (t1, t2) -> same_term t1 t2 || is_none t1) tts
      with Invalid_argument "Rose_tree.zip" -> false
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
          if debug() then Format.printf "%a |-> %a@." Id.print y Id.print x;
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
  if debug() then Format.printf "%a@." (print_list Id.print "; ") xs;
  let t'' = elim_substed_let.tr2_term xs t' in
  Trans.remove_label t''



let is_option t = is_none t || Option.is_some @@ decomp_some t

let is_option_type typ =
  match typ with
  | TTuple[x; _] when Id.typ x = none_flag.typ -> true
  | _ -> false

let elim_same_app = make_trans2 ()

let elim_same_app_term env t =
  match t.desc with
  | Let(Nonrecursive, [x,[],({desc=App({desc=Var _}, [{desc=Tuple _}])} as t1)], t2) ->
      begin
        try
          let y,_ = List.find (same_term t1 -| snd) env in
          if debug() then Format.printf "%a |-> %a@." Id.print x Id.print y;
          elim_same_app.tr2_term env @@ subst x (make_var y) t2
        with Not_found ->
          make_let [x,[],t1] @@ elim_same_app.tr2_term ((x,t1)::env) t2
      end
  | _ -> elim_same_app.tr2_term_rec env t

let () = elim_same_app.tr2_term <- elim_same_app_term
let elim_same_app = elim_same_app.tr2_term []



let replace_app = make_trans2 ()

let is_used_in t1 t2 = col_same_term t1 t2 <> []

let rec decomp_let_app_option f t =
  match t.desc with
  | Let(Nonrecursive, [x, [], {desc=App({desc=Var g}, [{desc=Tuple ts}])} as binding], t2) when Id.same f g ->
      let ts' = List.map decomp_some ts in
      let args = List.flatten @@ List.mapi (fun i t -> match t with None -> [] | Some t' -> [i, x, t']) ts' in
      let bindings,args',t' = decomp_let_app_option f t2 in
      binding::bindings, args@@@args', t'
  | Let(Nonrecursive, [x, [], {desc=App({desc=Var g}, [_])} as binding], t2) when Id.same f g ->
      invalid_argument ""
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
          let must = List.diff ~cmp apps1 apps2 in
          let apps' = apps1 @@@ apps2 in
          let env' = (f,apps')::env2 in
          let used = List.filter (fun (i,x,_) -> is_used_in (make_proj i @@ make_var x) t2) apps' in
          let must_but_not_used = List.diff ~cmp must used in
          let t2' = replace_app.tr2_term env' t2 in
          if List.length used < 2 (* negligence *)
          then raise (Invalid_argument "");
          if debug() then
            begin
              Format.printf "replace[%d]: %a@." (List.length apps1) Id.print x;
              List.iter (fun (i,x,t) -> Format.printf "APPS: %a = %a ...%d... %a ...@." Id.print x Id.print f i print_term t) apps';
              List.iter (fun (i,x,t) -> Format.printf "USED: %a = %a ...%d... %a ...@." Id.print x Id.print f i print_term t) used;
              List.iter (fun (i,x,t) -> Format.printf "MUST: %a = %a ...%d... %a ...@." Id.print x Id.print f i print_term t) must;
              List.iter (fun (i,x,t) -> Format.printf "MBNU: %a = %a ...%d... %a ...@." Id.print x Id.print f i print_term t) must_but_not_used
            end;
          let y = Id.new_var_id x in
          let sbst, arg =
            try
              let used' = List.sort used in
              List.iteri (fun i (j,_,_) -> if i+1 <> j then raise (Invalid_argument "")) used';
              let aux sbst (i,x,_) = fun t -> replace_term (make_proj i @@ make_var x) (make_proj i @@ make_var y) @@ sbst t in
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
  |> inline_wrapped
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "inline_wrapped" print_term
  |> Trans.flatten_let
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "flatten_let" print_term
  |> let_normalize
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "normalize let" print_term
  |> elim_sub_app
  |> elim_same_app
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "elim_same_app" print_term
  |> Trans.elim_unused_branch
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "elim_unused_branch" print_term
  |> Trans.elim_unused_let
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "elim_unused_let" print_term
  |> tupling
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "tupled" print_term
  |> Trans.normalize_let
  |> Trans.flatten_let
  |> Trans.inline_no_effect
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "normalize" print_term
  |> replace_app
  |> elim_same_app
  |@debug()&> Format.printf "%a:@.%a@.@." Color.s_red "replace_app" print_term
  |@> flip Type_check.check Type.TUnit
