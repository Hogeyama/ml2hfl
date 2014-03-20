open Util
open Syntax
open Term_util
open Type


let get_args = make_col2 [] List.rev_append

let get_args_desc (f:id) desc =
  match desc with
    App({desc=Var g}, ts) when Id.same f g ->
      List.fold_left (fun args t -> get_args.col2_term f t @ args) ts ts
  | _ -> get_args.col2_desc_rec f desc

let () = get_args.col2_desc <- get_args_desc

let get_args = get_args.col2_term



let same_term env t1 t2 =
  if is_simple_aexp t1 && is_simple_aexp t2
  then
    let conv t = FpatInterface.conv_formula @@ snd @@ CEGAR_util.trans_term "" [] [] t in
    let env' = List.map conv env in
    let p' = conv @@ make_eq t1 t2 in
    Fpat.SMTProver.implies env' [p']
  else t1.desc = t2.desc

let make_all xs =
  let n = List.length xs in
  let aux i j =
    match Id.typ (List.nth xs i), Id.typ (List.nth xs j) with
      TInt, TInt -> [i,j]
    | _ -> []
  in
  fromto 1 n
  |> List.map (fun i -> List.flatten @@ List.map (aux (i-1)) @@ fromto i n)
  |> List.flatten

let make_env xs same_args =
  let aux (i,j) =
    let t1 = make_var @@ List.nth xs i in
    let t2 = make_var @@ List.nth xs j in
    match t1.typ, t2.typ with
      TInt, TInt -> [make_eq t1 t2]
    | _ -> []
  in
  List.flatten @@ List.map aux same_args




let get_diff_args = make_col2 [] List.rev_append

let rec get_same_args env f t args =
  let diff_args = get_diff_args.col2_term (env,f) t in
  let same_args = diff args diff_args in
  if same_args = args
  then args
  else get_same_args env f t same_args

let get_diff_args_desc (env,(f:id)) desc =
  match desc with
    App({desc=Var g}, ts) when Id.same f g ->
      let its = mapi (fun i t -> i,t) ts in
      let rec aux acc = function
          [] -> acc
        | (i,t)::its' ->
            let diff = List.map (fun (i',t') -> if same_term env t t' then [] else [i,i']) its' in
            let diff' = List.flatten diff in
            aux (diff' @ acc) its'
      in
      aux [] its
  | Let(Nonrecursive, bindings, t)
  | Let(Recursive, ([_] as bindings), t) ->
      let aux (g,xs,t') =
        let all = make_all xs in
        let same_args = get_same_args env g t all in
        let env' = make_env xs same_args in
        get_diff_args.col2_term (env'@env,f) t'
      in
      let diff_args = get_diff_args.col2_term (env,f) t in
      List.flatten (List.map aux bindings) @ diff_args
  | Let(Recursive, bindings, t) -> raise (Fatal "Not implemented (get_diff_args)")
  | _ -> get_diff_args.col2_desc_rec (env,f) desc

let () = get_diff_args.col2_desc <- get_diff_args_desc

let get_diff_args env f t = get_diff_args.col2_term (env,f) t



let elim_nth ns xs = xs
  |> mapi (fun i x -> if List.mem i ns then [] else [x])
  |> List.flatten



let elim_arg = make_trans2 ()

let elim_arg_desc ((f:id),args) desc =
  match desc with
    App({desc=Var g}, ts) when Id.same f g ->
      let ts' = List.map (elim_arg.tr2_term (f,args)) @@ elim_nth args ts in
      App(make_var g, ts')
  | _ -> elim_arg.tr2_desc_rec (f,args) desc

let () = elim_arg.tr2_desc <- elim_arg_desc

let elim_arg f args t = elim_arg.tr2_term (f,args) t




let elim_arg_typ args typ =
  match typ with
    TFun _ ->
      let xs,typ' = decomp_tfun typ in
      let xs' = elim_nth args xs in
      List.fold_right (fun x typ -> TFun(x,typ)) xs' typ'
  | _ -> (if (args<>[]) then (Format.printf "typ:%a@." pp_print_typ typ; assert false)); typ






let trans = make_trans2 ()

let trans_desc env desc =
  match desc with
    Let(_, [f,xs,{desc=Fun _}], t2) -> assert false
  | Let(flag, [f,xs,t1], t2) ->
    let same_args = get_same_args env f t2 @@ make_all xs in
    let elim_args = List.map snd same_args in
    let f' =
      let typ = elim_arg_typ elim_args @@ Id.typ f in
      Id.set_typ f typ
    in
    let xs' = elim_nth elim_args xs in
    let t1' = t1
      |> trans.tr2_term (make_env xs same_args @ env)
      |@> (fun n -> Format.printf "???1@?")
      |> subst_map @@ List.map (fun (i,j) -> List.nth xs j, make_var @@ List.nth xs i) same_args
      |@> (fun n -> Format.printf "???2@?")
      |> elim_arg f elim_args
      |> subst f (make_var f')
      |@> (fun n -> Format.printf "???3@.")
    in
    let t2' = t2
      |> trans.tr2_term env
      |> elim_arg f elim_args
      |> subst f (make_var f')
    in
    Let(flag, [f',xs',t1'], t2')
  | _ -> trans.tr2_desc_rec env desc

let () = trans.tr2_desc <- trans_desc

(** Assume that the input is in CPS *)
let trans = trans.tr2_term []
