open Util
open Syntax
open Term_util
open Type
open Fair_termination_type


let print_fairness fm fairness =
  let pr fm (a,b) = Format.printf "(%s, %s)" a b in
  Format.fprintf fm "@[<hov 1>{%a}@]" (print_list pr ",@ ") fairness

let print_rank_fun xs fm {coeffs;const} =
  let xs' = List.map Option.some xs @ [None] in
  let pr fm (c,x) =
    match x with
    | Some x' -> Format.fprintf fm "%d*%a" c Id.print x'
    | None -> Format.fprintf fm "%d" c
  in
  Format.fprintf fm "%a" (print_list pr " + ") @@ List.combine (coeffs@[const]) xs'

let event_fun = "event"
let is_event_fun_var x = Id.name x = event_fun


let add_event s = Format.sprintf "let %s (s:string) = ();;\n\n%s" event_fun s

let rec is_value t =
  match t.desc with
  | Var _
  | Const _ -> true
  | BinOp(op, t1, t2) -> is_value t1 && is_value t2
  | _ -> false

let make_s_init fairness =
  make_tuple @@ List.make (List.length fairness) (make_pair false_term false_term)

(* The current FPAT support only int arguments for rank_fun functions *)
let is_ground_typ typ =
  match typ with
  | TInt -> true
  | _ -> false


let apply_rank_fun prev_variables variables {coeffs; const} =
  let rank xs =
    let mul n x = make_mul (make_int n) (make_var x) in
    List.fold_right make_add (List.map2 mul coeffs xs) (make_int const)
  in
  let previous = rank prev_variables in
  let current = rank variables in
  (* R(p_xs) > R(xs) && R(xs) >= 0 *)
  make_and (make_gt previous current) (make_geq current (make_int 0))

let make_check_rank ps xs rank_funs =
  make_ors @@ List.map (apply_rank_fun ps xs) rank_funs




(** remove the definition of "event" introduced by add_event in Mochi.main *)
(** and replace App("event", "P") with App(Event("P"), unit) *)
let remove_and_replace_event = make_trans ()
let remove_and_replace_event_desc desc =
  match desc with
  | App({desc = Var f}, ts) when is_event_fun_var f ->
      begin
        match ts with
        | [{desc = Const (String s)}] -> (make_event_unit s).desc
        | _ -> unsupported "the argument of event must be a constant"
      end
  | Let(_, [f, [_], _], t') when is_event_fun_var f -> (remove_and_replace_event.tr_term t').desc
  | _ -> remove_and_replace_event.tr_desc_rec desc
let () = remove_and_replace_event.tr_desc <- remove_and_replace_event_desc
let remove_and_replace_event = remove_and_replace_event.tr_term




(** normalization for redection of fair termination *)
let normalize = make_trans ()

let normalize_aux t =
  if is_value t
  then [], t
  else
    let x = new_var_of_term t in
    [x, [], t], make_var x

let normalize_term t =
  let t' = normalize.tr_term_rec t in
  match t'.desc with
  | BinOp(op, t1, t2) ->
      let bind1, t1' = normalize_aux t1 in
      let bind2, t2' = normalize_aux t2 in
      make_lets (bind1 @ bind2) {t with desc=BinOp(op,t1',t2')}
  | App({desc=Event(q, _)}, [_]) -> t
  | App(t1, ts) ->
      let bind, t1' = normalize_aux t1 in
      let binds, ts' = List.split_map normalize_aux ts in
      let aux ctx t = fun t' ->
        let tf = ctx t in
        let f = Id.new_var ~name:"f" tf.typ in
        make_let [f,[],tf] @@ make_app (make_var f) [t']
      in
      let ts'',t2 = List.decomp_snoc ts' in
      let t'' = List.fold_left aux (fun t -> make_app t1' [t]) ts'' t2 in
      make_lets (bind @ List.flatten binds) t''
  | If(t1, t2, t3) ->
      let bind, t1' = normalize_aux t1 in
      make_let bind {t with desc=If(t1', t2, t3)}
  | _ -> t'

let () = normalize.tr_term <- normalize_term
let normalize = normalize.tr_term -| Trans.short_circuit_eval



(** add event "Call" for the body of every function definition *)
let add_call = make_trans ()

let add_call_desc desc =
  match desc with
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let t' = add_call.tr_term t in
        let t'' = if xs = [] then t' else make_seq (make_event_unit "Call") t' in
        f, xs, t''
      in
      let bindings' = List.map aux bindings in
      Let(flag, bindings', add_call.tr_term t2)
  | _ -> add_call.tr_desc_rec desc

let () = add_call.tr_desc <- add_call_desc
let add_call = add_call.tr_term



(** insert extra parameters *)
let make_extra_param xs =
  let mk = make_var -| make_extra_coeff in
  List.fold_left (fun acc x -> make_add acc @@ make_mul !!mk (make_var x)) !!mk xs
let is_fun_var = is_fun_typ -| Id.typ
let new_exparam () = Id.new_var ~name:"ex" TInt

let insert_extra_param = make_trans2 ()

let insert_extra_param_desc vars desc =
  match desc with
  | Fun(x, t) when is_fun_var x ->
      let x' = insert_extra_param.tr2_var vars x in
      Fun(!!new_exparam, make_fun x' @@ insert_extra_param.tr2_term vars t)
  | Let(flag, bindings, t2) ->
      let aux (f,xs,t) =
        let f' = insert_extra_param.tr2_var vars f in
        let xs' = List.map (insert_extra_param.tr2_var vars) xs in
        let xs'' = List.flatten_map (fun x -> if is_fun_var x then [!!new_exparam; x] else [x]) xs' in
        let vars' = List.filter ((=) TInt -| Id.typ) xs @ vars in
        f', xs'', insert_extra_param.tr2_term vars' t
      in
      let bindings' = List.map aux bindings in
      let vars' = List.filter_map (fun (x,_,_) -> if Id.typ x = TInt then Some x else None) bindings @ vars in
      Let(flag, bindings', insert_extra_param.tr2_term vars' t2)
  | App(t1, ts) ->
      let t1' = insert_extra_param.tr2_term vars t1 in
      let ts' = List.map (insert_extra_param.tr2_term vars) ts in
      let ts'' = List.flatten_map (fun t -> if is_fun_typ t.typ then [make_extra_param vars; t] else [t]) ts' in
      App(t1', ts'')
  | _ -> insert_extra_param.tr2_desc_rec vars desc

let insert_extra_param_typ vars typ =
  match typ with
  | TFun(x, typ2) when is_fun_var x -> TFun(Id.new_var TInt, insert_extra_param.tr2_typ_rec vars typ)
  | _ -> insert_extra_param.tr2_typ_rec vars typ

let () = insert_extra_param.tr2_desc <- insert_extra_param_desc
let () = insert_extra_param.tr2_typ <- insert_extra_param_typ
let insert_extra_param = insert_extra_param.tr2_term []


let set_main t =
  match get_last_definition t with
  | None -> unsupported "fair_termination: set_main"
  | Some f ->
      let xs = get_args (Id.typ f) in
      List.iter (fun x -> assert (Id.typ x = TInt)) xs;
      let main = make_app (make_var f) @@ List.map (Fun.const randint_unit_term) xs in
      let main' = make_let [new_var_of_term main, [], main] unit_term in
      Trans.replace_main main' t


let get_states = make_col [] (@@@)
let get_states_desc desc =
  if desc = fail_term.desc then
    []
  else
    match desc with
    | Event(q, _) -> [q]
    | _ -> get_states.col_desc_rec desc
let () = get_states.col_desc <- get_states_desc
let get_states = get_states.col_term
