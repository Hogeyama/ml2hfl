open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util




exception EvalFail
exception EvalValue
exception Skip
exception Restart

let assoc_def defs n t =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
    List.nth defs' n

let rec is_value env = function
    Const Bottom -> false
  | Const RandBool -> false
  | Const _ -> true
  | Var x -> get_arg_num (get_typ env (Var x)) > 0
  | App(App(App(Const If, _), _), _) -> false
  | App _ as t ->
      let t1,ts = decomp_app t in
        List.for_all (is_value env) (t1::ts) && get_arg_num (get_typ env t) = List.length ts
  | Let _ -> assert false
  | Fun _ -> assert false

let rec read_bool () =
  Format.printf "RandBool (t/f/r/s): ";
  match read_line () with
    | s when String.length s = 0 -> read_bool ()
    | s when s.[0] = 't' -> true
    | s when s.[0] = 'f' -> false
    | s when s.[0] = 'r' -> raise Restart
    | s when s.[0] = 's' -> raise Skip
    | s -> read_bool ()

let rec step_eval_abst_cbn ce env_orig env_abst defs = function
    Const Bottom -> raise TypeBottom
  | Const RandBool ->
      let t =
        if read_bool ()
        then Const True
        else Const False
      in
        ce, t
  | Var x ->
      let ce',(f,xs,tf1,es,tf2) =
        if List.exists (fun (f,_) -> f = x) env_orig
        then List.tl ce, assoc_def defs (List.hd ce) (Var x)
        else ce, assoc_def defs 0 (Var x)
      in
        assert (tf1 = Const True);
        if List.mem (Event "fail") es then raise EvalFail;
        ce', tf2
  | App(App(App(Const If, Const True), t2), _) -> ce, t2
  | App(App(App(Const If, Const False), _), t3) -> ce, t3
  | App(App(App(Const If, t1), t2), t3) ->
      let ce',t1' = step_eval_abst_cbn ce env_orig env_abst defs t1 in
        ce', App(App(App(Const If, t1'), t2), t3)
  | App _ as t ->
      let t1,ts = decomp_app t in
        if t1 = Const If
        then
          match ts with
              t1::t2::t3::ts' ->
                let t2' = make_app t2 ts' in
                let t3' = make_app t3 ts' in
                  step_eval_abst_cbn ce env_orig env_abst defs (make_if t1 t2' t3')
            | _ -> assert false
        else
          let ce',(f,xs,tf1,es,tf2) =
            if List.exists (fun (f,_) -> Var f = t1) env_orig
            then List.tl ce, assoc_def defs (List.hd ce) t1
            else ce, assoc_def defs 0 t1
          in
          let ts1,ts2 = take2 ts (List.length xs) in
            assert (tf1 = Const True);
            if List.mem (Event "fail") es then raise EvalFail;
            ce', make_app (List.fold_right2 subst xs ts1 tf2) ts2
  | _ -> assert false

let rec eval_abst_cbn prog abst ce =
  Format.printf "Program with abstraction types::@.%a@." CEGAR_print.print_prog_typ abst;
  let env_orig = get_env prog in
  let env_abst = get_env abst in
  let defs = get_defs abst in
  let main = get_main abst in
  let ce' = ce in
  let rec loop ce t =
    Format.printf "%a -->@\n" print_term t;
    assert (match get_typ env_abst t with TBase(TUnit,_) -> true | _ -> false);
    let () =
      try
        match decomp_app t with
            Var x, _ ->
              Format.printf "  %s:: %a@\n" x print_typ (List.assoc x env_orig)
          | _ -> ()
      with Not_found -> ()
    in
    let ce',t' = step_eval_abst_cbn ce env_orig env_abst defs t in
      if t' <> Const Unit
      then loop ce' t'
  in
  let pr () =
    try
      loop ce' (Var main)
    with(*
          Failure "nth" ->
          Format.printf "RESET (inconsistent)@.@.";
          eval_abst_cbn prog abst ce*)
      | Restart -> eval_abst_cbn prog abst ce
      | EvalFail ->
          Format.printf "ERROR!@.@.";
          Format.printf "Press Enter.@.";
          ignore (read_line())
      | TypeBottom ->
          Format.printf "DIVERGE!@.@.";
          Format.printf "Press Enter.@.";
          ignore (read_line())
  in
    try
      Format.printf "Evaluation of abstracted program::@.  @[";
      pr ();
    with Skip -> ()






let rec get_nonrec main defs =
  let aux (f,_,t1,e,_) =
    f <> main &&
    t1 = Const True &&
    e = [] &&
    1 >= count_list (fun (_,_,t1,_,t2) -> List.mem f (get_fv t1 @@ get_fv t2)) defs &&
    1 >= count_list (fun (g,_,_,_,_) -> f = g) defs
  in
  let defs' = List.filter aux defs in
    List.map (fun (f,xs,_,_,t) -> f, List.fold_right (fun x t -> Fun(x,None,t)) xs t) defs'

let rec beta_reduce_term = function
    Const c -> Const c
  | Var x -> Var x
  | App(t1, t2) ->
      let t1' = beta_reduce_term t1 in
      let t2' = beta_reduce_term t2 in
        begin
          match t1' with
              Fun(x,_,t1') -> beta_reduce_term (subst x t2' t1')
            | _ -> App(t1', t2')
        end
  | Fun(x, typ, t) -> Fun(x, typ, beta_reduce_term t)
  | Let _ -> assert false
let beta_reduce_def (f,xs,t1,e,t2) =
  f, xs, beta_reduce_term t1, e, beta_reduce_term t2

let rec expand_nonrec (env,defs,main) =
  let nonrec = get_nonrec main defs in
  let aux (f,xs,t1,e,t2) = f, xs, subst_map nonrec t1, e, subst_map nonrec t2 in
  let rec loop defs =
    let defs' = List.map aux defs in
      if defs = defs'
      then defs
      else loop defs'
  in
  let defs' = List.filter (fun (f,_,_,_,_) -> not (List.mem_assoc f nonrec)) defs in
  let defs'' = loop defs' in
  let defs''' = List.map beta_reduce_def defs'' in
    (env,defs''',main)





let assoc_def labeled defs ce acc t =
  let f = match t with Var f -> f | _ -> assert false in
  let defs' = List.filter (fun (g,_,_,_,_) -> g = f) defs in
    if List.mem f labeled
    then
      let c = List.hd ce in
      let ce' = List.tl ce in
      let acc' = c::acc in
      let def = List.nth defs' c in
        ce', acc', def
    else
      let acc' = 0::acc in
      let def = List.hd defs' in
        assert (List.length defs' = 1);
        ce, acc', def

let init_cont ce acc _ = assert (ce=[]); List.rev acc

let rec trans_ce_aux labeled ce acc defs t k =
  if false then Format.printf "trans_ce_aux[%d,%d]: %a@." (List.length ce) (List.length acc) print_term t;
  match t with
    | Const RandInt -> assert false
    | Const c -> k ce acc (Const c)
    | Var x -> k ce acc (Var x)
    | App(App(Const (And|Or|Lt|Gt|Leq|Geq|EqUnit|EqBool|EqInt|Add|Sub|Mul as op),t1),t2) ->
        trans_ce_aux labeled ce acc defs t1 (fun ce acc t1 ->
        trans_ce_aux labeled ce acc defs t2 (fun ce acc t2 ->
          k ce acc (make_app (Const op) [t1;t2])))
    | App(Const RandInt, t) ->
        let r = new_id "r" in
          trans_ce_aux labeled ce acc defs (App(t,Var r)) k
    | App(t1,t2) ->
        trans_ce_aux labeled ce acc defs t1 (fun ce acc t1 ->
        trans_ce_aux labeled ce acc defs t2 (fun ce acc t2 ->
          let t1',ts = decomp_app (App(t1,t2)) in
          let _,xs,_,_,_ = List.find (fun (f,_,_,_,_) -> Var f = t1') defs in
            if List.length xs > List.length ts
            then k ce acc (App(t1,t2))
            else
              let ce',acc',(f,xs,tf1,e,tf2) = assoc_def labeled defs ce acc t1' in
              let ts1,ts2 = take2 ts (List.length xs) in
              let aux = List.fold_right2 subst xs ts1 in
              let tf2' = make_app (aux tf2) ts2 in
                assert (List.length xs = List.length ts);
                if e = [Event "fail"]
                then init_cont ce' acc' tf2'
                else trans_ce_aux labeled ce' acc' defs tf2' k))
    | Let _ -> assert false
    | Fun _ -> assert false

let trans_ce ce labeled ((env,defs,main):prog) =
  let _,_,_,_,t = List.find (fun (f,_,_,_,_) -> f = main) defs in
  let ce' = trans_ce_aux labeled ce [] defs t init_cont in
    assert (not (List.mem main labeled));
    0::ce'

