open Utilities
open CEGAR_syntax
open CEGAR_type
open CEGAR_print
open CEGAR_util




exception EvalFail
exception EvalValue
exception EvalSkip
exception EvalRestart
exception EvalTerminate

let assoc_def_aux defs n t =
  let defs' = List.filter (fun (f,_,_,_,_) -> Var f = t) defs in
    List.nth defs' n

let assoc_def labeled t ce defs =
  if List.exists (fun f -> Var f = t) labeled
  then List.tl ce, assoc_def_aux defs (List.hd ce) t
  else ce, assoc_def_aux defs 0 t


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
  Format.printf "RandBool (t/f/r/s): @?";
  match read_line () with
    | "t" -> true
    | "f" -> false
    | "r" -> raise EvalRestart
    | "s" -> raise EvalSkip
    | s -> read_bool ()

let rec step_eval_abst_cbn ce labeled env_abst defs = function
    Const Bottom -> raise TypeBottom
  | Const RandBool ->
      let t =
        if read_bool ()
        then Const True
        else Const False
      in
        ce, t
  | Const Unit -> ce, Const Unit
  | Var x ->
      let ce',(f,xs,tf1,es,tf2) = assoc_def labeled (Var x) ce defs in
        assert (tf1 = Const True);
        if List.mem (Event "fail") es then raise EvalFail;
        ce', tf2
  | App(App(App(Const If, Const True), t2), _) -> ce, t2
  | App(App(App(Const If, Const False), _), t3) -> ce, t3
  | App(App(App(Const If, t1), t2), t3) ->
      let ce',t1' = step_eval_abst_cbn ce labeled env_abst defs t1 in
        ce', App(App(App(Const If, t1'), t2), t3)
  | App(Const (Label n), t) ->
      step_eval_abst_cbn ce labeled env_abst defs t
  | App _ as t ->
      let t1,ts = decomp_app t in
        if t1 = Const If
        then
          match ts with
              t1::t2::t3::ts' ->
                let t2' = make_app t2 ts' in
                let t3' = make_app t3 ts' in
                  step_eval_abst_cbn ce labeled env_abst defs (make_if t1 t2' t3')
            | _ -> assert false
        else
          let ce',(f,xs,tf1,es,tf2) = assoc_def labeled t1 ce defs in
          let ts1,ts2 = take2 ts (List.length xs) in
            assert (tf1 = Const True);
            if List.mem (Event "fail") es then raise EvalFail;
            ce', make_app (List.fold_right2 subst xs ts1 tf2) ts2
  | _ -> assert false

let rec eval_abst_cbn prog labeled abst ce =
  Format.printf "Program with abstraction types::@.%a@." CEGAR_print.prog abst;
  Format.printf "CE: %a@." CEGAR_print.ce ce;
  let env_orig = get_env prog in
  let env_abst = get_env abst in
  let defs = get_defs abst in
  let main = get_main abst in
  let ce' = ce in
  let rec loop ce t =
    Format.printf "%a -->@\n" CEGAR_print.term t;
    assert (match get_typ env_abst t with TBase(TUnit,_) -> true | _ -> false);
    let () =
      try
        match decomp_app t with
            Var x, _ ->
              Format.printf "  %s:: %a@\n" x CEGAR_print.typ (List.assoc x env_orig)
          | _ -> ()
      with Not_found -> ()
    in
    let ce',t' = step_eval_abst_cbn ce labeled env_abst defs t in
      if t' = Const Unit then raise EvalTerminate;
      loop ce' t'
  in
  let rec confirm () =
    Format.printf "(s)kip/(r)estart: @?";
    match read_line () with
      | "s" -> ()
      | "r" -> eval_abst_cbn prog labeled abst ce
      | s -> confirm ()
  in
    Format.printf "Evaluation of abstracted program::@.";
    try
      loop ce' (Var main)
    with
        EvalRestart -> eval_abst_cbn prog labeled abst ce
      | EvalFail ->
          Format.printf "ERROR!@.@.";
          confirm ()
      | EvalSkip -> ()
      | EvalTerminate ->
          Format.printf "TERMINATES!@.@.";
          confirm ()
      | TypeBottom ->
          Format.printf "DIVERGES!@.@.";
          confirm ()







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
  if false then Format.printf "trans_ce_aux[%d,%d]: %a@." (List.length ce) (List.length acc) CEGAR_print.term t;
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




