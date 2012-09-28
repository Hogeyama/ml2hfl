open ExtList
open Term
open Formula

(** Typed substitutions *)

type t = (Var.t * Term.t * SimType.t) list

let pr_elem ppf (x, t, _) =
  Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t
let pr ppf xttys =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem " && ") xttys

let fvs_elem (x, t, _) = x :: Term.fvs t
let fvs xttys = Util.concat_map fvs_elem xttys

let fun_of xttys =
  let xttys' = List.map (fun (x, t, _) -> x, t) xttys in
  fun x -> List.assoc x xttys'

let dom xttys = List.map Util.fst3 xttys

(** @todo support other cases *)
let sub_of t =
  match t with
    Term.App(_, Term.App(_, Term.Const(_, Const.EqBool), Term.Var(_, x)), t') when not (List.mem x (Term.fvs t')) ->
      [x, t', SimType.Bool]
  | Term.App(_, Term.App(_, Term.Const(_, Const.EqInt), Term.Var(_, x)), t') when not (List.mem x (Term.fvs t')) ->
      [x, t', SimType.Int]
  | _ ->
      []

let subtract xttys xs =
  List.filter (fun (x, _, _) -> not (List.mem x xs)) xttys

let subtract_fun sub xs =
  fun x -> if List.mem x xs then raise Not_found else sub x

let rec subst sub t =
  match t with
    Term.Var(a, x) ->
      (try sub x with Not_found -> Term.Var(a, x))
  | Term.Const(a, c) ->
      Term.Const(a, c)
  | Term.App(a, t1, t2) ->
      Term.App(a, subst sub t1, subst sub t2)
  | Term.Call(_, _, _) | Term.Ret(_, _, _, _) | Term.Error(_) ->
      assert false
  | Term.Forall(a, env, t) ->
      let xs = List.map fst env in
      Term.Forall(a, env, subst (subtract_fun sub xs) t)
  | Term.Exists(a, env, t) ->
      let xs = List.map fst env in
      Term.Exists(a, env, subst (subtract_fun sub xs) t)

(** @todo compute the fixed-point of sub first *)
let subst_fixed sub t =
  let _ = Global.log_begin ~disable:true "TypSubst.subst_fixed" in
  let _ = Global.log (fun () -> Format.printf "input: %a@," Term.pr t) in
  let t = Util.fixed_point (subst sub) Term.equiv t in
  let _ = Global.log (fun () -> Format.printf "output: %a" Term.pr t) in
  let _ = Global.log_end "TermTypSubst.subst_fixed" in
  t

let rename sub t =
  subst (fun x -> List.assoc x sub) t

(** @param p every variable not satisfying p is renamed *)
let fresh p t =
  let xs = List.filter (fun x -> not (p x)) (Term.fvs t) in
  let sub = List.map (fun x -> x, Term.new_var ()) xs in
  subst (fun x -> List.assoc x sub) t

(** rename given variables to fresh ones
    @param xs variables to be renamed
    @require not (Util.is_dup xs) *)
let fresh_vars xs t =
  let sub = List.map (fun x -> x, Term.new_var ()) xs in
  subst (fun x -> List.assoc x sub) t


let elim_duplicate xttys =
  let xttys, tss =
    List.split
      (List.map
        (fun (((_, t1, ty1) as xtty) :: xttys) ->
          let ts =
            List.map
              (fun (_, t2, ty2) -> eq_tty (t1, ty1) (t2, ty2))
              xttys
          in
          xtty, ts)
        (Util.classify (fun (x1, _, _) (x2, _, _) -> x1 = x2) xttys))
  in
  xttys, List.flatten tss





let xtty_of p dom t =
  try
    ParLinArith.xtty_of_aif p dom (ParLinArith.aif_of t)
  with Invalid_argument _ ->
    (match fun_args t with
      Const(_, Const.EqUnit), [Var(_, x); t] when not (p x) && Util.inter (x::dom) (Term.fvs t) = [] ->
        x, t, SimType.Unit
    | Const(_, Const.EqUnit), [t; Var(_, x)] when not (p x) && Util.inter (x::dom) (Term.fvs t) = [] ->
        x, t, SimType.Unit
    | Const(_, Const.EqBool), [Var(_, x); t] when not (p x) && Util.inter (x::dom) (Term.fvs t) = [] ->
        x, t, SimType.Bool
    | Const(_, Const.EqBool), [t; Var(_, x)] when not (p x) && Util.inter (x::dom) (Term.fvs t) = [] ->
        x, t, SimType.Bool
    | Var(_, x), []                    when not (p x) ->
        x, ttrue, SimType.Bool
    | Const(_, Const.Not), [Var(_, x)] when not (p x) ->
        x, tfalse, SimType.Bool
    | _ ->
        raise Not_found)


(** @param pids specifies the priority *)
let extract_from pids p t =
  let ts = conjuncts t in
  let eqcs, ts =
    Util.partition_map
      (fun t ->
        match Term.fun_args t with
          Term.Const(_, Const.EqUnit), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Unit)
        | Term.Const(_, Const.EqBool), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Bool)
        | Term.Const(_, Const.EqInt), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Int)
        | _ -> `R(t))
      ts
  in
  let eqcs =
    let rec aux eqcs1 eqcs2 =
      match eqcs2 with
        [] -> eqcs1
      | eqc2::eqcs2' ->
          let eqcs1' =
            let flag = ref false in
            let eqcs =
              List.map
                (fun eqc1 ->
                  if Util.inter (fst eqc2) (fst eqc1) <> [] then
                    let _ = assert (snd eqc1 = snd eqc2) in
                    let _ = flag := true in
                    List.unique (fst eqc1 @ fst eqc2), snd eqc1
                  else
                    eqc1)
                eqcs1
            in
            if !flag then eqcs else eqc2 :: eqcs
          in
          aux eqcs1' eqcs2'
    in
    Util.fixed_point (aux []) (fun eqcs1 eqcs2 -> List.length eqcs1 = List.length eqcs2) eqcs
  in
  let ts', sub =
    Util.flatten_unzip
      (List.map
        (fun (eqc, ty) ->
          let xs1, xs2 = List.partition p eqc in
          match xs1 with
            [] ->
              [], List.map (fun x -> x, Term.make_var (List.hd xs2), ty) (List.tl xs2)
          | x::xs ->
              let x, xs =
                try
                  let pid = List.find (fun pid -> List.mem pid xs1) pids in
                  pid , Util.diff xs1 [pid]
                with Not_found -> x, xs
              in
              List.map (fun x' -> eq_tty (Term.make_var x, ty) (Term.make_var x', ty)) xs,
              List.map (fun x' -> x', Term.make_var x, ty) xs2)
        eqcs)
  in
  fun_of sub, band (ts @ ts')

(** may return a substitution of the form {x -> y, y -> z}
    unsound for non linear expressions? maybe not *)
let extract_from2 pvs p ts =
  let nlfvs = LinArith.nlfvs (band ts) in
  let rec aux ts xttys0 ts0 =
    match ts with
      [] -> xttys0, ts0
    | t::ts' ->
        let xttys0, ts0 =
          try
            let dom = List.map Util.fst3 xttys0 in
            let xtty = xtty_of p dom t in
            let xtty =
              (*Format.printf "xtty: %a@,nlfvs: %a@,pvs: %a@," pr_elem xtty Var.pr_list nlfvs Var.pr_list pvs;*)
              if List.mem (Util.fst3 xtty) nlfvs && not (is_linear (Util.snd3 xtty)) ||
                 List.mem (Util.fst3 xtty) pvs && Term.coeffs (Util.snd3 xtty) <> [] (*|| t is constant*) then
                raise Not_found
              else
                xtty
            in
            xtty::xttys0, ts0
          with Not_found ->
            xttys0, t::ts0
        in
        aux ts' xttys0 ts0
  in
  let xttys0, ts0 = aux ts [] [] in
  let xttys1, ts1 = elim_duplicate xttys0 in
  xttys1, band (ts0 @ ts1)
