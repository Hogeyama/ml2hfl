open ExtList
open Util
open Term
open Formula

(** Typed substitutions *)

type t = (Var.t * Term.t * SimType.t) list

(** {6 Printers} *)

let pr_elem ppf (x, t, _) =
  Format.fprintf ppf "%a -> %a" Var.pr x Term.pr t
let pr ppf xttys =
  Format.fprintf ppf "%a" (Util.pr_list pr_elem " && ") xttys

(** {6 Basic functions} *)

let fvs_elem (x, t, _) = x :: Term.fvs t
let fvs xttys = Util.concat_map fvs_elem xttys

let sub_of xttys =
  let xttys' = List.map (fun (x, t, _) -> x, t) xttys in
  fun x -> List.assoc x xttys'

let dom xttys = List.map Util.fst3 xttys

let cyclic xttys = raise (NotImplemented "TypSubst.cyclic")

let subtract xttys xs =
  List.filter (fun (x, _, _) -> not (List.mem x xs)) xttys

let subtract_fun sub xs =
  fun x -> if List.mem x xs then raise Not_found else sub x

(** @require is_valid t iff t is valid *)
let non_dup ?(is_valid = fun t -> assert false) xttys =
  let _ = Global.log_begin ~disable:true "TypSubst.is_dup" in
  let b =
    List.for_all
      (function
        [] -> assert false
      | (_, t, ty) :: xttys ->
          let _ = Global.log (fun () -> Format.printf "ts: %a, " Term.pr t) in
          let b =
            List.for_all
              (fun (_, t', ty') ->
                let _ = Global.log (fun () -> Format.printf "%a, " Term.pr t') in
                is_valid (Formula.eq_tty (t, ty) (t', ty')))
              xttys
          in
          let _ = Global.log (fun () -> Format.printf "@,") in
          b)
      (Util.classify (fun (x, _, _) (y, _, _) -> x = y) (List.unique xttys))
  in
  let _ = Global.log_end "TypSubst.non_dup" in
  b

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

(** @return (x, t, ty)
    @ensure pred x t ty *)
let xtty_of_formula pred t =
  try
    ParLinArith.xtty_of_aif pred (ParLinArith.aif_of t)
  with Invalid_argument _ ->
    (match fun_args t with
      Const(_, Const.EqUnit), [Var(_, x); t] when pred x t SimType.Unit ->
        x, t, SimType.Unit
    | Const(_, Const.EqUnit), [t; Var(_, x)] when pred x t SimType.Unit ->
        x, t, SimType.Unit
    | Const(_, Const.EqBool), [Var(_, x); t] when pred x t SimType.Bool ->
        x, t, SimType.Bool
    | Const(_, Const.EqBool), [t; Var(_, x)] when pred x t SimType.Bool ->
        x, t, SimType.Bool
    | Var(_, x), []                    when pred x ttrue SimType.Bool ->
        x, ttrue, SimType.Bool
    | Const(_, Const.Not), [Var(_, x)] when pred x tfalse SimType.Bool ->
        x, tfalse, SimType.Bool
    | _ ->
        raise Not_found)

(** possibly return a substitution of the form {x -> y, y -> z}
    @param pred do not require that mem x (fvs t) implies not (pred x t ty)
    @todo check whether it is unsound for non-linear expressions
    @return (xttys, ts)
    @ensure not (cyclic xttys) &&
            List.for_all (fun (x, t, ty) -> pred x t ty) xttys *)
let xttys_of pred ts =
  let _ = Global.log_begin ~disable:true "TypSubst.xttys_of" in
  let xttys0, ts0 =
    List.fold_left
      (fun (xttys0, ts0) t ->
        try
          let xtty =
            let pred =
              let dom = dom xttys0 in
              fun x t ty ->
                pred x t ty &&
                (* @todo check whether substitution is acyclic instead *)
                Util.inter (x :: dom) (Term.fvs t) = []
            in
            let xtty = xtty_of_formula pred t in
            let _ = Global.log (fun () ->
              Format.printf "xtty: %a@," pr_elem xtty)
            in
            xtty
          in
          xtty :: xttys0, ts0
        with Not_found ->
          xttys0, t :: ts0)
      ([], [])
      ts
  in
  let xttys1, ts1 = elim_duplicate xttys0 in
  let res = xttys1, band (ts0 @ ts1) in
  let _ = Global.log_end "TypSubst.xttys_of" in
  res

(** @param pids if possible, a variables in pids is used in ts and the range of xttys
    @return (xttys, ts)
    @ensure List.for_all (fun x -> not (p x)) (dom xttys) *)
let xytys_of pids p t =
    (conjuncts t)
  |>
    (Util.partition_map
      (fun t ->
        match Term.fun_args t with
          Term.Const(_, Const.EqUnit), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Unit)
        | Term.Const(_, Const.EqBool), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Bool)
        | Term.Const(_, Const.EqInt), [Term.Var(_, x1); Term.Var(_, x2)] ->
            `L([x1; x2], SimType.Int)
        | _ -> `R(t)))
  |>
      ((Util.fixed_point
        (List.fold_left
          (fun eqcs eqc2 ->
            let updated = ref false in
            let eqcs' =
              List.map
                (fun eqc1 ->
                  if Util.inter (fst eqc2) (fst eqc1) <> [] then
                    let _ = if !Global.debug then assert (snd eqc1 = snd eqc2) in
                    let _ = updated := true in
                    List.unique (fst eqc1 @ fst eqc2), snd eqc1
                  else
                    eqc1)
                eqcs
            in
            if !updated then eqcs' else eqc2 :: eqcs')
          [])
        (fun eqcs1 eqcs2 -> List.length eqcs1 = List.length eqcs2))
    ++
      id)
  |>
        (((List.map
          (fun (eqc, ty) ->
            let xs1, xs2 = List.partition p eqc in
            if xs1 = [] then
              [], List.map (fun x -> x, Term.make_var (List.hd xs2), ty) (List.tl xs2)
            else
              let x, xs =
                try
                  let pid = List.find (fun pid -> List.mem pid xs1) pids in
                  pid , Util.diff xs1 [pid]
                with Not_found -> List.hd xs1, List.tl xs1
              in
              List.map (fun x' -> eq_tty (Term.make_var x, ty) (Term.make_var x', ty)) xs,
              List.map (fun x' -> x', Term.make_var x, ty) xs2))
      |-
        unzip
      |-
        (List.flatten ++ List.flatten))
    ++
      id)
  |>
    (fun ((ts', xttys), ts) -> xttys, band (ts @ ts'))
