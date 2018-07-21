open Combinator
open Util

include Term

(** {6 Auxiliary constructors} *)

let mk_fix_pat (pat, ty) e = mk_binder (Binder.Fix ty) pat e
let mk_fix (x, ty) e = mk_binder (Binder.Fix ty) (Pattern.mk_var x) e

let mk_fun_pat ptys e =
  List.fold_right
    (fun (p, ty) -> mk_binder (Binder.Lambda ty) p)
    ptys e
let mk_fun xtys e = mk_fun_pat (List.map (Pair.map_fst Pattern.mk_var) xtys) e

let mk_if ty e1 e2 e3 =
  mk_app
    (mk_const (Const.ML_If(ty)))
    [e1;
     mk_fun_pat [Pattern.W, Type.mk_unit] e2;
     mk_fun_pat [Pattern.W, Type.mk_unit] e3]
let mk_cases ty ees e =
  List.fold_right
    (fun (e1, e2) e3 -> mk_if ty e1 e2 e3)
    ees
    e

let mk_let_pat ty pty e1 e2 =
  mk_app (mk_const (Const.ML_Let(ty))) [e1; mk_fun_pat [pty] e2]
let mk_let ty_bd (x, ty) e1 e2 = mk_let_pat ty_bd (Pattern.mk_var x, ty) e1 e2
(** @require e1 and e2 are of the type unit *)
let seq e1 e2 =
  if e1 = UnitTerm.make then e2
  else if e2 = UnitTerm.make then e1
  else mk_let Type.mk_unit (Idnt.new_var (), Type.mk_unit) e1 e2

let mk_letrec_pat fds e =
  let fs = List.map Triple.fst fds in
  mk_app
    (mk_const (Const.ML_LetRec(List.length fds)))
    [List.fold_right
       mk_fix_pat
       fs
       (mk_app
          (mk_const Const.ML_LetAnd)
          (List.map (fun (_, xtys, e) -> mk_fun_pat xtys e) fds));
     mk_fun_pat fs e]
let mk_letrec fds e =
  let fs = List.map Triple.fst fds in
  mk_app
    (mk_const (Const.ML_LetRec(List.length fds)))
    [List.fold_right
       mk_fix
       fs
       (mk_app
          (mk_const Const.ML_LetAnd)
          (List.map (fun (_, xtys, e) -> mk_fun xtys e) fds));
     mk_fun fs e]
let mk_letrec_simple fds e =
  let rec loop e = function
    | [] -> e
    | xty :: xtys ->
      let d = Idnt.new_var () in
      mk_letrec [(d, Type.mk_top), [xty], loop e xtys] (mk_var d)
  in
  mk_letrec
    (List.map
       (fun (xty, xtys, e) ->
          if List.length xtys <= 1
          then xty, xtys, e
          else xty, [List.hd xtys], loop e (List.tl xtys))
       fds)
    e

let mk_recfun ty xty xtys e1 = mk_let ty xty (mk_fix xty (mk_fun xtys e1))
let mk_recfun_f ty xty f = mk_let ty xty (mk_fix xty f)
let mk_closure env xty e =
  mk_app
    (mk_const (Const.ML_Closure(List.length env)))
    (env @ [mk_fun [xty] e])

let mk_undef = mk_const Const.Undef
let mk_bot = mk_const Const.Bot
let mk_top = mk_const Const.Top

let mk_rand ty xty e = mk_let ty xty (mk_const Const.RandInt) e
let mk_rand_bool = mk_const Const.RandBool
let mk_rand_int = mk_const Const.RandInt
let mk_rand_real = mk_const Const.RandReal
(*
let mk_read_int (id,ty) e =
  (\* assume "let id = read_int() in e" form *\)
*)
(*
   mk_let xty (mk_const @@ Const.ReadInt(Idnt.mk_var id, [ty]))
*)
let mk_read_bool = mk_const (Const.ReadBool(Idnt.new_var (), []))
let mk_read_int = mk_const (Const.ReadInt(Idnt.new_var (), []))
let mk_read_real = mk_const (Const.ReadReal(Idnt.new_var (), []))

let mk_event id e = mk_app (mk_const (Const.Event(id))) [e]
let mk_fail = mk_event Const.event_fail UnitTerm.make
let mk_end = mk_event Const.event_end UnitTerm.make

let mk_assert = function
  | Const(Const.False) -> mk_fail
  | e -> mk_if Type.mk_unit e UnitTerm.make mk_fail

let rec of_pattern ctenv p ty =
  match p, Type.fun_args ty with
  | Pattern.V x, _ -> Term.mk_var x
  | Pattern.U, _ -> UnitTerm.make
  | Pattern.W, _ -> Term.mk_var (Idnt.new_var ())
  | Pattern.T pl, (Type.Const (TypConst.Tuple _), tys) ->
    TupTerm.make tys (List.map2 (of_pattern ctenv) pl tys)
  | Pattern.K(c, p), (Type.Const (TypConst.Adt _), []) ->
    let tyc = List.assoc_fail c ctenv in
    ADTTerm.mk_kon
      (c, tyc)
      (List.map2 (of_pattern ctenv) (Pattern.list_of p) (tyc |> Type.args_of))
  | _, _ ->
    Format.printf
      "error in MLExp.of_pattern:@.  p = %a@.  ty = %a@."
      Pattern.pr p
      Type.pr ty;
    assert false

let rec name_of_proj = function
  | [] -> ""
  | (m, n) :: path ->
    "java_" ^ string_of_int m ^ "_" ^ string_of_int n ^ name_of_proj path
let name_of_proj = name_of_proj >> Idnt.make
let name_of_id = Idnt.make "java_id"

let mk_church_tuple es =
  let x = Idnt.new_var () in
  mk_fun [x, Type.mk_top] (mk_app (mk_var x) es)
let rec mk_church_proj = function
  | [m, n] ->
    let xtys = List.gen m (fun _ -> Idnt.new_var (), Type.mk_top) in
    mk_fun xtys (mk_var (List.nth xtys (n - 1) |> fst))
  | (m, n) :: path ->
    let xtys = List.gen m (fun _ -> Idnt.new_var (), Type.mk_top) in
    mk_fun xtys
      (mk_app
         (mk_var (List.nth xtys (n - 1) |> fst))
         [mk_church_proj path])
  | _ -> assert false
let mk_church_proj_def path =
  match path with
  | [m, n] ->
    let xtys =
      List.map
        (fun _ -> Idnt.new_var (), Type.mk_top)
        (List.from_to 1 m)
    in
    (name_of_proj path, Type.mk_top),
    xtys,
    mk_var (List.nth xtys (n - 1) |> fst)
  | (m, n) :: path' ->
    let xtys =
      List.map
        (fun _ -> Idnt.new_var (), Type.mk_top)
        (List.from_to 1 m)
    in
    (name_of_proj path, Type.mk_top),
    xtys,
    mk_app
      (mk_var (List.nth xtys (n - 1) |> fst))
      [name_of_proj path' |> mk_var]
  | _ -> assert false

let mk_church_fst e = mk_app e [mk_var (name_of_proj [2,1])]
let mk_church_snd e = mk_app e [mk_var (name_of_proj [2,2])]
let mk_id =
  let xty = Idnt.new_var (), Type.mk_top in
  mk_fun [xty] (xty |> fst |> mk_var)

(** {6 Auxiliary destructors} *)

let is_closure = fun_args >> function
  | Const(Const.ML_Closure(n)), es when List.length es = n + 1 -> true
  | _ -> false

let let_fix e f =
  let_binder e (fun (Binder.Fix(ty)) (Pattern.V(x)) e' -> f (x, ty) e')
let rec let_funs e f =
  if CunTerm.is_fun e then
    CunTerm.let_fun e
      (fun xty e' -> let_funs e' (fun xtys e'' -> f (xty :: xtys) e''))
  else f [] e
let rec let_n_funs n e f =
  if n = 0 then f [] e
  else
    CunTerm.let_fun e
      (fun xty e' ->
         let_n_funs (n - 1) e' (fun xtys e'' -> f (xty :: xtys) e''))
let let_let_pat e f =
  match fun_args e with
  | Const(Const.ML_Let(ty)), e1 :: e2 :: es ->
    CunTerm.let_fun_pat e2 (fun pty e2' -> f ty pty e1 e2' es)
  | _ -> assert false
let let_let e f =
  let_let_pat e
    (fun ty_bd (Pattern.V(x), ty) e1 e2 es ->
       f ty_bd (x, ty) e1 e2 es)
let let_letrec e f =
  match fun_args e with
  | Const(Const.ML_LetRec(n)),
    e1 :: e2 :: es ->
    let rec loop n e' =
      if n = 0 then [], e'
      else
        let_fix e'
          (fun (x, ty) e1' ->
             loop (n - 1) e1' |> Pair.map_fst (List.cons (x, ty)))
    in
    let xtys, e1' = loop n e1 in
    let Const(Const.ML_LetAnd), es' = fun_args e1' in
    let fds =
      List.map2
        (fun xty e -> let_funs e (fun xtys' e' -> xty, xtys', e'))
        xtys es'
    in
    let_n_funs n e2 (fun _ e2' -> f fds e2' es)
  | _ -> assert false
let let_closure e f =
  match fun_args e with
  | Const(Const.ML_Closure(n)), es
    when List.length es >= n + 1 ->
    let es1, es2 = List.split_at (n + 1) es in
    CunTerm.let_fun (List.last es1)
      (fun xty e1 -> let env = List.initial es1(*@todo*) in f env xty e1 es2)
  | _ -> assert false
let let_top e f =
  match fun_args e with
  | Const(Const.Top), [] -> f ()
  | _ -> assert false
let let_bot e f =
  match fun_args e with
  | Const(Const.Bot), [] -> f ()
  | _ -> assert false
let let_event e f =
  match fun_args e with
  | Const(Const.Event(id)), e1 :: es -> f id e1 es
  | _ -> assert false

(** {6 Morphisms} *)

let rec para f e =
  match e with
  | Var(x) -> f#fvar x
  | Const(c) -> f#fcon c
  | App(_, _) ->
    let e1, r1, es =
      match fun_args e with
      | Const(Const.ML_If(_)), _ ->
        CunTerm.let_if e
          (fun ty e1 e2 e3 es ->
             let r1 = para f e1 in
             let r2 = para f e2 in
             let r3 = para f e3 in
             mk_if ty e1 e2 e3, f#fif ty e1 r1 e2 r2 e3 r3, es)
      | Const(Const.ML_Let(_)), _ ->
        let_let_pat e
          (fun ty pty e1 e2 es ->
             let r1 = para f e1 in
             let r2 = para f e2 in
             mk_let_pat ty pty e1 e2, f#flet ty pty e1 r1 e2 r2, es)
      | Const(Const.ML_LetRec(_)), _ ->
        let_letrec e
          (fun fds e es ->
             let fds' =
               List.map (fun (xty, xtys, e) -> xty, xtys, e, para f e) fds
             in
             let r = para f e in
             mk_letrec fds e, f#fletrec fds' e r, es)
      | Const(Const.Event(_)), _ ->
        let_event e
          (fun id e1 es ->
             let r1 = para f e1 in
             mk_event id e1, f#fevent id e1 r1, es)
      | Const(Const.Tuple _), _ ->
        TupTerm.let_tuple e
          (fun tys es1 ->
             let rs1 = List.map (para f) es1 in
             TupTerm.make tys es1, f#ftuple tys es1 rs1, [])
          (fun () -> assert false)
      | Const (Const.Con(_, _)), _ ->
        ADTTerm.let_kon e
          (fun ty id es1 ->
             let rs1 = List.map (para f) es1 in
             ADTTerm.mk_kon (id, ty) es1, f#fkon (id, ty) es1 rs1, [])
      | Const(Const.ML_Closure _), _ ->
        let_closure e
          (fun env xty e1 es ->
             let r1 = para f e1 in
             mk_closure env xty e1, f#fcls env xty e1 r1, es)
      | Const (Const.Array (_)), _ ->
        ArrayTerm.let_array e
          (fun es1 es2 ->
             let rs1 = List.map (para f) es1 in
             ArrayTerm.mk_array es1, f#farray es1 rs1, es2)
      | Const (Const.AGet), _ ->
        ArrayTerm.let_aget e
          (fun a n es ->
             let r1 = para f a in
             let r2 = para f n in
             ArrayTerm.mk_aget a n, f#faget a r1 n r2, es)
      | Const (Const.ASet), _ ->
        ArrayTerm.let_aset e
          (fun a n m e es ->
             let r1 = para f a in
             let r2 = para f n in
             let r3 = para f m in
             let r4 = para f e in
             ArrayTerm.mk_aset a n m e, f#faset a r1 n r2 m r3 e r4, es)
      | e1, es -> e1, para f e1, es
    in
    if es = [] then r1
    else f#fapp e1 r1 es (List.map (para f) es)
  | Binder(Binder.Lambda(ty), Pattern.V(x), e1) ->
    f#ffun (x, ty) e1 (para f e1)
  | Binder(Binder.Fix(ty), Pattern.V(x), e1) ->
    f#ffix (x, ty) e1 (para f e1)
  | _ ->
    Format.printf "not supported (in MLExp.para): %a@," Term.pr e;
    assert false

let fold f =
  para
    (object
      method fvar x = f#fvar x
      method fcon c = f#fcon c
      method fif ty _ r1 _ r2 _ r3 = f#fif ty r1 r2 r3
      method flet ty pty _ r1 _ r2 = f#flet ty pty r1 r2
      method fletrec fds _ r1 = f#fletrec (List.map Quadruple.get124 fds) r1
      method fevent id _ r1 = f#fevent id r1
      method fapp _ r1 _ rs = f#fapp r1 rs
      method ffun xty _ r1 = f#ffun xty r1
      method ftuple tys _ rs = f#ftuple tys rs
      method fkon xty _ rs = f#fkon xty rs
      method ffix xty _ r1 = f#ffix xty r1
      method fcls env xty _ r1 = f#fcls env xty r1
      method farray _ rs = f#farray rs
      method faget _ a _ n  = f#faget a n
      method faset _ a _ n _ m _ e  = f#faset a n m e
    end)

let visit f e =
  para
    (object
      method fvar x = fun () -> f#fvar x
      method fcon c = fun () -> f#fcon c
      method fif ty e1 _ e2 _ e3 _ = fun () -> f#fif ty e1 e2 e3
      method flet ty pty e1 _ e2 _ = fun () -> f#flet ty pty e1 e2
      method fletrec fds e1 _ = fun () ->
        f#fletrec (List.map Quadruple.get123 fds) e1
      method fevent id e1 _ = fun () -> f#fevent id e1
      method fapp e1 _ es _ = fun () -> f#fapp e1 es
      method ffun xty e1 _ = fun () -> f#ffun xty e1
      method ftuple tys es _ = fun () -> f#ftuple tys es
      method fkon xty es _ = fun () -> f#fkon xty es
      method ffix xty e1 _ = fun () -> f#ffix xty e1
      method fcls env xty e1 _ = fun () -> f#fcls env xty e1
      method farray rs _ = fun () -> f#farray rs
      method faget a _ n _ = fun () -> f#faget a n
      method faset a _ n _ m _ e _ = fun () -> f#faset a n m e
    end)
    e
    ()

let map_const f =
  fold
    (object
      method fvar = mk_var
      method fcon c = mk_const (f c)
      method fif = mk_if
      method flet = mk_let_pat
      method fletrec = mk_letrec
      method fevent = mk_event
      method fapp = mk_app
      method ffun xty e1 = mk_fun [xty] e1
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = mk_fix
      method fcls = mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)

(** {6 Printers} *)

(** @todo const *)
let upr_of =
  fold
    (object
      method fvar x = fun ppf () -> Idnt.pr ppf x
      method fcon c = fun ppf () -> String.pr ppf (Const.string_of c)
      method fif _ upr1 upr2 upr3 = fun ppf () ->
        Format.fprintf ppf
          "(@[<v>if %a then@,  %a@,else@,  %a@])"
          upr1 () upr2 () upr3 ()
      method flet _ (p, _) upr1 upr2 = fun ppf () ->
        Format.fprintf ppf
          "(@[<hov>@[<hov2>let %a =@ %a in@]@ %a@])"
          Pattern.pr p upr1 () upr2 ()
      method fletrec fds upr1 = fun ppf () ->
        Format.fprintf ppf "(@[<v>";
        List.iteri
          (fun i (xty, xtys, upr) ->
             (if i = 0 then Format.fprintf ppf "@[<hov2>let rec "
              else Format.fprintf ppf "@[<hov2>and ");
             Format.fprintf ppf
               "%a =@ %a@]@ "
               (List.pr Idnt.pr " ") (xty :: xtys |> List.map fst)
               upr ())
          fds;
        Format.fprintf ppf "in@ %a@])" upr1 ()
      method fevent id upr1 = fun ppf () ->
        Format.fprintf ppf "(@[<hov>event %s in@ %a@])" id upr1 ()
      method fapp upr uprs = fun ppf () ->
        Format.fprintf ppf
          "(%a)"
          (Printer.concat_uprs_app (upr :: uprs) "@ ") ()
      method ffun (x, _) upr = fun ppf () ->
        Format.fprintf ppf "(@[<hov2>fun %a ->@ %a@])" Idnt.pr x upr ()
      method ftuple _ uprs = fun ppf () ->
        Format.fprintf ppf "(%a)" (Printer.concat_uprs uprs ",@ ") ()
      method fkon (x, _) uprs = fun ppf () ->
        Format.fprintf ppf
          "(%a(%a))"
          Idnt.pr x
          (Printer.concat_uprs uprs ",@ ") ()
      method ffix (x, _) upr = fun ppf () ->
        Format.fprintf ppf
          "(@[<hov2>fix %a ->@ %a@])"
          Idnt.pr x upr ()
      method fcls env (x, _) upr = fun ppf () ->
        Format.fprintf ppf
          "(@[<hov2>cls(...,@ %a,@ %a)@])"
          Idnt.pr x upr ()
      method farray uprs = fun ppf () ->
        Format.fprintf ppf "%a" (Printer.concat_uprs uprs "@ ") ()
      method faget upr1 upr2 = fun ppf () ->
        Format.fprintf ppf "@[<hov2>%a.(%a)@]" upr1 () upr2 ()
      method faset upr1 upr2 upr3 upr4 = fun ppf () ->
        Format.fprintf ppf
          "@[<hov2>%a.(%a) <- %a in %a@]"
          upr1 () upr2 () upr3 () upr4 ()
    end)

let pr ppf e = upr_of e ppf ()
let pr_list = List.pr pr ","

(** {6 Operators} *)

let elim_fun =
  fold
    (object
      method fvar = mk_var
      method fcon = mk_const
      method fif = mk_if
      method flet = mk_let_pat
      method fletrec = mk_letrec
      method fevent = mk_event
      method fapp = mk_app
      method ffun xty e1 =
        let x' = Idnt.new_var () in
        mk_letrec [(x', Type.mk_top), [xty], e1] (mk_var x')
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = mk_fix
      method fcls = mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)

let elim_def_with_mult_args =
  fold
    (object
      method fvar = mk_var
      method fcon = mk_const
      method fif = mk_if
      method flet = mk_let_pat
      method fletrec = mk_letrec_simple
      method fevent = mk_event
      method fapp = mk_app
      method ffun xty e1 = mk_fun [xty] e1
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = mk_fix
      method fcls = mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)

let cps ctx e =
  fold
    (object
      method fvar x = fun ctx -> ctx (mk_var x)
      method fcon c = fun ctx -> ctx (mk_const c)
      method fif ty r1 r2 r3 = fun ctx -> assert false
      method flet ty pty r1 r2 = fun ctx -> assert false
      method fletrec fds r1 = fun ctx ->
        mk_letrec
          (List.map
             (fun (xty, xtys, r) ->
                assert(List.length xtys <= 1);
                let k = Idnt.new_var () in
                xty,
                (if xtys = [] then xtys else xtys @ [k, Type.mk_top]),
                (if xtys = [] then r id
                 else r (fun ret -> mk_app (mk_var k) [ret])))
             fds)
          (r1 ctx)
      method fevent id r1 = fun ctx ->
        if id = Const.event_fail then mk_fail (*@todo*)
        else mk_event id (r1 ctx)
      method fapp r1 rs =
        let rec aux rs = fun ctx ->
          match rs with
          | [] -> r1 ctx
          | r :: rs' ->
            r (fun v2 ->
                aux rs'
                  (fun v1 ->
                     let xty1 = Idnt.new_var (), Type.mk_top in
                     let xty2 = Idnt.new_var (), Type.mk_top in
                     mk_letrec
                       [xty1, [xty2], ctx (xty2 |> fst |> mk_var)]
                       (mk_app v1 [v2; xty1 |> fst |> mk_var])))
        in
        aux (List.rev rs)
      method ffun _ _ = fun ctx -> assert false
      method ftuple _ _ = fun ctx -> assert false
      method fkon _ _ = fun ctx -> assert false
      method ffix xty r1 = fun ctx -> assert false
      method fcls _ _ _ = fun ctx -> assert false
      method farray rs = fun ctx -> assert false
      method faget a n = fun ctx -> assert false
      method faset a n m e = fun ctx -> assert false
    end)
    e
    ctx

let elim_def_with_free_vars fs ren e =
  para
    (object
      method fvar x = fun fs ren ->
        if List.mem_assoc x ren then
          mk_app (mk_var x) (List.map (fst >> mk_var) (List.assoc x ren))
        else mk_var x
      method fcon c = fun fs ren -> mk_const c
      method fif ty _ r1 _ r2 _ r3 = fun fs ren ->
        mk_if ty (r1 fs ren) (r2 fs ren) (r3 fs ren)
      method flet ty pty _ r1 _ r2 = fun fs ren -> assert false
      method fletrec fds _ r1 = fun fs ren ->
        let fs = fs @ List.map (Quadruple.fst >> fst) fds in
        let ren =
          ren
          @ List.map
            (fun (xty, xtys, e, _) ->
               xty |> fst,
               e
               |> flip SimTypJudge.env_of (xty |> snd |> Type.ret_of)
               |> flip TypEnv.diff (fs @ List.map fst xtys)
               |> List.unique)
            fds
        in
        mk_letrec
          (List.map
             (fun (xty, xtys, _, r) ->
                xty,
                List.assoc (xty |> fst) ren @ xtys,
                r fs ren)
             fds)
          (r1 fs ren)
      method fevent id _ r1 = fun fs ren -> mk_event id (r1 fs ren)
      method fapp _ r1 _ rs = fun fs ren ->
        mk_app (r1 fs ren) (List.map (fun r -> r fs ren) rs)
      method ffun xty _ r1 = fun fs ren -> assert false
      method ftuple tys _ rs = fun fs ren ->
        TupTerm.make tys (List.map (fun r -> r fs ren) rs)
      method fkon xty _ rs = fun fs ren ->
        ADTTerm.mk_kon xty (List.map (fun r -> r fs ren) rs)
      method ffix xty _ r1 = fun fs ren -> assert false
      method fcls _ _ _ _ = fun fs ren -> assert false
      method farray _ rs = fun fs ren ->
        ArrayTerm.mk_array (List.map (fun r -> r fs ren) rs)
      method faget _ r1 _ r2 = fun fs ren ->
        ArrayTerm.mk_aget (r1 fs ren) (r2 fs ren)
      method faset _ r1 _ r2 _ r3 _ r4 = fun fs ren ->
        ArrayTerm.mk_aset (r1 fs ren) (r2 fs ren) (r3 fs ren) (r4 fs ren)
    end)
    e
    fs ren

let lift =
  fold
    (object
      method fvar x = [], mk_var x
      method fcon c = [], mk_const c
      method fif ty (fds1, e1) (fds2, e2) (fds3, e3) =
        fds1 @ fds2 @ fds3, mk_if ty e1 e2 e3
      method flet _ _ _ _ = assert false
      method fletrec fds (fds1, e1) =
        fds
        |> List.map (fun (xty, xtys, (fds, e)) -> fds, (xty, xtys, e))
        |> List.split
        |> Pair.map_fst List.flatten
        |> uncurry2 (@)
        |> flip (@) fds1,
        e1
      method fevent id (fds1, e1) = fds1, mk_event id e1
      method fapp (fds1, e1) rs =
        let fds2, es2 =
          rs
          |> List.split
          |> Pair.map_fst List.concat
        in
        fds1 @ fds2, mk_app e1 es2
      method ffun _ _ = assert false
      method ftuple tys rs =
        List.split rs |> Pair.map List.concat (TupTerm.make tys)
      method fkon xty rs =
        List.split rs |> Pair.map List.concat (ADTTerm.mk_kon xty)
      method ffix _ _ = assert false
      method fcls _ _ _ = assert false
      method farray rs =
        List.split rs |> Pair.map List.concat ArrayTerm.mk_array
      method faget r1 r2 = assert false(*Pair.map id (ArrayTerm.mk_aget r1) r2*)
      method faset r1 r2 r3 r4 = assert false
    end)
let lift e =
  let rec loop e =
    let e' = elim_def_with_free_vars [] [] e in
    if e = e' then e else loop e'
  in
  e |> loop |> lift |> uncurry2 mk_letrec

let pattern_of =
  fold
    (object
      method fvar x = Pattern.mk_var x
      method fcon c =
        if c = Const.Unit then Pattern.U else invalid_arg "MLExp.pattern_of"
      method fif ty _ _ _ = invalid_arg "MLExp.pattern_of"
      method flet _ _ _ _ = invalid_arg "MLExp.pattern_of"
      method fletrec _ _ = invalid_arg "MLExp.pattern_of"
      method fevent _ _ = invalid_arg "MLExp.pattern_of"
      method fapp _ _ = invalid_arg "MLExp.pattern_of"
      method ffun _ _ = invalid_arg "MLExp.pattern_of"
      method ftuple _ ps = Pattern.T(ps)
      method fkon _ _ = invalid_arg "MLExp.pattern_of"
      method ffix _ _ = invalid_arg "MLExp.pattern_of"
      method fcls _ _ _ = invalid_arg "MLExp.pattern_of"
      method farray rs = invalid_arg "MLExp.patter_of"
      method faget a r = invalid_arg "MLExp.patter_of"
      method faset a r1 r2 r3 = invalid_arg "MLExp.patter_of"
    end)

let diff_env env xs = List.filter (fun (x, _) -> not (List.mem x xs)) env
let rec update_env env env' =
  (** @todo verify that this is always terminating *)
  let rec loop env =
    let env' =
      List.map
        (function
          | x, None -> x, None
          | x, Some(e) -> x, Some(inline env e))
        env
    in
    if env' = env then env else loop env'
  in
  env @ loop env'
and inline env e =
  visit
    (object
      method fvar x =
        match Map_.apply_default (Some(mk_var x)) env x with
        | Some(e) -> e
        | None -> mk_var name_of_id (* @todo *)
      method fcon = mk_const
      method fif ty e1 e2 e3 =
        mk_if ty (inline env e1) (inline env e2) (inline env e3)
      method flet ty_bd (pat, ty) e1 e2 =
        let e1' = inline env e1 in
        match pat with
        | Pattern.V(x)
          when (is_rand e1' || not (may_have_side_effect e1'))
            && num_of_var_occurrences x e2 = 0 ->
          inline env e2
        | Pattern.V(x)
          when not (may_have_side_effect e1')
            && (num_of_var_occurrences x e2 = 1 || is_simple e1') ->
          inline ((x, Some(e1')) :: env) e2
        | pat when (try Pattern.equiv (pattern_of e2) pat
                    with Invalid_argument _ -> false) ->
          (* translate "let pat = e in pat" to "e" *)
          e1'
        | _ ->
          mk_let_pat ty_bd (pat, ty) e1'
            (inline (diff_env env (Pattern.fvs pat)) e2)
      method fletrec fds e1 =
        let env', fds' =
          List.partition_map
            (fun (xty, xtys, e) ->
               if xtys = [] then `L(fst xty, Some(e))
               else if fst xty <> name_of_id (* @todo *)
                    && xtys
                       |> TypEnv.dom
                       |> List.map mk_var
                       |> mk_app2
                       |> (=) e
                       (* the second line of the condition checks whether
                          the definition of [f] is of the form:
                          [let rec f x1 x2 ... xn = x1 x2 ... xn] *)
               then `L(fst xty, None)
               else `R(xty, xtys, e))
            fds
        in
        let env =
          update_env
            (diff_env env (List.map (Triple.fst >> fst) fds))
            env'
        in
        mk_letrec
          (List.map
             (fun (xty, xtys, e) ->
                xty,
                xtys,
                inline (diff_env env (List.map fst xtys)) e)
             fds')
          (inline env e1)
      method fevent id e1 = mk_event id (inline env e1)
      method fapp e1 es =
        try
          let_var e1
            (fun x ->
               match List.assoc x env with
               | None ->
                 es
                 |> List.map (inline env)
                 |> mk_app2
               | _ -> raise Not_found)
        with
        | Global.NoMatch _
        | Not_found -> mk_app (inline env e1) (List.map (inline env) es)
      method ffun xty e1 = (*mk_fun [xty] e1*) assert false
      method ftuple tys es = TupTerm.make tys (List.map (inline env) es)
      method fkon xty es = ADTTerm.mk_kon xty (List.map (inline env) es)
      method ffix xty e1 = (*mk_fix xty e1*) assert false
      method fcls env xty e1 = (*mk_closure env xty e1*) assert false
      method farray rs = ArrayTerm.mk_array (List.map (inline env) rs)
      method faget a r = ArrayTerm.mk_aget a (inline env r)
      method faset a r1 r2 r3 =
        ArrayTerm.mk_aset a (inline env r1) (inline env r2) (inline env r3)
    end)
    e

let simplify =
  fold
    (object
      method fvar = mk_var
      method fcon = mk_const
      method fif = mk_if
      method flet = mk_let_pat
      method fletrec = mk_letrec (* @todo eliminate unused arguments *)
      method fevent = mk_event
      method fapp e1 es =
        let e = mk_app e1 es in
        try LinTermIntExp.simplify_full e
        with Invalid_argument _ ->
        try
          e
          |> Formula.of_term
          |> FormulaSimplifier.simplify
          |> Formula.term_of
        with _ -> e
      method ffun xty e1 = mk_fun [xty] e1
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = mk_fix
      method fcls = mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)

let rec anf_aux ty rs ts f =
  match rs with
  | [] -> f ts
  | r :: rs ->
    r (fun t ->
        if is_var t || is_const t then anf_aux ty rs (ts @ [t]) f
        else
          let v = Idnt.new_var () in
          mk_let ty (v, Type.mk_unknown) t
            (anf_aux ty rs (ts @ [Term.mk_var v]) f))
let anf t =
  fold
    (object
      method fvar x = fun k -> k (mk_var x)
      method fcon c = fun k -> k (mk_const c)
      method fif ty r1 r2 r3 = fun k ->
        r1 (fun t1 -> k (mk_if ty t1 (r2 id) (r3 id)))
      method flet ty pty r1 r2 = fun k ->
        r1 (fun t1 -> k (mk_let_pat ty pty t1 (r2 id)))
      method fletrec fds r1 = fun k ->
        mk_letrec
          (List.map (fun (x, args, r2) -> (x, args, r2 id)) fds)
          (r1 k)
      method fevent str r1 = fun k -> k (mk_event str (r1 id))
      method fapp r1 rs = fun k ->
        anf_aux Type.mk_unknown (r1 :: rs) []
          (function
            | t1 :: ts -> k (mk_app t1 ts)
            | [] -> assert false)
      method ffun xty r = fun k -> k (mk_fun [xty] (r id))
      method ftuple tys rs = fun k ->
        anf_aux (Type.mk_tuple tys) rs [] (fun ts -> k (TupTerm.make tys ts))
      method fkon xty rs = fun k ->
        let ty = Type.ret_of (snd xty) in
        anf_aux ty rs [] (fun ts -> k (ADTTerm.mk_kon xty ts))
      method ffix xty e1 = fun k -> assert false
      method fcls env xty e1 = fun k -> assert false
      method farray es = fun k -> assert false
      method faget a e = fun k -> assert false
      method faset a e1 e2 e3 = fun k -> assert false
    end)
    t
    id

(* assume that each angelic nondeterministic boolean generation is not
   evaluated iteratively *)
let determinize m =
  fold
    (object
      method fvar = mk_var
      method fcon = mk_const
      method fif ty e1 e2 e3 =
        if Term.is_const e1 then
          Term.let_const e1
            (function
              | Const.ReadBool(x, []) -> if List.assoc_fail x m then e2 else e3
              | _ -> mk_if ty e1 e2 e3)
        else mk_if ty e1 e2 e3
      method flet = mk_let_pat
      method fletrec = mk_letrec
      method fevent = mk_event
      method fapp = mk_app
      method ffun xty e1 = mk_fun [xty] e1
      method ftuple = TupTerm.make
      method fkon = ADTTerm.mk_kon
      method ffix = mk_fix
      method fcls = mk_closure
      method farray = ArrayTerm.mk_array
      method faget = ArrayTerm.mk_aget
      method faset = ArrayTerm.mk_aset
    end)

(** {6 Inspectors} *)

let boolean_angelic_nondet_ids =
  fold
    (object
      method fvar x = []
      method fcon = function Const.ReadBool(x, []) -> [x] | _ -> []
      method fif _ r1 r2 r3 = r1 @ r2 @ r3
      method flet _ pty r1 r2 = r1 @ r2
      method fletrec fds r1 = r1
      method fevent id r1 = r1
      method fapp r1 rs = r1 @ List.concat rs
      method ffun xty r1 = r1
      method ftuple tys rs = List.concat rs
      method fkon xty rs = List.concat rs
      method ffix xty r1 = r1
      method fcls env xty r1 = r1
      method farray rs = List.concat rs
      method faget a r1 = r1
      method faset a r1 r2 r3 = r1 @ r2 @ r3
    end)

let has_read_bool = boolean_angelic_nondet_ids >> (<>) []

(** {6 Inspectors} *)

let is_value arity_of =
  para
    (object
      method fvar _ = true
      method fcon _ = true
      method flet _ _ _ _ _ r = r
      method fletrec _ _ _ = assert false
      method fevent _ _ _ = assert false
      method fapp e1 _ _ rs =
        List.for_all id rs &&
        match e1 with
        | Term.Var x -> arity_of x > (List.length rs)
        | _ -> true
      method ffun _ _ _ = assert false
      method ffix _ _ _ = assert false
      method fcls _ _ _ _ = assert false
      method ftuple _ _ rs = List.for_all id rs
      method fkon _ _ rs = List.for_all id rs
      method fif _ _ _ _ _ _ _ = false
      method farray _ rs = false
      method faget a r1 n r2 = false
      method faset a r1 n r2 m r3 e r4 = false
    end)

let is_app e =
  let e, es = fun_args e in
  es <> [] && is_var e

(** {6 Sample expressions} *)

let wild_var = Idnt.make "_"(*@todo*), Type.mk_top

let fib =
  mk_recfun
    Type.mk_unknown
    wild_var
    [wild_var]
    (mk_if
       Type.mk_int
       (IntAtom.leq (mk_var (Idnt.N(0))) IntTerm.one |> Atom.term_of)
       IntTerm.one
       (IntTerm.add
          (mk_app
             (mk_var (Idnt.N(1)))
             [IntTerm.sub (mk_var (Idnt.N(0))) IntTerm.one])
          (mk_app
             (mk_var (Idnt.N(1)))
             [IntTerm.sub (mk_var (Idnt.N(0))) (IntTerm.make 2)])))
    (mk_app (mk_var (Idnt.N(0))) [IntTerm.one])

let fib_cps =
  mk_recfun
    Type.mk_unknown
    wild_var
    [wild_var; wild_var]
    (mk_if
       Type.mk_unknown (*@todo*)
       (IntAtom.leq (mk_var (Idnt.N(1))) IntTerm.one |> Atom.term_of)
       (mk_app (mk_var (Idnt.N(0))) [IntTerm.one])
       (mk_recfun
          Type.mk_unknown
          wild_var
          [wild_var]
          (mk_recfun
             Type.mk_unknown
             wild_var
             [wild_var]
             (mk_app
                (mk_var (Idnt.N(4)))
                [IntTerm.add (mk_var (Idnt.N(2))) (mk_var (Idnt.N(0)))])
             (mk_app
                (mk_var (Idnt.N(5)))
                [IntTerm.sub (mk_var (Idnt.N(4))) (IntTerm.make 2);
                 mk_var (Idnt.N(0))]))
          (mk_app
             (mk_var (Idnt.N(3)))
             [IntTerm.sub (mk_var (Idnt.N(2))) IntTerm.one;
              mk_var (Idnt.N(0))])))
    (mk_recfun
       Type.mk_unknown
       wild_var
       [wild_var]
       (mk_var (Idnt.N(0)))
       (mk_app
          (mk_var (Idnt.N(1)))
          [IntTerm.make 20(*29*); mk_var (Idnt.N(0))]))

let test_cps () =
  cps
    id
    (mk_app
       (mk_app
          (mk_var (Idnt.make "f"))
          [mk_var (Idnt.make "g")])
       [mk_var (Idnt.make "h")])
  |> Format.printf "%a@," pr
