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
      | Const(Const.ML_Closure _), _ ->
        let_closure e
          (fun env xty e1 es ->
             let r1 = para f e1 in
             mk_closure env xty e1, f#fcls env xty e1 r1, es)
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
