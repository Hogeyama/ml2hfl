open Util
open Combinator

(** Rewriting systems *)

let pr_elem ppf (t1, t2) =
  Format.fprintf ppf "@[<hov2>%a ->@ %a@]" Term.pr t1 Term.pr t2
let pr ppf = Format.fprintf ppf "%a" (List.pr pr_elem "@,")

let rewrite rs t =
  Term.find_mapc
    (fun ctx t ->
       Option.of_nf (fun () ->
           List.find_map
             (fun (s1, s2) ->
                Option.of_exc (fun () ->
                    let sub = Term.pat_match t s1 in
                    (*Format.printf "applied rule: %a -> %a@," Term.pr s1 Term.pr s2;*)
                    ctx (Term.subst sub s2))
                  ())
             rs)
         ())
    t
let rewrite =
  Logger.log_block2
    "RewriteSystem.rewrite"
    ~before:(fun _ -> Logger.printf "input:@,  %a@," Term.pr)
    ~after:(Logger.printf "output:@,  %a" Term.pr)
    ~on_exception:(fun Not_found -> Logger.printf0 "no rule applies@,")
    rewrite

let rec normalize rs t =
  try t |> rewrite rs |> normalize rs with Not_found -> t

let mk_capp t ts =
  List.fold_left
    (fun t1 t2 ->
       Term.mk_app
         (Term.mk_const Const.App)
         [t1; t2])
    t
    ts
let app ?(exc = fun c -> false) =
  Term.fold_wo_app
    (object
      method fvar x rs = mk_capp (Term.mk_var x) rs
      method fcon c rs =
        if rs = [] then Term.mk_const c
        else if exc c then Term.mk_app (Term.mk_const c) rs
        else mk_capp (Term.mk_const c) rs
      method fbin b x r rs = mk_capp (Term.mk_binder b x r) rs
    end)

let rec unapp t =
  match Term.fun_args t with
  | Term.Var(x), [] -> Term.mk_var x
  | Term.Const(c), [] -> Term.mk_const c
  | Term.Const(Const.App), [t1; t2] -> Term.mk_app (unapp t1) [unapp t2]
  | t, ts -> Term.mk_app (unapp t) (List.map unapp ts)


let rec dict_order (>>) ts1 ts2 =
  match ts1, ts2 with
  | [], [] -> false
  | t1 :: ts1', t2 :: ts2' -> t1 >> t2 || t1 = t2 && dict_order (>>) ts1' ts2'
  | _ -> assert false

let rec dict_part_term_order (>>) t1 t2 =
  (*Format.printf "t1: %a@,t2: %a@," Term.pr t1 Term.pr t2;*)
  match Term.fun_args t1 with
  | Term.Var(_), [] -> false
  | Term.Const(f), ts1 ->
    List.exists (fun t -> t = t2 || dict_part_term_order (>>) t t2) ts1
    || (match Term.fun_args t2 with
        | Term.Const(g), ts2 ->
          List.for_all (fun t -> dict_part_term_order (>>) t1 t) ts2
          && (f >> g || f = g && dict_order (dict_part_term_order (>>)) ts1 ts2)
        | _ -> false)





let group_theory () =
  let es =
    [RealTerm.add RealTerm.zero (Term.var_of_string "x1"),
     Term.var_of_string "x1";
     RealTerm.add
       (RealTerm.neg (Term.var_of_string "x2"))
       (Term.var_of_string "x2"),
     RealTerm.zero;
     RealTerm.add
       (RealTerm.add (Term.var_of_string "x3") (Term.var_of_string "y3"))
       (Term.var_of_string "z3"),
     RealTerm.add
       (Term.var_of_string "x3")
       (RealTerm.add (Term.var_of_string "y3") (Term.var_of_string "z3"))]
  in
  let const_ord c1 c2 =
    Logger.printf2
      "%a > %a? "
      String.pr (Const.string_of c1)
      String.pr (Const.string_of c2);
    match c1, c2 with
    | Const.Add(ty), Const.Real(_)
    | Const.Mul(ty), Const.Real(_) when Type.is_real ty ->
      Logger.printf0 "yes@,"; true
    | Const.Neg(ty1), Const.Add(ty2) (*@todo*)
    | Const.Neg(ty1), Const.Mul(ty2) when Type.is_real ty1 && Type.is_real ty2 -> (*@todo*)
      Logger.printf0 "yes@,"; true
    | _, _ -> Logger.printf0 "no@,"; false
  in
  es, dict_part_term_order const_ord
