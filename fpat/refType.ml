open Util
open Combinator

(** Refinement intersection types *)

type t =
  | Bot
  | Top
  | Base of Idnt.t * TypConst.t * Formula.t
  | Fun of (t * t) list (* binders are ... *)

let refine_unit = ref true
let refine_function = ref false

(** {6 Inspectors} *)

let is_base rty =
  match rty with
  | Bot -> false(*@todo*)
  | Top -> false(*@todo*)
  | Base(_, _, _) -> true
  | Fun(_) -> false

let bv_of rty =
  match rty with
  | Bot -> assert false
  | Top -> assert false
  | Base(bv, _, _) -> bv
  | Fun(_) -> assert false

let rec args_ret rty =
  match rty with
  | Fun([rty1, rty2]) ->
    let args, ret = args_ret rty2 in
    rty1 :: args, ret
  | Fun(_) ->
    assert false
  | _ ->
    [], rty

(* y is visible from x? *)
let rec visible x y =
  match x with
  | Idnt.V(_) ->
    false(*@todo*)
  | Idnt.T(z, uid, arg) ->
    begin
      match y with
      | Idnt.V(_) -> false(*@todo*)
      | Idnt.T(z', uid', arg') ->
        (z = z' && uid = uid' && arg' <= arg) ||
        (visible z y)
    end


let rec visible_vars x =
  match x with
  | Idnt.V(_) ->
    [x]
  | Idnt.T(x, uid, arg) ->
    visible_vars x
    @ List.init (arg + 1) (fun i -> Idnt.T(x, uid, i))

(** @require x is a structured variable
    @ensure returned var. y is base
    @ensure forall returned var. y, visible x y *)
let visible_vars env x =
  visible_vars x
  |> List.filter_map
    (fun x -> if Type.is_base (env x)
                 || Type.is_adt (env x) then
        Some(x, env x)
      else None)

let refinable ty =
  if Type.is_fun ty then !refine_function
  else if Type.is_base ty || Type.is_adt ty then
    if Type.is_unit ty then
      !refine_unit
    else
      true
  else
    assert false

(** @require selective pred. abs. is applied *)
let find_last_base_exc env (x, uid) =
  let rec aux ty i j =
    if Type.is_base ty || Type.is_adt ty then
      j
    else if Type.is_fun ty then
      Type.let_fun
        ty
        (fun ty1 ty2 ->
           aux ty2 (i + 1) (if refinable ty1 then i else j))
    else
      begin
        Format.printf "%a@," Type.pr ty;
        assert false
      end
  in
  let i = aux (env x) 0 (-1) in
  if i = -1 then
    raise Not_found
  else
    Idnt.T(x, uid, i)

let find_last_base env (x, uid) =
  try
    find_last_base_exc env (x, uid)
  with Not_found ->
    (* condition must be Formula.mk_true *)
    Idnt.T(x, uid, -1)

let rec fresh_names new_var ty =
  match ty with
  | Bot -> []
  | Top -> []
  | Base(x, _, t) ->
    let y = new_var () in
    [x, y]
  | Fun(xs) ->
    List.concat_map 
      (fun (rty1, rty2) ->
         let sub1 = fresh_names new_var rty1 in
         let sub2 = fresh_names new_var rty2 in
         sub1 @ sub2)
      xs

let pred_of_base rty =
  match rty with
  | Bot -> assert false
  | Top -> assert false
  | Base(x, _, t) -> x, t
  | Fun(_) -> assert false

(** {6 Printers} *)

let rec pr ppf rty =
  match rty with
  | Bot ->
    String.pr ppf "bot"
  | Top ->
    String.pr ppf "top"
  | Base(x, c, t) ->
    Format.fprintf ppf "@[<hov>";
    (if Formula.is_true t then
       String.pr ppf (TypConst.string_of c)
     else
       Format.fprintf
         ppf
         "{%a:%s | %a}"
         Idnt.pr x
         (TypConst.string_of c)
         Formula.pr t);
    Format.fprintf ppf "@]"
  | Fun(xs) ->
    assert (xs <> []);
    let pr_aux ppf (rty1, rty2) =
      Format.fprintf ppf "@[<hov>";
      (if is_base rty1 (*|| is_adt rty1*) then
         Format.fprintf ppf "%a:%a" Idnt.pr (bv_of rty1) pr rty1
       else
         Format.fprintf ppf "(%a)" pr rty1);
      Format.fprintf ppf "@ ->@ %a@]" pr rty2
    in
    Format.fprintf ppf "@[<hv>%a@]" (List.pr pr_aux " /\\@ ") xs

let pr_bind ppf (f, sty) =
  Format.fprintf ppf "%a: %a" Idnt.pr f pr sty
let pr_env ppf env =
  Format.fprintf ppf "@[<v>%a@]" (List.pr pr_bind "@ ") env

(** {6 Auxiliary constructors} *)

let answer_type =
  Base(Idnt.make "nu", TypConst.Ext("X"(*@todo*)), Formula.mk_true)

let mk_fun tys =
  List.fold_right
    (fun ty1 ty2 -> Fun [ty1, ty2])
    (List.initial tys)
    (List.last tys)

let rec of_simple_type ty =
  if Type.is_bot ty then
    Bot
  else if Type.is_top ty then
    Top
  else if Type.is_base ty || Type.is_adt ty then
    Type.let_base_or_adt
      ty
      (fun c ->
         Base(Idnt.new_var(), c, Formula.mk_true))
  else if Type.is_fun ty then
    Type.let_fun
      ty
      (fun ty1 ty2 ->
         Fun([of_simple_type ty1, of_simple_type ty2]))
  else
    assert false

let rec subst xts rty =
  match rty with
  | Bot -> Bot
  | Top -> Top
  | Base(x, c, t) ->
    let xts' = Map_.diff xts [x] in
    Base(x, c, Formula.subst xts' t)
  | Fun(xs) ->
    Fun
      (List.map
         (fun (rty1, rty2) ->
            let xts' =
              if is_base rty1 (*|| is_adt rty1*) then
                Map_.diff xts [bv_of rty1]
              else
                xts
            in
            subst xts rty1,
            subst xts' rty2)
         xs)

let set_base_cond rty t =
  match rty with
  | Base(x, c, _) ->
    Base(x, c, t)
  | Fun(_) ->
    assert false


let rename xys rty =
  let xts = List.map (fun (x, y) -> x, Term.mk_var y) xys in
  let rec aux rty =
    match rty with
    | Base(x, c, t) ->
      let y = try List.assoc x xys with Not_found -> x in
      Base(y, c, Formula.subst xts t)
    | Fun(xs) ->
      Fun
        (List.map
           (fun (rty1, rty2) ->
              aux rty1, aux rty2)
           xs)
  in
  aux rty

let new_var =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; Idnt.make ("v" ^ (string_of_int !cnt))

let canonize rty =
  rename (fresh_names new_var rty) rty
let canonize =
  Logger.log_block1
    "RefType.canonize"
    canonize

let merge tys =
  match tys with
  | Fun(_) :: _ ->
    Fun
      (List.concat_map
         (function Fun(xs) -> xs | _ -> assert false)
         tys)
  | [ty] -> ty
  | [] -> raise Not_found (*Fun []*)
  | _ ->
    Format.printf
      "non-function types: @[<hv>%a@]@,"
      (List.pr pr " /\\@ ") tys;
    assert false

let pred_of_var env x =
  PredVar.make x (visible_vars env x)

let pva_of_var env x =
  Pva.make
    x
    (List.map
       (fun (x, ty) -> Term.mk_var x, ty)
       (visible_vars env x))

let rec mk_template flag env x =
  let ty = env x in
  let n = Type.arity_of ty in
  if n = 0 then
    if Type.is_ext ty then
      Type.let_ext
        ty
        (fun id ->
           Base(x, TypConst.Ext(id), Formula.mk_true))
    else if Type.is_base ty || Type.is_adt ty then
      Type.let_base_or_adt
        ty
        (fun c ->
           Base
             (x,
              c,
              if flag then
                Pva.to_formula
                  (pva_of_var env x)
              else
                Formula.mk_true))
    else
      assert false
  else
    let j =
      try
        let Idnt.T(_, _, j) = find_last_base_exc env (x, -1) in
        j
      with Not_found ->
        -1(* reachable here *)
        (*Format.printf "%a@," Idnt.pr x;
            assert false*)
    in
    mk_fun
      (List.init
         n
         (fun i -> mk_template
             (i = j)
             env
             (Idnt.T(x, -1, i))) @
       [mk_template false env (Idnt.T(x, -1, n))])
let mk_template = mk_template false

let rec pvars rty =
  match rty with
  | Base(x, c, phi) ->
    begin
      try
        phi
        |> Pva.of_formula
        |> Pva.pvar_of
        |> fst
        |> List.return
      with Global.NoMatch _ -> []
    end
  | Fun(xs) ->
    List.concat_map
      (fun (rty1, rty2) ->
         pvars rty1 @ pvars rty2)
      xs
  | _ -> assert false

let rec env_ty args rty =
  match args, rty with
  | [], _ ->
    [], rty
  | Pattern.V(arg) :: args', Fun([rty1, rty2]) ->
    let xts =
      if is_base rty1(* || is_adt rty1*) then
        [bv_of rty1, Term.mk_var arg]
      else
        []
    in
    let env, rty' = env_ty args' (subst xts rty2) in
    (Idnt.string_of(*@todo*) arg, rty1) :: env, rty'




(*
type t = Forall of (Idnt.t list * ExpType.t) list

let pr ppf (Forall(xs)) =
  let rec _pr ppf (ids, sigma) =
      Format.fprintf ppf "@[<2>";
      let _ =
        if ids <> [] then
          Format.fprintf ppf "%s%s.@,"
            Common.symbols.(6)
            (String.concat "," ids) in
      let _ =
        Format.fprintf ppf "%a" ExpType.pr sigma
      in Format.fprintf ppf "@]" in
  Format.fprintf ppf "%a"
    (Util.pr_list _pr " /\\@ ") xs

let fv (Forall(xs)) =
  Util.catmap (fun (_, tau) -> ExpType.fv tau) xs

let fpv (Forall(xs)) =
  Util.catmap (fun (_, tau) -> ExpType.fpv tau) xs

let fpv2 (Forall(xs)) =
  Util.catmap (fun (_, tau) -> ExpType.fpv2 tau) xs

let ftv (Forall(xs)) =
  Util.catmap (fun (ids, tau) -> Util.diff (ExpType.ftv tau) ids) xs

let is_fun (Forall((_, tau)::_)) = ExpType.is_fun tau

(*
let alpha_conv rn (Forall(xs)) =
  Forall(List.map (fun (id, tau) -> id, ExpType.alpha_conv rn tau) xs)
*)

let simplify (Forall(xs)) =
  Forall(List.map (fun (id, tau) -> id, ExpType.simplify tau) xs)

let assign_pred_var sub (Forall(xs)) =
  Forall(List.map (fun (tvs, tau) -> tvs, ExpType.assign_pred_var sub tau) xs)

let embed tau =
  Forall([[], tau])


(*
let make_int_type () = Adt(Id.gen_exp_var (), [], "int", Vfol.G(Fol.True))

let make_fun_type fs f =
  let ids = Id.gen_exp_vars fs in
  fst
  (List.fold_right
    (fun f (t, ids) ->
      let id::ids' = ids in
      Fun(id, f (List.rev ids'), t), ids')
    fs
    (f ids, List.rev ids))

let rec fold f tau =
  match tau with
    Var(id) ->
      f#fvar id
  | Tuple(id, taus, psi) ->
      f#ftuple id (List.map (fold f) taus) psi
  | Fun(id, tau1, tau2) ->
      f#ffun id (fold f tau1) (fold f tau2)
  | Adt(id1, taus, id2, psi) ->
      f#fadt id1 (List.map (fold f) taus) id2 psi

let rec visit f tau =
  match tau with
    Var(id) ->
      f#fvar id
  | Tuple(id, taus, psi) ->
      f#ftuple id taus psi
  | Fun(id, tau1, tau2) ->
      f#ffun id tau1 tau2
  | Adt(id1, taus, id2, psi) ->
      f#fadt id1 taus id2 psi

let rec pr ppf =
  visit
    (object
       method fvar id = Format.fprintf ppf "%s" id
       method ftuple id taus psi = 
         if psi = Vfol.G(Fol.True) then
           Format.fprintf ppf
             "%a"
             (Util.pr_list pr " * ") taus
         else
                 Format.fprintf ppf
                   "{%a:%a|%a}"
                   Id.pr id
                   (Util.pr_list pr " * ") taus
                   Vfol.pr psi
       method ffun id tau1 tau2 =
         Format.fprintf ppf "@[<2>%a:%a%s@,%a@]"
           Id.pr id
           pr tau1
           Common.symbols.(9)
           pr tau2
       method fadt id1 taus id2 psi =
         let _pr ppf =
           function
             [] -> ()
           | [tau] ->
               Format.fprintf ppf "%a " pr tau
           | taus ->
               Format.fprintf ppf "(%a) " (Util.pr_list pr ",") taus in
         if psi = Vfol.G(Fol.True) then
           Format.fprintf ppf
             "%a%s"
             _pr taus
             id2
         else
           Format.fprintf ppf
             "{%s:%a%s|%a}"
             id1
             _pr taus
             id2
             Vfol.pr psi
     end)

let fv =
  fold
    (object
       method fvar _ = []
       method ftuple id fvs psi =
         (List.concat fvs) @ (Util.diff (Vfol.fv psi) [id])
       method ffun id fv1 fv2 = fv1 @ (Util.diff fv2 [id])
       method fadt id fvs _ psi = (List.concat fvs) @ (Util.diff (Vfol.fv psi) [id])
     end)

let fpv =
  fold
    (object
       method fvar _ = []
       method ftuple _ fpvs psi =
         (List.concat fpvs) @ (Vfol.fpv psi)
       method ffun _ fpv1 fpv2 = fpv1 @ fpv2
       method fadt _ fpvs _ psi = (List.concat fpvs) @ (Vfol.fpv psi)
     end)

let fpv2 =
  fold
    (object
       method fvar _ = []
       method ftuple _ fpvs psi =
         psi::(List.concat fpvs)
       method ffun _ fpv1 fpv2 = fpv1 @ fpv2
       method fadt _ fpvs _ psi = psi::(List.concat fpvs)
     end)

let rec rename rn =
  visit
    (object
       method fvar id =
         Var(Util.assoc_default id id rn)
       method ftuple id taus psi =
         Tuple(id, List.map (rename rn) taus, Vfol.rename (List.remove_assoc id rn) psi)
       method ffun id tau1 tau2 =
         Fun(id, rename rn tau1, rename (Util.remove_assoc_list_all rn [id]) tau2)
       method fadt id1 taus id2 psi =
         let id1' = Id.gen_exp_var () in
         Adt(id1, List.map (rename rn) taus, id2, Vfol.rename (Util.remove_assoc_list_all rn [id1]) psi)
     end)

(*
let rec alpha_conv rn =
  visit
    (object
       method fvar id =
         Var(List.assoc_default id id rn)
       method ftuple id taus psi =
dame
         Tuple(id, List.map (alpha_conv rn) taus, Vfol.alpha_conv rn psi)
       method ffun id tau1 tau2 =
         let id' = Id.gen_exp_var () in
         Fun(id', alpha_conv rn tau1, alpha_conv ((id, id')::rn) tau2)
       method fadt id1 taus id2 psi =
         let id1' = Id.gen_exp_var () in
         Adt(id1', List.map (alpha_conv rn) taus, id2, Vfol.alpha_conv ((id1, id1')::rn) psi)
     end)
*)

let simplify =
  fold
    (object
       method fvar id = Var(id)
       method ftuple id taus psi = Tuple(id, taus, Vfol.simplify psi)
       method ffun id tau1 tau2 = Fun(id, tau1, tau2)
       method fadt id1 taus id2 psi = Adt(id1, taus, id2, Vfol.simplify psi)
     end)

let assign_pred_var sub =
  fold
    (object
       method fvar id = Var(id)
       method ftuple id taus psi = Tuple(id, taus, Vfol.G(Vfol.assign_pred_var sub psi))
       method ffun id tau1 tau2 = Fun(id, tau1, tau2)
       method fadt id1 taus id2 psi = Adt(id1, taus, id2, Vfol.G(Vfol.assign_pred_var sub psi))
     end)

let ftv =
  fold
    (object
       method fvar id = [id]
       method ftuple _ ftvs _ = List.concat ftvs
       method ffun _ ftv1 ftv2 = ftv1 @ ftv2
       method fadt _ ftvs _ _ = List.concat ftvs
     end)

let assign_type_var sub =
  fold
    (object
       method fvar id = Util.assoc_default (Var(id)) id sub
       method ftuple id taus psi = Tuple(id, taus, psi)
       method ffun id tau1 tau2 = Fun(id, tau1, tau2)
       method fadt id1 taus id2 psi = Adt(id1, taus, id2, psi)
     end)

let is_fun = function Fun(_, _, _) -> true | _ -> false

(*tuple case is obsolete
let rec subtype tau1 tau2 =
  match tau1, tau2 with
    Var(id1), Var(id2) when id1 = id2 ->
      Fol.True
  | Tuple(xs1, psi1), Tuple(xs2, psi2) when Util.equiv (List.map fst xs1) (List.map fst xs2) ->
      let ids1, taus1 = List.split xs1 in
      let ids2, taus2 = List.split xs2 in
      Fol.logand
        ((Fol.forall ids1 (Fol.Imply (psi1, psi2)))::
        (List.map2 (fun tau1 tau2 -> subtype tau1 tau2) taus1 taus2))
  | Fun(id1, tau1, tau1'), Fun(id2, tau2, tau2') ->
      let id = Id.gen_exp_var () in
      let tau1' = alpha_conv [id1, id] tau1' in
      let tau2' = alpha_conv [id2, id] tau2' in
      Fol.And(subtype tau2 tau1, Fol.Forall(id, subtype tau1' tau2'))
  | Adt(id1, taus1, id1', psi1), Adt(id2, taus2, id2', psi2) when id1' = id2' ->
      let id = Id.gen_exp_var () in
      Fol.logand
        ((Fol.Forall(id, Fol.Imply(Fol.alpha_conv [id1, id] psi1, Fol.alpha_conv [id2, id] psi2)))::
        (List.map2 (fun tau1 tau2 -> (**covariant**) subtype tau1 tau2) taus1 taus2))
  | _, _ -> raise (Common.Type_error "expType.subtype")
*)

let rec fpv_negative pos =
  visit
    (object
       method fvar _ =
         []
       method ftuple _ taus psi =
         (if pos then [] else [psi]) @
         (Util.catmap (fpv_negative pos) taus)
       method ffun _ tau1 tau2 =
         (fpv_negative (not pos) tau1) @
         (fpv_negative pos tau2)
       method fadt _ taus _ psi =
         (if pos then [] else [psi]) @
         (Util.catmap (fpv_negative pos(*???*)) taus)
     end)





let type_of_int f =
  let id = Id.gen_exp_var () in
  ExpType.Adt(
    id,
    [],
    "int",
    Vfol.G(f id))

let type_of_bool f =
  let id = Id.gen_exp_var () in
  ExpType.Adt(
    id,
    [],
    "bool",
    Vfol.G(f id))

let type_of_option tau f =
  let id = Id.gen_exp_var () in
  ExpType.Adt(
    id,
    [tau],
    "option",
     Vfol.G(f id))

let type_of_array tau f =
  let id = Id.gen_exp_var () in
  ExpType.Adt(
    id,
    [tau],
    "array",
     Vfol.G(f id))

let type_schema_of_int n =
  embed (type_of_int (fun id -> Fol.eq (Arith.Var(id)) (Arith.of_int n)))

let type_schema_of_unary_minus =
 embed
   (let id1 = Id.gen_exp_var () in
   ExpType.Fun(
       id1,
       type_of_int (fun _ -> Fol.True),
       type_of_int (fun id -> Fol.eq (Arith.Var(id)) (Arith.BinOp(BinOp.Sub, Arith.of_int 0, Arith.Var(id1))))))

let type_schema_of_binop k=
  embed
      (let id1 = Id.gen_exp_var () in
      let id2 = Id.gen_exp_var () in
      ExpType.Fun(
        id1,
        type_of_int (fun _ -> Fol.True),
        ExpType.Fun(
          id2,
          type_of_int (fun _ -> Fol.True),
          (let id = Id.gen_exp_var () in
          type_of_int (fun id -> Fol.eq (Arith.Var(id)) (Arith.BinOp(k, Arith.Var(id1), Arith.Var(id2))))))))

let type_schema_of_binrel k=
  embed
      (let id1 = Id.gen_exp_var () in
      let id2 = Id.gen_exp_var () in
      ExpType.Fun(
        id1,
        type_of_int (fun _ -> Fol.True),
        ExpType.Fun(
          id2,
          type_of_int (fun _ -> Fol.True),
       type_of_bool
         (fun id ->
           let phi11 = Fol.BinRel(Arith.Var(id1), k, Arith.Var(id2)) in
           let phi12 = Fol.eq (Arith.Var(id)) Arith.atrue in
           let phi21 = Fol.BinRel(Arith.Var(id1), BinRel.complement k, Arith.Var(id2)) in
           let phi22 = Fol.eq (Arith.Var(id)) Arith.afalse in
              Fol.And(Fol.Imply(phi11, phi12), Fol.Imply(phi21, phi22))))))

let type_of_unit = ExpType.Adt(Id.gen_exp_var (), [], "unit", Vfol.G(Fol.True))
let type_schema_of_unit = embed (type_of_unit)

let type_schema_of_none =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    ExpType.Adt(
      Id.gen_exp_var (),
      [ExpType.Var(tv)],
      "option",
      Vfol.G(Fol.True))])
let type_schema_of_some =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    ExpType.Fun(
      Id.gen_exp_var (),
      ExpType.Var(tv),
            ExpType.Adt(
              Id.gen_exp_var (),
              [ExpType.Var(tv)],
              "option",
              Vfol.G(Fol.True)))])

let type_schema_of_true = embed (let id = Id.gen_exp_var () in ExpType.Adt(id, [], "bool", Vfol.G(Fol.eq (Arith.Var(id)) Arith.atrue)))
let type_schema_of_false = embed (let id = Id.gen_exp_var () in ExpType.Adt(id, [], "bool", Vfol.G(Fol.eq (Arith.Var(id)) Arith.afalse)))
let type_schema_of_nil =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    ExpType.Adt(
      Id.gen_exp_var (),
      [ExpType.Var(tv)],
      "list",
      Vfol.G(Fol.True))])
let type_schema_of_cons =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    ExpType.Fun(
      Id.gen_exp_var (),
      ExpType.Tuple
        (Id.gen_exp_var (),
        [ExpType.Var(tv);
        ExpType.Adt(
          Id.gen_exp_var (),
          [ExpType.Var(tv)],
          "list",
          Vfol.G(Fol.True))],
        Vfol.G(Fol.True)),
      ExpType.Adt(
        Id.gen_exp_var (),
        [ExpType.Var(tv)],
        "list",
        Vfol.G(Fol.True)))])
let type_schema_of_snil =
  let id = Id.gen_exp_var () in
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    ExpType.Adt(
      id,
      [ExpType.Var(tv)],
      "sized_list",
      Vfol.G(
        Fol.eq (Arith.len (Arith.Var(id))) (Arith.of_int 0)))])
let type_schema_of_scons =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      ExpType.Tuple
        (Id.gen_exp_var (),
        [ExpType.Var(tv);
        ExpType.Adt(
          Id.gen_exp_var (),
          [ExpType.Var(tv)],
          "sized_list",
          Vfol.G(Fol.True))],
        Vfol.G(Fol.True)),
      let id2 = Id.gen_exp_var () in
      ExpType.Adt(
        id2,
        [ExpType.Var(tv)],
        "sized_list",
        Vfol.G(
          Fol.eq
            (Arith.len (Arith.Var(id2)))
            (Arith.BinOp(BinOp.Add,
              Arith.len (Arith.proj id1 2),
              Arith.of_int 1)))))])
let type_schema_of_onil =
  Forall(
    [[],
    let id = Id.gen_exp_var () in
    ExpType.Adt(
      id,
      [],
      "ordered_list",
      Vfol.G(Fol.eq (Arith.Var(id)) Arith.nil))])
let type_schema_of_ocons =
  Forall(
    [[],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      (let id = Id.gen_exp_var () in
      ExpType.Tuple(
        id,
        [ExpType.make_int_type ();
        ExpType.Adt(
          Id.gen_exp_var (),
          [],
          "ordered_list",
          Vfol.G(Fol.True))],
        Vfol.G(
          Fol.Or(
            Fol.eq (Arith.proj id 2) Arith.nil,
            (*Fol.And(
              Fol.Not(Fol.eq (Arith.proj id 2) Arith.nil),*)
              Fol.leq
                (Arith.proj id 1)
                (Arith.hd (Arith.proj id 2))(*)*))))),
      (let id2 = Id.gen_exp_var () in
      ExpType.Adt(
        id2,
        [],
        "ordered_list",
        Vfol.G(
          Fol.And(
            Fol.Not(Fol.eq (Arith.Var(id2)) Arith.nil),
            Fol.eq (Arith.hd (Arith.Var(id2))) (Arith.proj id1 1))))))])

let type_schema_of_inil =
  Forall(
    [[],
    let id = Id.gen_exp_var () in
    ExpType.Adt(
      id,
      [],
      "intlist",
      Vfol.G(
        Fol.And(
          Fol.eq (Arith.Var(id)) Arith.nil,
          Fol.eq (Arith.Con("ord", [Arith.Var(id)])) Arith.atrue)))])
let type_schema_of_icons =
  Forall(
    [[],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      (ExpType.Tuple(
        Id.gen_exp_var (),
        [ExpType.make_int_type ();
        ExpType.Adt(
          Id.gen_exp_var (),
          [],
          "intlist",
          Vfol.G(Fol.True))],
        Vfol.G(Fol.True))),
      (let id2 = Id.gen_exp_var () in
      ExpType.Adt(
        id2,
        [],
        "intlist",
        Vfol.G(
          Fol.list_conj
            [Fol.Not(Fol.eq (Arith.Var(id2)) Arith.nil);
            Fol.eq (Arith.hd (Arith.Var(id2))) (Arith.proj id1 1);
            Fol.Imply
              (Fol.Or(
                            Fol.eq (Arith.proj id1 2) Arith.nil,
                            (*Fol.And(
                              Fol.Not(Fol.eq (Arith.proj id 2) Arith.nil),*)
                              Fol.leq
                                (Arith.proj id1 1)
                                (Arith.hd (Arith.proj id1 2))(*)*)),
              Fol.eq (Arith.Con("ord", [Arith.Var(id2)])) Arith.atrue)]))))])


let type_schema_of_qnil =
  Forall(
    [[],
    let id = Id.gen_exp_var () in
    ExpType.Adt(
      id,
      [],
      "qordered_list",
      Vfol.G(Fol.eq (Arith.Var(id)) Arith.nil))])
let type_schema_of_qcons =
  Forall(
    [[],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      (let id = Id.gen_exp_var () in
      ExpType.Tuple(
        id,
        [ExpType.make_int_type ();
        ExpType.Adt(
          Id.gen_exp_var (),
          [],
          "qordered_list",
          Vfol.G(Fol.True))],
        Vfol.G(
          Fol.Or(
            Fol.eq (Arith.proj id 2) Arith.nil,
            (*Fol.And(
              Fol.Not(Fol.eq (Arith.proj id 2) Arith.nil),*)
              Fol.leq
                (Arith.proj id 1)
                (Arith.hd (Arith.proj id 2))(*)*))))),
      (let id2 = Id.gen_exp_var () in
      ExpType.Adt(
        id2,
        [],
        "qordered_list",
        Vfol.G(
          Fol.list_conj (
            [Fol.Not(Fol.eq (Arith.Var(id2)) Arith.nil);
            Fol.eq (Arith.hd (Arith.Var(id2))) (Arith.proj id1 1);
            Fol.Or(
              Fol.And(
                Fol.eq (Arith.proj id1 2) Arith.nil,
                Fol.eq (Arith.proj id1 1) (Arith.ub (Arith.Var(id2)))),
              Fol.And(
                Fol.Not(Fol.eq (Arith.proj id1 2) Arith.nil),
                Fol.eq (Arith.ub (Arith.proj id1 2)) (Arith.ub (Arith.Var(id2)))))])))))])

let type_schema_of_arraysize =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_array (ExpType.Var(tv)) (fun _ -> Fol.True),
      type_of_int (fun id -> Fol.eq (Arith.Var(id)) (Arith.len (Arith.Var(id1)))))])

let type_schema_of_update =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_array (ExpType.Var(tv)) (fun _ -> Fol.True),
      ExpType.Fun(
        Id.gen_exp_var (),
        type_of_int
          (fun id ->
            Fol.And(
              Fol.leq
                (Arith.of_int 0)
                (Arith.Var(id)),
(**)
              Fol.lt
                (Arith.Var(id))
                (Arith.len (Arith.Var(id1)))
(**)
(*
              Fol.leq
                (Arith.add (Arith.Var(id)) (Arith.of_int 1))
                (Arith.len (Arith.Var(id1)))
*)
            )),
        ExpType.Fun(
          Id.gen_exp_var (),
          ExpType.Var(tv),
          type_of_unit)))])

let type_schema_of_sub =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_array (ExpType.Var(tv)) (fun _ -> Fol.True),
      ExpType.Fun(
        Id.gen_exp_var (),
        type_of_int
          (fun id ->
            Fol.And(
              Fol.leq
                (Arith.of_int 0)
                (Arith.Var(id)),
(**)
              Fol.lt
                (Arith.Var(id))
                (Arith.len (Arith.Var(id1)))
(**)
(*
              Fol.leq
                (Arith.add (Arith.Var(id)) (Arith.of_int 1))
                (Arith.len (Arith.Var(id1)))
*)
            )),
              ExpType.Var(tv)))])

let type_schema_of_alloc =
  let tv = Id.gen_type_var () in
  Forall(
    [[tv],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_int (fun id -> Fol.geq (Arith.Var(id)) (Arith.of_int 0)),
      ExpType.Fun(
        Id.gen_exp_var (),
        ExpType.Var(tv),
        type_of_array
          (ExpType.Var(tv))
          (fun id -> Fol.eq (Arith.len (Arith.Var(id))) (Arith.Var(id1)))))])

let type_schema_of_abs =
  Forall(
    [[],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_int (fun _ -> Fol.True),
      type_of_int
        (fun id ->
          Fol.Or(
                  Fol.And(
                    Fol.leq (Arith.Var(id1)) (Arith.of_int 0),
                    Fol.eq (Arith.Var(id)) (Arith.mul (Arith.of_int (-1)) (Arith.Var(id1)))),
                  Fol.And(
                    Fol.gt (Arith.Var(id1)) (Arith.of_int 0),
                    Fol.eq (Arith.Var(id)) (Arith.Var(id1))))))])

let type_schema_of_mid =
  Forall(
    [[],
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_int (fun _ -> Fol.True),
      let id2 = Id.gen_exp_var () in
      ExpType.Fun(
        id2,
        type_of_int (fun _ -> Fol.True),
              type_of_int
                (fun id ->
                  Fol.And(
                    Fol.leq (Arith.Var(id1)) (Arith.Var(id)),
                    Fol.leq (Arith.Var(id)) (Arith.Var(id2))))))])

let type_schema_of_bcopy =
  let tv = Id.gen_type_var () in
  let tau =
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_array (ExpType.Var(tv)) (fun _ -> Fol.True),
      ExpType.Fun(
        Id.gen_exp_var (),
        type_of_array (ExpType.Var(tv)) (fun id -> Fol.leq (Arith.len (Arith.Var(id1))) (Arith.len (Arith.Var(id)))),
        type_of_unit)) in
  Forall([[tv], ExpType.Fun(Id.gen_exp_var (), tau, type_of_unit)])

let type_schema_of_dotprod =
  let tau =
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      type_of_array (type_of_int (fun _ -> Fol.True)) (fun _ -> Fol.True),
      ExpType.Fun(
        Id.gen_exp_var (),
        type_of_array (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.eq (Arith.len (Arith.Var(id1))) (Arith.len (Arith.Var(id)))),
        type_of_int (fun _ -> Fol.True))) in
  Forall([[], ExpType.Fun(Id.gen_exp_var (), tau, type_of_unit)])

(*
let type_schema_of_bsearch =
  let tau =
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      (type_of_int (fun _ -> Fol.True)),
      ExpType.Fun(
        Id.gen_exp_var (),
        type_of_array (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.True),
        type_of_option (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.True))) in
  Forall([[], ExpType.Fun(Id.gen_exp_var (), tau, type_of_unit)])
*)

let type_schema_of_queens =
  let tau =
    let id1 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      (type_of_int (fun _ -> Fol.True)),
      type_of_unit) in
  Forall([[], ExpType.Fun(Id.gen_exp_var (), tau, type_of_unit)])

let type_schema_of_combine =
  let tv = Id.gen_type_var () in
  let tau =
    let id1 = Id.gen_exp_var () in
    let id2 = Id.gen_exp_var () in
    ExpType.Fun(
      id1,
      ExpType.Tuple
        (id2,
        [ExpType.Adt(
          Id.gen_exp_var (),
          [ExpType.Var(tv)],
          "sized_list",
          Vfol.G(Fol.True));
        ExpType.Adt(
          Id.gen_exp_var (),
          [ExpType.Var(tv)],
          "sized_list",
          Vfol.G(Fol.True))],
        Vfol.G(Fol.eq (Arith.len (Arith.proj id2 1)) (Arith.len (Arith.proj id2 2)))),
      type_of_unit) in
  Forall([[tv], tau])

let type_schema_of_move =
  Forall(
    [[],
    ExpType.make_fun_type
      [(fun _ -> type_of_int (fun _ -> Fol.True));
      (fun _ -> type_of_int (fun _ -> Fol.True));
      (fun _ -> type_of_array (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.True));
      (fun _ -> type_of_int (fun _ -> Fol.True));
      (fun _ -> type_of_array (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.True));
      (fun _ -> type_of_int (fun _ -> Fol.True));
      (fun _ -> type_of_array (type_of_int (fun _ -> Fol.True)) (fun id -> Fol.True));
      (fun ids ->
        type_of_int
          (fun id8 ->
            let id1::id2::id3::id4::id5::id6::id7::[] = ids in
            let size = Arith.Var(id1) in
            let n = Arith.Var(id2) in
            let a1 = Arith.Var(id3) in
            let s = Arith.Var(id4) in
            let a2 = Arith.Var(id5) in
            let d1 = Arith.Var(id6) in
            let a3 = Arith.Var(id7) in
            let d2 = Arith.Var(id8) in
            Fol.list_conj
              [Fol.lt (Arith.of_int 0) n;
               Fol.leq (Arith.of_int 0) s;
               Fol.eq (Arith.len a1) size;
               Fol.eq (Arith.len a2) size;
               Fol.eq (Arith.len a3) size;
               Fol.leq d1 size;
               Fol.leq d2 size;
               Fol.eq (Arith.add s (Arith.add d1 d2)) (Arith.add size size);
               Fol.leq (Arith.add s n) size;
               Fol.leq n d1;
               Fol.leq n d2;
              ]))]
      (fun _ -> type_of_unit)])

let type_schema_of_ordered =
  let tau =
    let id = Id.gen_exp_var () in
    ExpType.Adt(
      id,
      [],
      "intlist",
      Vfol.G(Fol.eq (Arith.Con("ord", [Arith.Var(id)])) Arith.atrue))
  in
  Forall([[], ExpType.Fun(Id.gen_exp_var (), tau, type_of_unit)])
*)
*)
