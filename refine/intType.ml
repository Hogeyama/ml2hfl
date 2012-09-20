open ExtList
open ExtString

(** Interaction types *)

(** only argument side can have intersection types *)
type s =
  Unit of Var.t
| Bool of Var.t
| Int of Var.t
| Fun of s * s
| And of s list
and t = { shape: s; pre: Term.t; post: Term.t }

let make shape pre post = { shape = shape; pre = pre; post = post }

let make_fun_shape args ret =
  List.fold_right
    (fun arg ret -> Fun(arg, ret))
    args
    ret

let of_simple_type sty =
  let rec aux sty =
    match sty with
      SimType.Unit -> Unit(Var.make (Idnt.new_id ()))
    | SimType.Bool -> Bool(Var.make (Idnt.new_id ()))
    | SimType.Int -> Int(Var.make (Idnt.new_id ()))
    | SimType.Fun(sty1, sty2) ->
        Fun(aux sty1, aux sty2)
  in
  make (aux sty) Formula.ttrue Formula.ttrue

let is_base sh =
  match sh with
    Unit(_) | Bool(_) | Int(_) -> true
  | Fun(_, _) | And(_) -> false

let rec pr_shape ppf sh =
  match sh with
    Unit(x) ->
      Format.fprintf ppf "unit[%a]" Var.pr x
  | Bool(x) ->
      Format.fprintf ppf "bool[%a]" Var.pr x
  | Int(x) ->
      Format.fprintf ppf "int[%a]" Var.pr x
  | Fun(sh1, sh2) ->
      let _ = Format.fprintf ppf "@[<hv>" in
      let _ =
        if is_base sh1 then
          Format.fprintf ppf "%a " pr_shape sh1
        else
          Format.fprintf ppf "(%a) " pr_shape sh1
      in
      let _ = Format.fprintf ppf "->@ " in
      Format.fprintf ppf "%a@]" pr_shape sh2
  | And(shs) ->
      (match shs with
        [] ->
          Format.fprintf ppf "Top"
      | _ ->
          Format.fprintf ppf "@[<hv>%a@]" (Util.pr_list pr_shape " /\\@ ") shs)

let rec pr ppf ty =
  match ty.pre, ty.post with
    Term.Const(_, Const.True), Term.Const(_, Const.True) ->
      Format.fprintf ppf "%a" pr_shape ty.shape
  | _, _ ->
      Format.fprintf ppf "<@[<hov>%a |@ %a |@ %a@]>" Term.pr ty.pre pr_shape ty.shape Term.pr ty.post

let pr_bind ppf (f, ty) = Format.fprintf ppf "%a: %a" Var.pr f pr ty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

let pr_shape_bind ppf (x, sh) = Format.fprintf ppf "%a: %a" Var.pr x pr_shape sh
let pr_shape_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_shape_bind "@ ") env

let rec rename_shape sub ty =
  match ty with
    Unit(x) ->
      Unit(try sub x with Not_found -> x)
  | Bool(x) ->
      Bool(try sub x with Not_found -> x)
  | Int(x) ->
      Int(try sub x with Not_found -> x)
  | Fun(sh1, sh2) ->
      Fun(rename_shape sub sh1, rename_shape sub sh2)
  | And(shs) ->
      And(List.map (rename_shape sub) shs)

let rename sub ty =
  let subst x = Term.make_var (sub x) in
  make
    (rename_shape sub ty.shape)
    (TypSubst.subst subst ty.pre)
    (TypSubst.subst subst ty.post)

let rec typed_unify new_var shs =
  if shs = [] then
    []
  else
    match List.hd shs with
      Unit(_) ->
        let y = new_var () in
        List.map (function Unit(x) -> (x, y), SimType.Unit | _ -> assert false) shs
    | Bool(_) ->
        let y = new_var () in
        List.map (function Bool(x) -> (x, y), SimType.Bool | _ -> assert false) shs
    | Int(_) ->
        let y = new_var () in
        List.map (function Int(x) -> (x, y), SimType.Int | _ -> assert false) shs
    | Fun(_) ->
        let shs1, shs2 = List.split (List.map (function (Fun(sh1, sh2)) -> sh1, sh2 | _ -> assert false) shs) in
        typed_unify new_var shs1 @ typed_unify new_var shs2
    | And(_) ->
        let shss = List.map (function (And(shs)) -> shs | _ -> assert false) shs in
        Util.concat_map (typed_unify new_var) (Util.transpose shss)

let unify new_var tys =
  List.map fst (typed_unify new_var tys)

let rec fresh_names new_var sh =
  match sh with
    Unit(x)
  | Bool(x)
  | Int(x) ->
      let y = new_var () in
      [x, y]
  | Fun(sh1, sh2) ->
      fresh_names new_var sh1 @
      fresh_names new_var sh2
  | And(shs) ->
      Util.concat_map (fresh_names new_var) shs

let rec env_of sh =
  match sh with
    Unit(x) -> [x, SimType.Unit]
  | Bool(x) -> [x, SimType.Bool]
  | Int(x) -> [x, SimType.Int]
  | Fun(sh1, sh2) ->
      List.unique (env_of sh1 @ env_of sh2)
  | And(shs) ->
      List.unique (Util.concat_map env_of shs)

let alpha ty =
  let sub = fresh_names Var.new_var ty.shape in
  rename (fun x -> List.assoc x sub) ty

let canonize ty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V(Idnt.make ("v" ^ (string_of_int !cnt)))
  in
  let sub = fresh_names new_var ty.shape in
  rename (fun x -> List.assoc x sub) ty


let rec flatten sh =
  match sh with
    Unit(_) | Bool(_) | Int(_) -> sh
  | Fun(sh1, sh2) -> Fun(flatten sh1, flatten sh2)
  | And(shs) ->
      And
        (Util.concat_map
          (function (And(shs)) -> shs | sh -> [sh])
          (List.map flatten shs))

let intersect_shapes shs =
  match shs with
    [sh] -> sh
  | _ -> flatten (And(shs))
(*
  | [] -> assert false
  | _ ->
      let _ = Format.printf "%a@," (Util.pr_list pr_shape ":") shs in
      assert false
*)

let shape_env_of env fcs =
  let tlfcs =
    List.unique
      (List.filter
        (fun (x, _) -> Var.is_top x)
        fcs)
  in
  let rec shape_of (x, uid) =
    match env x with
      SimType.Unit ->
        (* the predicate discovery algorithm assumes that top level functions are of a function type
           but actual implementation of MoCHi violates the assumption *)
        Unit(x)
(*
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
*)
    | SimType.Bool ->
        Bool(x)
    | SimType.Int ->
        Int(x)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        (*
        Format.printf "%a@," Var.pr (Var.T(x, uid, n));
        *)
        make_fun_shape
          (List.init n
            (fun i ->
              (*
              Format.printf "%a@," Var.pr (Var.T(x, uid, i));
              *)
              intersect_shapes (shapes_of (Var.T(x, uid, i)))))
          (intersect_shapes (shapes_of (Var.T(x, uid, n))))
(*
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
*)
  and shapes_of x =
    match env x with
      SimType.Unit -> [Unit(x)]
    | SimType.Bool -> [Bool(x)]
    | SimType.Int -> [Int(x)]
    | SimType.Fun(_, _) as ty ->
        let res =
          List.map
            (fun uid -> shape_of (x, uid))
            (List.filter_map (fun (y, uid) -> if x = y then Some(uid) else None) fcs)
        in
        if res = [] then [(of_simple_type ty).shape] else res
  in
  List.map (fun x_uid -> x_uid, shape_of x_uid) tlfcs

let of_summaries prog sums fcs =
  List.map
    (fun (x_uid, sh) ->
      let pres = List.filter_map (function `Pre(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      let posts = List.filter_map (function `Post(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      fst x_uid,
      canonize
        (make sh
          (if pres = [] then (*???*)Formula.ttrue else Formula.band(*???*) pres)
          (if posts = [] then (*???*)Formula.ttrue else Formula.bor(*???*) posts)))
    (shape_env_of (Prog.type_of prog) fcs)

let simplify ty =
  match ty.shape with
    And(shs) when List.for_all is_base shs ->
      let sub = unify Var.new_var shs in
      make
        (rename_shape (fun x -> List.assoc x sub) (List.hd shs))
        (TypSubst.subst (fun x -> Term.make_var (List.assoc x sub)) ty.pre)
        (TypSubst.subst (fun x -> Term.make_var (List.assoc x sub)) ty.post)
  | _ -> ty

let intersect tys =
  match tys with
    [] -> assert false
  | [ty] -> ty
  | _ ->
      let shs = List.map (fun ty -> ty.shape) tys in
      let pres = List.map (fun ty -> ty.pre) tys in
      let posts = List.map (fun ty -> ty.post) tys in
      simplify
        (make
          (intersect_shapes shs)
          (Formula.bor pres)
          (Formula.band (List.map2 (fun pre post -> Formula.imply pre post) pres posts)))

let intersect_env env =
  List.map
    (fun f_tys -> fst (List.hd f_tys), intersect (List.map snd f_tys))
    (Util.classify (fun (x, _) (y, _) -> Var.equiv x y) env)


let shapes_of sh =
  match sh with
    Unit(_) | Bool(_) | Int(_) | Fun(_, _) -> [sh]
  | And(shs) -> shs

let rec coerce pos sh1 sh2 phi =
  match shapes_of sh1, shapes_of sh2 with
    [], _ | _, [] -> assert false
  | [Unit(x)], [Unit(y)]
  | [Bool(x)], [Bool(y)]
  | [Int(x)], [Int(y)] ->
      TypSubst.subst (fun z -> if Var.equiv x z then Term.make_var y else raise Not_found) phi
  | [Fun(sh11, sh12)], [Fun(sh21, sh22)] ->
      coerce (not pos) sh11 sh21 (coerce pos sh12 sh22 phi)
  | shs1, shs2 ->
      let n1 = List.length shs1 in
      let n2 = List.length shs2 in
      if pos then
        let fs = Util.maps n1 n2 in
        Formula.band
          (List.map
            (fun f ->
              List.fold_right
                (fun i phi ->
                  let fi = List.assoc i f in
                  coerce pos (List.nth shs1 i) (List.nth shs2 fi) phi)
                (List.init n1 (fun j -> j))
                phi)
            fs)
      else
        let fs = Util.maps n2 n1 in
        Formula.bor
          (List.map
            (fun f ->
              List.fold_right
                (fun i phi ->
                  let finvi = List.filter_map (fun (x, y) -> if y = i then Some(x) else None) f in
                  coerceNegSet (List.nth shs1 i) (Util.filteri (fun j _ -> List.mem j finvi) shs2) phi)
                (List.init n1 (fun x -> x))
                phi)
            fs)
and coerceNegSet sh shs phi =
  Formula.exists
    (env_of sh)
    (Formula.band (List.map (fun sh' -> coerce false sh sh' phi) shs))

let compose ty1 ty2 =
  (*
  let _ = Format.printf "%a %a@ " pr ty1 pr ty2 in
  *)
  match ty1.shape with
    Unit(_) | Bool(_) | Int(_) ->
      assert false
  | _ ->
      let shs1, shs2 =
        match flatten ty1.shape with
          Fun(sh1, sh2) ->
            [sh1], [sh2]
        | And(shs) ->
            List.split
              (List.map
                (function (Fun(sh1, sh2)) -> sh1, sh2 | _ -> assert false)
                shs)
        | _ -> assert false
      in
      let sh1 = intersect_shapes shs1 in
      let sh2 = intersect_shapes shs2 in
      let pre1 = ty1.pre in
      let post1 = ty1.post in
      let post1' = coerce false sh1 ty2.shape post1 in
      let pre2 = ty2.pre in
      let post2 = ty2.post in
      let post2' = coerce true ty2.shape sh1 post2 in
      let env = List.unique (env_of sh1) in
      let pre = Formula.band
        [Formula.forall env (Formula.imply post2' pre1);
         Formula.forall (env_of ty2.shape) (Formula.imply post1' pre2)]
      in
      let post = Formula.band
        [Formula.exists env (Formula.band [post1; post2']);
         Formula.exists (env_of ty2.shape) (Formula.band [post1'; post2])]
      in
      make sh2 pre post

let type_of_const c =
  (*
  Format.printf "%a@ " Const.pr c;
  *)
  match c with
    Const.Event(id) when Idnt.string_of id = Term.event_fail ->
      (** ToDo *)
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (make_fun_shape [Unit(x1)] (Unit(x2)))
        Formula.tfalse
        Formula.ttrue
  | Const.Event(_) ->
      assert false
  | Const.Unit ->
      let x = Var.new_var () in
      make (Unit(x)) Formula.ttrue (Formula.eqUnit (Term.make_var x) (Term.tunit))
  | Const.True ->
      let x = Var.new_var () in
      make (Bool(x)) Formula.ttrue (Formula.iff (Term.make_var x) (Formula.ttrue))
  | Const.False ->
      let x = Var.new_var () in
      make (Bool(x)) Formula.ttrue (Formula.iff (Term.make_var x) (Formula.tfalse))
  | Const.And ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.band [Term.make_var x1; Term.make_var x2]))
  | Const.Or ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.bor [Term.make_var x1; Term.make_var x2]))
  | Const.Imply ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.imply (Term.make_var x1) (Term.make_var x2)))
  | Const.Iff ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.iff (Term.make_var x1) (Term.make_var x2)))
  | Const.Not ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1)] (Bool(x2)))
        Formula.ttrue
        (Formula.iff (Term.make_var x2) (Formula.bnot (Term.make_var x1)))
  | Const.Lt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.lt (Term.make_var x1) (Term.make_var x2)))
  | Const.Gt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.gt (Term.make_var x1) (Term.make_var x2)))
  | Const.Leq ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.leq (Term.make_var x1) (Term.make_var x2)))
  | Const.Geq ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.geq (Term.make_var x1) (Term.make_var x2)))
  | Const.EqBool ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.eqBool (Term.make_var x1) (Term.make_var x2)))
  | Const.NeqBool ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Bool(x1); Bool(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.neqBool (Term.make_var x1) (Term.make_var x2)))
  | Const.EqUnit ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Unit(x1); Unit(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.eqUnit (Term.make_var x1) (Term.make_var x2))) (**ToDo*)
  | Const.NeqUnit ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Unit(x1); Unit(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.neqUnit (Term.make_var x1) (Term.make_var x2)))
  | Const.EqInt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.eqInt (Term.make_var x1) (Term.make_var x2)))
  | Const.NeqInt ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Bool(x3)))
        Formula.ttrue
        (Formula.iff (Term.make_var x3) (Formula.neqInt (Term.make_var x1) (Term.make_var x2)))
  | Const.Int(n) ->
      let x = Var.new_var () in
      make (Int(x)) Formula.ttrue (Formula.eqInt (Term.make_var x) (Term.tint n))
  | Const.RandInt ->
      (** ToDo *)
      let x = Var.new_var () in
      make (Int(x)) Formula.ttrue Formula.ttrue
(*
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (Fun([Unit(x1), Int(x2)]))
        Formula.ttrue
        Formula.ttrue
*)
  | Const.Add ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Int(x3)))
        Formula.ttrue
        (Formula.eqInt (Term.make_var x3) (Term.add (Term.make_var x1) (Term.make_var x2)))
  | Const.Sub ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Int(x3)))
        Formula.ttrue
        (Formula.eqInt (Term.make_var x3) (Term.sub (Term.make_var x1) (Term.make_var x2)))
  | Const.Mul ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      let x3 = Var.new_var () in
      make
        (make_fun_shape [Int(x1); Int(x2)] (Int(x3)))
        Formula.ttrue
        (Formula.eqInt (Term.make_var x3) (Term.mul (Term.make_var x1) (Term.make_var x2)))
  | Const.Minus ->
      let x1 = Var.new_var () in
      let x2 = Var.new_var () in
      make
        (make_fun_shape [Int(x1)] (Int(x2)))
        Formula.ttrue
        (Formula.eqInt (Term.make_var x2) (Term.minus (Term.make_var x1)))
