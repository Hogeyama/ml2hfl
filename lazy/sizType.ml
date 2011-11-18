open ExtList
open ExtString

(* only argument side can have intersection types *)
type s =
  Unit of Var.t
| Bool of Var.t
| Int of Var.t
| Fun of (s * s) list
and t = { shape: s; pre: Term.t; post: Term.t }

let make shape pre post = { shape = shape; pre = pre; post = post }

let of_simple_type sty =
		let rec aux sty =
		  match sty with
		    SimType.Unit -> Unit(Var.make (Idnt.new_id ()))
		  | SimType.Bool -> Bool(Var.make (Idnt.new_id ()))
		  | SimType.Int -> Int(Var.make (Idnt.new_id ()))
		  | SimType.Fun(sty1, sty2) ->
		      Fun([aux sty1, aux sty2])
  in
  make (aux sty) Term.ttrue Term.ttrue

let make_fun_shape tys ty =
  let n = List.length tys in
  List.fold_right
    (fun ty retty -> Fun [ty, retty])
    tys
    ty

let is_base ty =
  match ty with
    Unit(_) | Bool(_) | Int(_) -> true
  | Fun(_) -> false

let rec pr_shape ppf ty =
  match ty with
    Unit(x) ->
      Format.fprintf ppf "unit[%a]" Var.pr x
  | Bool(x) ->
      Format.fprintf ppf "bool[%a]" Var.pr x
  | Int(x) ->
      Format.fprintf ppf "int[%a]" Var.pr x
  | Fun(xs) ->
      let pr_aux ppf (ty1, ty2) =
        let _ = Format.fprintf ppf "@[<hv>" in
        let _ =
          if is_base ty1 then
            Format.fprintf ppf "%a " pr_shape ty1
          else
            Format.fprintf ppf "(%a) " pr_shape ty1
        in
        let _ = Format.fprintf ppf "->@ " in
        Format.fprintf ppf "%a@]" pr_shape ty2
      in
      let _ =
        match xs with
          [] ->
            Format.fprintf ppf "T"
        | _ ->
            Format.fprintf ppf "@[<hv>%a@]" (Util.pr_list pr_aux " /\\@ ") xs
      in
      ()

let rec pr ppf sty =
  match sty.pre, sty.post with
    Term.Const(_, Const.True), Term.Const(_, Const.True) ->
      Format.fprintf ppf "%a" pr_shape sty.shape
  | _, _ ->
      Format.fprintf ppf "<@[<hov>%a |@ %a |@ %a@]>" Term.pr sty.pre pr_shape sty.shape Term.pr sty.post

let merge_shapes shs =
  match shs with
    Fun(_)::_ ->
        Fun
          (Util.concat_map
            (function (Fun(xs)) -> xs | _ -> assert false)
            shs)
  | [ty] ->
      ty
  | [] -> Fun [](*???*)
  | _ ->
      let _ = Format.printf ":%a:@." (Util.pr_list pr_shape ":") shs in
      assert false

let pr_bind ppf (f, sty) = Format.fprintf ppf "%a: %a" Var.pr f pr sty
let pr_env ppf env = Format.fprintf ppf "@[<v>%a@]" (Util.pr_list pr_bind "@ ") env

let rec rename_shape sub ty =
  match ty with
    Unit(x) ->
      Unit(try sub x with Not_found -> x)
  | Bool(x) ->
      Bool(try sub x with Not_found -> x)
  | Int(x) ->
      Int(try sub x with Not_found -> x)
  | Fun(xs) ->
      Fun
        (List.map
          (fun (ty1, ty2) ->
            rename_shape sub ty1,
            rename_shape sub ty2)
          xs)

let rename sub sty =
  let subst x = Term.make_var2 (sub x) in
  make
    (rename_shape sub sty.shape)
    (Term.subst subst sty.pre)
    (Term.subst subst sty.post)

let rec sub_from_tys new_var tys =
  if tys = [] then
    []
  else
		  match List.hd tys with
		    Unit(_) ->
		      let y = new_var () in
		      List.map (function Unit(x) -> x, y | _ -> assert false) tys
		  | Bool(_) ->
		      let y = new_var () in
		      List.map (function Bool(x) -> x, y | _ -> assert false) tys
		  | Int(_) ->
		      let y = new_var () in
		      List.map (function Int(x)  -> x, y | _ -> assert false) tys
		  | Fun(xs) ->
		      let tys1, tys2 = List.split (List.map (fun (ty1, ty2) -> ty1, ty2) xs) in
		      let sub1 = sub_from_tys new_var tys1 in
		      let sub2 = sub_from_tys new_var tys2 in
		      sub1 @ sub2

let rec sub_from new_var ty =
		match ty with
		  Unit(x)
		| Bool(x)
		| Int(x) ->
		    let y = new_var () in
		    [x, y]
		| Fun(xs) ->
		    Util.concat_map (fun (ty1, ty2) -> let sub1 = sub_from new_var ty1 in let sub2 = sub_from new_var ty2 in sub1 @ sub2) xs

let canonize sty =
  let new_var =
    let cnt = ref 0 in
    fun () -> cnt := !cnt + 1; Var.V("v" ^ (string_of_int !cnt))
  in
  let sub = sub_from new_var sty.shape in
  rename (fun x -> List.assoc x sub) sty



let shape_env_of env fcs =
  let tlfcs =
    List.unique
		    (List.filter
		      (fun (x, _) -> Var.is_top x)
		      fcs)
  in
  let rec shape_of_x_uid (x, uid) =
    match env x with
      SimType.Unit -> Unit(x)
    | SimType.Bool -> Bool(x)
    | SimType.Int -> Int(x)
    | SimType.Fun(_, _) as ty ->
        let n = SimType.arity ty in
        let ret = merge_shapes (shape_of (Var.T(x, uid, n))) in
(*
Format.printf "%a@." Var.pr (Var.T(x, uid, n));
*)
        make_fun_shape
          (List.init n
            (fun i ->
              let args = shape_of (Var.T(x, uid, i)) in
(*
Format.printf "%a@." Var.pr (Var.T(x, uid, i));
*)
              merge_shapes args))
          ret
    | _ ->
        let _ = Format.printf "%a:%d" Var.pr x uid in
        assert false
  and shape_of x =
    match env x with
      SimType.Unit -> [Unit(x)]
    | SimType.Bool -> [Bool(x)]
    | SimType.Int -> [Int(x)]
    | SimType.Fun(_, _) as ty ->
        let res =
		        List.map
		          (fun uid -> shape_of_x_uid (x, uid))
		          (List.filter_map (fun (y, uid) -> if x = y then Some(uid) else None) fcs)
        in
        if res = [] then [(of_simple_type ty).shape] else res
  in
  List.map (fun x_uid -> x_uid, shape_of_x_uid x_uid) tlfcs

let of_summaries senv sums =
  List.map
    (fun (x_uid, sh) ->
      let pres = Util.filter_map (function `Pre(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      let posts = Util.filter_map (function `Post(x_uid', t) when x_uid = x_uid' -> Some(t) | _ -> None) sums in
      fst x_uid, canonize (make sh (if pres = [] then (*???*)Term.ttrue else Term.band pres) (if posts = [] then (*???*)Term.ttrue else Term.bor posts)))
    senv


let check_prog env prog = assert false
