open ExtList
open IntType

(** Interaction type checking *)

exception Ill_typed

let subtype ty1 ty2 =
  let pre1 = ty1.pre in
  let post1 = ty1.post in
  let pre2 = ty2.pre in
  let post2 = ty2.post in
  Formula.band
    [Formula.imply (coerce true ty2.shape ty1.shape pre2) pre1;
     Formula.imply
       (Formula.band
         [coerce true ty1.shape ty2.shape
            (Formula.band
              [coerce true ty2.shape ty1.shape pre2;
               post1]);
          pre2])
       post2]

let rec infer_term cenv env t =
  let f, args = Term.fun_args t in
  let ty_f =
    match f with
      Term.Var(_, x) when not (Var.is_coeff x) ->
        (try
          alpha (List.assoc x cenv)
        with Not_found ->
          (try
            let copy_cat sh =
              let subty = typed_unify Var.new_var [sh] in
              let sub = List.map fst subty in
              let sh' = rename_shape (fun x -> List.assoc x sub) sh in
              make
                sh'
                Formula.ttrue
                (Formula.band
                  (List.map
                    (fun ((x, y), ty) ->
                      Formula.eq_ty ty (Term.make_var y) (Term.make_var x))
                    subty))
            in
            copy_cat (List.assoc x env)
          with Not_found ->
            let _ = Format.printf "\"%a\" not found@ " Var.pr x in
            let _ = Format.printf "type env: %a@ " pr_env cenv in
            let _ = Format.printf "shape env: %a@ " pr_shape_env env in
            raise Ill_typed))
    | Term.Const(_, c) ->
        type_of_const c
    | Term.App(_, _, _) | Term.Call(_, _, _) | Term.Ret(_, _, _, _) | Term.Error(_) | Term.Forall(_, _, _) | Term.Exists(_, _, _) ->
        assert false
  in
  let tys = List.map (infer_term cenv env) args in
  let ty = List.fold_left compose ty_f tys in
  (*
  let _ = Format.printf "sty_f: %a@ " pr sty_f in
  *)
  let _ = Format.printf "@[<v>type inference of term: %a@   env: {%a}@   type: %a@ @]" Term.pr t pr_shape_env env pr ty in
  ty

(** require: ty.shape is not of the form And(_) for simplicity *)
let check_fdef cenv fdef ty =
  let args =
    let rec args_ret sh =
      match sh with
        Fun(sh1, sh2) ->
          let args, ret = args_ret sh2 in
          sh1::args, ret
      | Unit(_) | Bool(_) | Int(_) ->
          [], sh
      | _ -> let _ = Format.printf "%a@," pr_shape sh in assert false
    in
    let args, ret = args_ret ty.shape in
    let _ = assert (is_base ret) in
    args
  in
  let env =
    try
      List.map2
        (fun x sh -> Var.V(x), sh)
        fdef.Fdef.args
        args
    with List.Different_list_size(_) ->
      assert false
  in
  let guard =
    let ty = infer_term cenv env fdef.Fdef.guard in
    let _ = if not (Term.equiv ty.pre Formula.ttrue) then let _ = Format.printf "%a@," pr ty in assert false in
    match ty.shape with
      Bool(x) -> Term.subst (fun y -> if Var.equiv x y then Formula.ttrue else raise Not_found) ty.post
    | _ -> let _ = Format.printf "%a@," pr ty in assert false
  in
  (*
  let _ = Format.printf "guard: %a@ " Term.pr guard in
    *)
  try
    let rty = infer_term cenv env fdef.Fdef.body in
    let vc = subtype (make (make_fun_shape args rty.shape) rty.pre rty.post) (make ty.shape (Formula.band [guard; ty.pre]) ty.post) in
    let _ = Format.printf "@[<v>type checking@   verification condition: %a@ @]" Term.pr vc in
    Cvc3Interface.is_valid vc
    (*CsisatInterface.implies Formula.ttrue vc*)
  with Ill_typed ->
    false

let check_prog cand_env prog =
  let env =
    Util.fixed_point
      (fun env ->
        List.filter
          (function (Var.V(id), ty) ->
            Format.printf "@[<v>checking if \"%a\" has %a:@   @]" Idnt.pr id pr ty;
            let fdefs = Prog.fdefs_of prog id in
            let res =
              List.for_all
                (fun fdef -> check_fdef (intersect_env env) fdef ty)
                fdefs
            in
            let _ =
              if res then
                Format.printf "well-typed@ "
              else
                Format.printf "ill-typed@ "
            in
            res
          | _ -> assert false)
          env)
      (fun env1 env2 ->
        (*
        Format.printf "%d, %d@ " (List.length env1) (List.length env2);
          *)
        List.length env1 = List.length env2)
      cand_env
  in
  List.exists
    (function (Var.V(id), ty) ->
      id = prog.Prog.main (* && ty is a subtype of a required type *)
    | _ -> assert false)
    env
