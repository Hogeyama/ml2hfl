open Util

let mk_meth_name_list (JavaProg.Prog(cls_list,exp)) =
  let rec mk_meth_name_list__ meth_list =
    match meth_list with
    | [] -> []
    | (id,Idnt.V meth_name,args,exp) :: tl ->
       meth_name :: (mk_meth_name_list__ tl)
    | _ -> failwith "match error in mk_meth_name_list__"
  in
  let rec mk_meth_name_list_ cls_list =
    match cls_list with
    | [] -> []
    | (cls_id,(super_cls_id,field_list,meth_list)) :: tl ->
       (mk_meth_name_list__ meth_list) @ (mk_meth_name_list_ tl)
  in
  mk_meth_name_list_ cls_list
  |> List.unique

let mk_cls_name_list (JavaProg.Prog(cls_list,exp)) =
  let rec mk_cls_name_list_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_name,(super_cls_id,field_list,meth_list)) :: tl ->
       cls_name :: mk_cls_name_list_ tl
    | _ -> failwith "match error in mk_cls_name_list"
  in
  mk_cls_name_list_ cls_list
  |> List.unique

let rec string_to_ml_type fj str =
  let cls_name_list = mk_cls_name_list fj in
  let meth_name_list = mk_meth_name_list fj in
  let cls_constructors =
    List.map (fun cls_name ->
              List.map (fun meth_name ->
                        Idnt.make (String.capitalize meth_name^"_"^cls_name))
                       meth_name_list)
             cls_name_list
    |> List.flatten in
  match str with
  | "Int"     -> Type.mk_int
  | "Boolean" -> Type.mk_bool
  | "unit"    -> Type.mk_unit
  | "cls"     -> Type.mk_adt (Idnt.make "cls") cls_constructors
  | _ -> Type.mk_adt (Idnt.make "obj") [Idnt.make "Obj"]
and id_to_ml_type fj id =
  match id with
  | Idnt.V s -> string_to_ml_type fj s
  | _ -> failwith "match error in id_to_ml_type"

let mk_meth_name_list_by_cls_name (JavaProg.Prog(cls_list,exp)) cls_name =
  let rec mk_meth_name_list_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl when cls_name' = cls_name ->
       List.map (function (id,Idnt.V meth_name,args,exp) -> meth_name | _ -> failwith "match_error") meth_list
    | h::tl -> mk_meth_name_list_ tl
  in
  mk_meth_name_list_ cls_list
  |> List.unique

let mk_field_list (JavaProg.Prog(cls_list,exp)) cls_name =
  let rec mk_field_list_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl when cls_name' = cls_name -> field_list
    | h::tl -> mk_field_list_ tl
  in
  mk_field_list_ cls_list
  |> List.unique

let get_meth (JavaProg.Prog(cls_list,exp)) meth_name =
  let rec get_meth__ meth_list =
    match meth_list with
    | [] -> []
    | ((Idnt.V return_type,Idnt.V meth_name',args,exp) as meth) :: tl when meth_name' = meth_name -> [meth]
    | h::tl -> get_meth__ tl
  in
  let rec get_meth_ cls_list =
    match cls_list with
    | [] -> failwith "no class found"
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl ->
       if (get_meth__ meth_list) <> [] then List.hd (get_meth__ meth_list) else get_meth_ tl
    | _ -> failwith "no method found in get_meth"
  in
  get_meth_ cls_list

let get_meth_by_cls_name_and_meth_name (JavaProg.Prog(cls_list,exp)) cls_name meth_name =
  let rec get_meth_by_cls_name_and_meth_name__ meth_list =
    match meth_list with
    | [] -> failwith "no meth found"
    | ((Idnt.V return_type,Idnt.V meth_name',args,exp) as meth) :: tl when meth_name' = meth_name -> meth
    | h::tl -> get_meth_by_cls_name_and_meth_name__ tl
  in
  let rec get_meth_by_cls_name_and_meth_name_ cls_list =
    match cls_list with
    | [] -> failwith "no class found in get_meth_by_cls_name_and_meth_name"
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl when cls_name' = cls_name ->
       get_meth_by_cls_name_and_meth_name__ meth_list
    | h::tl -> get_meth_by_cls_name_and_meth_name_ tl
  in
  get_meth_by_cls_name_and_meth_name_ cls_list

let cls_has_meth (JavaProg.Prog(cls_list,exp)) cls_name meth_name =
  let rec mk_meth_name_list__ meth_list =
    match meth_list with
    | [] -> false
    | (id,Idnt.V meth_name',args,exp) :: tl -> if meth_name' = meth_name then true else mk_meth_name_list__ tl
    | _ -> failwith "match error in mk_meth_name_list__"
  in
  let rec mk_meth_name_list_ cls_list =
    match cls_list with
    | [] -> false
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl ->
       if cls_name' = cls_name then mk_meth_name_list__ meth_list else mk_meth_name_list_ tl
    | _ -> failwith "match error in mk_meth_name_list__"
  in
  mk_meth_name_list_ cls_list

let mk_arg_list (JavaProg.Prog(cls_list,exp)) cls_name meth_name =
  let rec mk_arg_list__ meth_list =
    match meth_list with
    | [] -> []
    | (id1,Idnt.V meth_name',args,exp) :: tl when meth_name' = meth_name -> args
    | h::tl -> mk_arg_list__ tl
  in
  let rec mk_arg_list_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl when cls_name' = cls_name ->
       (mk_arg_list__ meth_list)
    | h::tl -> mk_arg_list_ tl
  in
  mk_arg_list_ cls_list

let mk_arg_list_by_meth_name (JavaProg.Prog(cls_list,exp) as fj) meth_name =
  let rec mk_arg_list_by_meth_name_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl when mk_arg_list fj cls_name' meth_name <> [] ->
       mk_arg_list fj cls_name' meth_name
    | h::tl -> mk_arg_list_by_meth_name_ tl
  in
  mk_arg_list_by_meth_name_ cls_list

let get_return_type (JavaProg.Prog(cls_list,exp)) cls_name meth_name =
  let rec get_return_type__ meth_list =
    match meth_list with
    | [] -> None
    | (Idnt.V return_type,Idnt.V meth_name',args,exp) :: tl when meth_name' = meth_name -> Some return_type
    | h::tl -> get_return_type__ tl
  in
  let rec get_return_type_ cls_list =
    match cls_list with
    | [] -> failwith "no class found"
    | (Idnt.V cls_name',(super_cls_id,field_list,meth_list)) :: tl ->
       begin
       match get_return_type__ meth_list with
       | None -> get_return_type_ tl
       | Some a -> a
       end
    | h::tl -> get_return_type_ tl
  in
  get_return_type_ cls_list

let rec mk_object_defs fj meth_name_list cls_list =
  match meth_name_list with
  | [] -> []
  | meth_name::tl ->
     match get_meth fj meth_name with
     | ret_type_id, Idnt.V m, arg_list, exp ->
        let args = (List.map (fun (ty_id, name_id) -> Pattern.mk_var name_id, id_to_ml_type fj ty_id) arg_list)
                   @ [Pattern.mk_var (Idnt.V "this"), string_to_ml_type fj "obj";] in
        Fdef.make (m^"_Object")
                  []
                  Formula.mk_true
                  (MLExp.mk_fun_pat args (MLExp.mk_assert (Term.mk_const (Const.False))))(* (Term.var_of_string "assert false") *)
        :: mk_object_defs fj tl cls_list
     | _ -> failwith "match error in meth_to_ml"

let rec mk_adt_def fj =
  let get_type_str = function
    | Idnt.V type_str, Idnt.V var_str -> type_str
    | _ -> failwith "match error in get_type_str" in
  let rec meth_name_to_adt_def_str cls_name_list meth_name =
    match cls_name_list with
    | [] -> failwith "no definition found"
    | h::tl ->
       if cls_has_meth fj h meth_name then
         List.map get_type_str (mk_arg_list fj h meth_name) @ ["obj"; get_return_type fj h meth_name]
         |> List.map (string_to_ml_type fj)
         |> Type.mk_fun
       else
         meth_name_to_adt_def_str tl meth_name in
  let cls_name_list = mk_cls_name_list fj in
  let meth_name_list = mk_meth_name_list fj in
  [(Idnt.make "obj",
    [Idnt.make "Obj",
     (List.fold_right (fun x l -> (string_to_ml_type fj "cls") :: x :: l)
                      (List.map (meth_name_to_adt_def_str cls_name_list) meth_name_list)
                      []) @ [string_to_ml_type fj "obj"]
     |> Type.mk_fun]); (* ex. [(obj->bool); (obj->int)] => (obj->bool) * (obj->int) *)
   (Idnt.make "cls",
    (* for all combination of cls_name and meth_name *)
    List.map (fun cls_name ->
              List.map (fun meth_name ->
                        Idnt.make (String.capitalize meth_name^"_"^cls_name),
                        List.map get_type_str
                                 ((mk_field_list fj cls_name) (* @ (mk_arg_list_by_meth_name fj meth_name) *))
                        |> (fun l -> l @ ["cls"])
                        |> List.map (string_to_ml_type fj) |> Type.mk_fun)
                       meth_name_list)
             cls_name_list
    |> List.flatten)
  ]

let pr_adt_def dtyps =
  if dtyps <> [] then
    Format.printf
      "type @[%a@]@,"
      (List.pr
         (fun ppf (id, env) ->
          Format.fprintf ppf "%a = @[%a@]@," Idnt.pr id TypEnv.pr env)
         ".@,")
      dtyps

let rec exp_to_ml_type fj exp =
  match exp with
  | JavaExp.Var(_,x)      -> Type.mk_var x
  | JavaExp.Eq(_,e1,e2)   -> Type.mk_bool
  | JavaExp.Add(_,e1,e2)  -> Type.mk_int
  | JavaExp.Sub(_,e1,e2)  -> Type.mk_int
  | JavaExp.Mul(_,e1,e2)  -> Type.mk_int
  | JavaExp.Not(_,e)      -> Type.mk_bool
  | JavaExp.Int(_,n)      -> Type.mk_int
  | JavaExp.Int_(_)       -> Type.mk_int
  | JavaExp.Bool(_,true)  -> Type.mk_bool
  | JavaExp.Bool(_,false) -> Type.mk_bool
  | JavaExp.Field(_,e,f)  -> Type.mk_var f
  | JavaExp.Method(_,e,Idnt.V m,es) -> Type.mk_fun (List.map (exp_to_ml_type fj) es @ [exp_to_ml_type fj e])
  | JavaExp.New(_,Idnt.V s,es) -> string_to_ml_type fj "obj"
  | JavaExp.Assert(_,JavaExp.Bool(_,_)) -> Type.mk_unit
  | JavaExp.Assert(_, e)       -> Type.mk_unit
  | JavaExp.IF(_,e1,e2,e3)     -> exp_to_ml_type fj e2
  | JavaExp.IF_(_,e1,e2)       -> exp_to_ml_type fj e1
  | JavaExp.Cast(_,Idnt.V s,e) -> exp_to_ml_type fj e
  | JavaExp.Fail(_)            -> Type.mk_unit
  | _ -> failwith "Unexpected Java exppression in exp_to_ml_type"

let rec exp_to_ml fj exp =
  let meth_name_list = mk_meth_name_list fj in
  match exp with
  | JavaExp.Var(_,x)      -> Term.mk_var x
  | JavaExp.Eq(_,e1,e2)   -> Term.mk_app (Term.mk_const (Const.Eq Type.mk_int)) [exp_to_ml fj e1; exp_to_ml fj e2]
  | JavaExp.Add(_,e1,e2)  -> Term.mk_app (Term.mk_const (Const.Add Type.mk_int)) [exp_to_ml fj e1; exp_to_ml fj e2]
  | JavaExp.Sub(_,e1,e2)  -> Term.mk_app (Term.mk_const (Const.Sub Type.mk_int)) [exp_to_ml fj e1; exp_to_ml fj e2]
  | JavaExp.Mul(_,e1,e2)  -> Term.mk_app (Term.mk_const (Const.Mul Type.mk_int)) [exp_to_ml fj e1; exp_to_ml fj e2]
  | JavaExp.Not(_,e)      -> Term.mk_app (Term.mk_const (Const.Not)) [exp_to_ml fj e]
  | JavaExp.Int(_,n)      -> Term.mk_const (Const.Int n)
  | JavaExp.Int_(_)       -> MLExp.mk_rand_int
  | JavaExp.Bool(_,true)  -> Term.mk_const (Const.True)
  | JavaExp.Bool(_,false) -> Term.mk_const (Const.False)
  | JavaExp.Field(_,e,f)  -> Term.mk_var f
  | JavaExp.Method(_,e,Idnt.V m,es) ->
     MLExp.mk_app (Term.var_of_string ("send_"^m))
                  (List.map (exp_to_ml fj) es @ [exp_to_ml fj e])
  | JavaExp.New(_,Idnt.V s,es) ->
     let rec mk_es_var n =
       if n = 0 then [] else Term.var_of_string ("x"^string_of_int n) :: mk_es_var (n-1) |> List.rev in
     let es_var = mk_es_var (List.length es) in
     let fun_list = List.map (fun m -> Term.mk_app (Term.var_of_string (m^"_"^s)) es_var) meth_name_list in
     let cls_list =
       let cls_type = Type.mk_fun (List.make ((List.length es) + 1) Type.mk_unknown) in
       List.map (fun m -> ADTTerm.mk_kon (Idnt.V (String.capitalize m^"_"^s), cls_type) es_var)
                meth_name_list in
     let obj =
       let obj_type = List.assoc (Idnt.make "obj") (mk_adt_def fj) |> List.map snd |> List.hd in
       ADTTerm.mk_kon (Idnt.V "Obj", obj_type)
                      (List.fold_right2 (fun x1 x2 l -> x1::x2::l) cls_list fun_list []) in
     let rec mk_obj es es_var =
       match es, es_var with
       | [], _ -> obj
       | h::tl, (Term.Var h')::tl' ->
         MLExp.mk_let
           (exp_to_ml_type fj h)
           (h',Type.mk_unknown)
           (exp_to_ml fj h)
           (mk_obj tl tl')
       | _ -> failwith "match error"
     in
     mk_obj es es_var
  | JavaExp.Assert(_,JavaExp.Bool(_,_)) ->
    MLExp.mk_assert (Term.mk_const (Const.False))
  | JavaExp.Assert(_, e) ->
    MLExp.mk_if Type.mk_unit
      (exp_to_ml fj e)
      UnitTerm.make
      (MLExp.mk_assert (Term.mk_const (Const.False)))
  | JavaExp.IF(_, e1, e2, e3) ->
    MLExp.mk_if Type.mk_unknown
      (exp_to_ml fj e1)
      (exp_to_ml fj e2)
      (exp_to_ml fj e3)
  | JavaExp.IF_(_,e1,e2) ->
    MLExp.mk_if Type.mk_unknown
      (MLExp.mk_rand_bool)
      (exp_to_ml fj e1)
      (exp_to_ml fj e2)
  | JavaExp.Cast(_,Idnt.V s,e) ->
    Term.mk_app (Term.var_of_string ("send_castTo_"^s)) [exp_to_ml fj e]
  | JavaExp.Fail(_) ->
    MLExp.mk_assert (Term.mk_const Const.False)
  | _ -> failwith "Unexpected Java expression"
and exp_option_to_ml fj = function
  | None   -> failwith "none"
  | Some e -> exp_to_ml fj e


let rec meth_list_to_ml fj cls_name super_cls_name field_list meth_name_list =
  let meth_to_ml meth =
    match meth with
    | ret_type_id, Idnt.V m, arg_list, exp ->
       let f    = List.map (fun (type_id,name_id) -> Pattern.mk_var name_id, id_to_ml_type fj type_id) field_list in
       let x    = List.map (fun (type_id,name_id) -> Pattern.mk_var name_id, id_to_ml_type fj type_id) arg_list in
       let this = (Pattern.mk_var (Idnt.V "this"), string_to_ml_type fj "obj") in
       let term = MLExp.mk_fun_pat (f @ x @ [this]) (exp_option_to_ml fj exp) in
       Fdef.make (m^"_"^cls_name)
                 []
                 Formula.mk_true
                 term
    | _ -> failwith "match error in meth_to_ml"
  in
  let meth_to_ml_for_parent meth_name child_cls_name =
    let (_, _, arg_list, _) = get_meth fj meth_name in
    let args_for_let =
      let f    = List.map (fun (type_id,name_id) -> Pattern.mk_var name_id, id_to_ml_type fj type_id) (mk_field_list fj child_cls_name) in
      let x    = List.map (fun (type_id,name_id) -> Pattern.mk_var name_id, id_to_ml_type fj type_id) arg_list in
      let this = (Pattern.mk_var (Idnt.make "this"), string_to_ml_type fj "obj") in
      f @ x @ [this] in
    let args_for_app =
      let x    = List.map (fun (type_id, name_id) -> Term.mk_var name_id) arg_list in
      let this = Term.var_of_string "this" in
      x @ [this]
    in
    let term = MLExp.mk_fun_pat args_for_let (MLExp.mk_app (Term.var_of_string (meth_name^"_"^super_cls_name)) args_for_app) in
    Fdef.make (meth_name^"_"^cls_name)
              []
              Formula.mk_true
              term
  in
  let my_meth = mk_meth_name_list_by_cls_name fj cls_name in
  match meth_name_list with
  | [] -> []
  | h :: tl ->
     if (List.find_all (fun l -> l = h) my_meth) == [] (* not in my meth *) then
       meth_to_ml_for_parent h cls_name :: (meth_list_to_ml fj cls_name super_cls_name field_list tl)
     else
       meth_to_ml (get_meth_by_cls_name_and_meth_name fj cls_name h) :: meth_list_to_ml fj cls_name super_cls_name field_list tl
and cls_list_to_ml fj meth_name_list cls_list =
  match cls_list with
  | [] -> []
  | (Idnt.V cls_name, (Idnt.V super_cls_name,field_list,meth_list)) :: tl ->
     (meth_list_to_ml fj cls_name super_cls_name field_list meth_name_list) :: cls_list_to_ml fj meth_name_list tl
  | _ -> failwith "match error in cls_to_ml"

let insert_cast_function (JavaProg.Prog(cls_list,exp)) =
  let rec insert_cast_function_ cls_list =
    match cls_list with
    | [] -> []
    | (Idnt.V cls_str,(super_cls_id,field_list,meth_list)) :: tl ->
       let exp = Some (JavaExp.Var ([], Idnt.make "this")) in
       let cast_meth = (Idnt.V cls_str,Idnt.V ("castTo_"^cls_str),[],exp) in
       (Idnt.V cls_str,(super_cls_id,field_list,(cast_meth :: meth_list))) :: (insert_cast_function_ tl)
    | _ -> failwith "pattern match error in insert_cast_function"
  in
  JavaProg.Prog(insert_cast_function_ cls_list,exp)

let mk_call_function_list fj meth_name_list =
  let rec mk_call_function_list_ meth_name_list_tl =
    match meth_name_list_tl with
    | [] -> []
    | h::tl ->
       let pattern_match_for_obj =
         Pattern.K(
             Idnt.V "Obj",
             Pattern.T (List.fold_right (fun h' l -> Pattern.mk_var (Idnt.V (if h = h' then "cm" else "c_"^h'))
                                                     :: Pattern.mk_var (Idnt.V (if h = h' then  "m" else  "_"^h'))
                                                     :: l)
                                        meth_name_list []))
       in
       let args_var_for_app =
         mk_arg_list_by_meth_name fj h
         |> List.map (fun (arg_type,arg_name) -> Term.mk_var arg_name)
       in
       let this =
         ADTTerm.mk_kon
           (Idnt.V "Obj", List.assoc (Idnt.make "obj") (mk_adt_def fj) |> List.map snd |> List.hd )
           (List.fold_right (fun h' l -> Term.var_of_string (if h = h' then "cm" else "c_"^h')
                                         :: Term.var_of_string (if h = h' then  "m" else  "_"^h')
                                         :: l)
                            meth_name_list [])
       in
       let patterns =
         (mk_arg_list_by_meth_name fj h
          |> List.map (fun (arg_type,arg_name) -> Pattern.mk_var arg_name, id_to_ml_type fj arg_type))
         @ [pattern_match_for_obj, string_to_ml_type fj "obj"]
       in
       let term = MLExp.mk_fun_pat
                    patterns
                    (MLExp.mk_app (Term.var_of_string "m") (args_var_for_app @ [this])) in
       (Fdef.make ("send_"^h) [] Formula.mk_true term) :: mk_call_function_list_ tl
  in
  mk_call_function_list_ meth_name_list

let rec meth_list_to_ml_type fj cls_name super_cls_name field_list meth_name_list =
  let meth_to_ml_type meth =
    match meth with
    | Idnt.V ret_type_str, Idnt.V m, arg_list, exp ->
       let f    = List.map (fun (type_id,name_id) -> id_to_ml_type fj type_id) field_list in
       let x    = List.map (fun (type_id,name_id) -> id_to_ml_type fj type_id) arg_list in
       let this = string_to_ml_type fj "this" in
       ((m^"_"^cls_name), (Type.mk_fun (f @ x @ [this] @ [string_to_ml_type fj ret_type_str])))
    | _ -> failwith "match error in meth_to_ml"
  in
  let meth_to_ml_for_parent_type meth_name child_cls_name =
    let (ret_type_id, _, arg_list, _) = get_meth fj meth_name in
    let args_for_let =
      let f    = List.map (fun (type_id,name_id) -> id_to_ml_type fj type_id) (mk_field_list fj child_cls_name) in
      let x    = List.map (fun (type_id,name_id) -> id_to_ml_type fj type_id) arg_list in
      let this = string_to_ml_type fj "this" in
      f @ x @ [this] @ [id_to_ml_type fj ret_type_id] in
    ((meth_name^"_"^cls_name), Type.mk_fun args_for_let)
  in
  let my_meth = mk_meth_name_list_by_cls_name fj cls_name in
  match meth_name_list with
  | [] -> []
  | h :: tl ->
     if (List.find_all (fun l -> l = h) my_meth) == [] (* not in my meth *) then
       meth_to_ml_for_parent_type h cls_name :: (meth_list_to_ml_type fj cls_name super_cls_name field_list tl)
     else
       meth_to_ml_type (get_meth_by_cls_name_and_meth_name fj cls_name h) :: meth_list_to_ml_type fj cls_name super_cls_name field_list tl
and cls_list_to_ml_type fj meth_name_list cls_list =
  match cls_list with
  | [] -> []
  | (Idnt.V cls_name, (Idnt.V super_cls_name,field_list,meth_list)) :: tl ->
     (meth_list_to_ml_type fj cls_name super_cls_name field_list meth_name_list) :: cls_list_to_ml_type fj meth_name_list tl
  | _ -> failwith "match error in cls_to_ml"


let rec mk_object_defs_type fj meth_name_list cls_list =
  match meth_name_list with
  | [] -> []
  | meth_name::tl ->
     match get_meth fj meth_name with
     | Idnt.V ret_type_str, Idnt.V m, arg_list, exp ->
        let args = (List.map (fun (ty_id, name_id) -> id_to_ml_type fj ty_id) arg_list)
                   @ [string_to_ml_type fj "this";] @ [string_to_ml_type fj ret_type_str]  in
        ((m^"_Object"), Type.mk_fun args) :: mk_object_defs_type fj tl cls_list
     | _ -> failwith "match error in meth_to_ml"

let mk_call_function_list_type fj meth_name_list =
  let rec mk_call_function_list_type_ meth_name_list_tl =
    match meth_name_list_tl with
    | [] -> []
    | h::tl ->
       let arg_types =
         mk_arg_list_by_meth_name fj h
         |> List.map (fun (arg_id,arg_name) -> id_to_ml_type fj arg_id)
       in
       let ret_type = (get_return_type fj (List.hd (mk_cls_name_list fj)) h) in
       ("send_"^h, Type.mk_fun (arg_types @ [string_to_ml_type fj "this"] @ [string_to_ml_type fj ret_type])) :: mk_call_function_list_type_ tl
  in
  mk_call_function_list_type_ meth_name_list

let fdefs2progs (fdefs:Fdef.t list) fj meth_name_list cls_list args' =
  let meth_type_env =
    (cls_list_to_ml_type fj meth_name_list cls_list |> List.concat)
    @ mk_call_function_list_type fj meth_name_list
    @ mk_object_defs_type fj meth_name_list cls_list
    @ [("main", Type.mk_fun (List.map snd args' @ [Type.mk_unit]))]
  in
  List.map (fun f -> MLExp.mk_let Type.mk_unknown
                                  (Idnt.make f.Fdef.name, Type.mk_unknown)
                                  f.Fdef.body
                                  (Term.mk_const Const.Undef), (* for top level function definition *)
                     [(Idnt.make f.Fdef.name, (List.assoc f.Fdef.name meth_type_env))]) fdefs

(* let fj2ml (JavaProg.Prog(cls_list,exp) as fj) = *)
(*   (\*  let (JavaProg.Prog(cls_list,exp) as fj) = insert_cast_function fj in*\) *)
(*   let meth_name_list = mk_meth_name_list fj in *)
(*   let fdef_object_class_list  = mk_object_defs fj meth_name_list cls_list in *)
(*   let fdef_call_function_list = mk_call_function_list fj meth_name_list in *)
(*   let fdef_method_list        = cls_list_to_ml fj meth_name_list cls_list |> List.flatten in *)
(*   let fdef_main               = Fdef.make "main" [Pattern.U] Formula.mk_true (exp_to_ml fj exp) in *)
(*   let fdefs = fdef_object_class_list @ fdef_call_function_list @ fdef_method_list @ [fdef_main] in *)
(*   pr_adt_def (mk_adt_def fj); *)
(*   Prog.({fdefs; types = []; main = "main"}) *)
(*   |> Prog.pr (Format.std_formatter) *)

let fj2ml (JavaProg.Prog(cls_list,JavaMain.Main(_,args,exp)) as fj) =
  (*  let (JavaProg.Prog(cls_list,exp) as fj) = insert_cast_function fj in*)
  let args' = if args = [] then [Pattern.U,Type.mk_unit] else List.map (fun (ty,id) -> Pattern.mk_var id, id_to_ml_type fj ty) args in
  let meth_name_list = mk_meth_name_list fj in
  let fdef_object_class_list  = mk_object_defs fj meth_name_list cls_list in
  let fdef_call_function_list = mk_call_function_list fj meth_name_list in
  let fdef_method_list        = cls_list_to_ml fj meth_name_list cls_list |> List.flatten in
  let fdef_main               = let term = MLExp.mk_fun_pat args' (exp_to_ml fj exp) in Fdef.make "main" [] Formula.mk_true term  in
  let fdefs = fdef_object_class_list @ fdef_call_function_list @ fdef_method_list @ [fdef_main] in
   ([],[]), (fdefs2progs fdefs fj meth_name_list cls_list args', mk_adt_def fj)
