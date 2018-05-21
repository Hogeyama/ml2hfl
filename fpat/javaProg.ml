open Util
open Combinator

type 'a t = Prog of 'a JavaClass.t list * 'a JavaMain.t

let embed (Prog(cls', JavaMain.Main(_,_,exp))) =
  let cls =
    List.map
      (fun (id, _) ->
       let rec f id_this =
         if Idnt.string_of id_this = "Object" then
           [], []
         else
           let id_super, fields_this, methods_this =
             List.assoc id_this cls'
           in
           let mtds_super, flds_super = f id_super in
           let mtds_this =
             List.map (fun (_, id, _, _) -> id, id_this) methods_this
           in
           let flds_this = List.map snd fields_this in
           List.map
             (List.map
                (fun (id, id') ->
                 try id, List.assoc id mtds_this
                 with Not_found -> id, id'))
             mtds_super
           @ [List.filter
                (fun (id, _) ->
                 not (List.mem_assoc id (List.concat mtds_super)))
                mtds_this],
           flds_super @ [flds_this]
       in
       let mtds, flds = f id in
       id, (mtds, flds))
      cls'
  in
  let fdefs, paths =
    cls'
    |> List.map (JavaClass.embed cls)
    |> List.split
    |> Pair.map List.concat List.concat
  in
  let paths =
    paths
    |> List.cons [2,2]
    |> List.cons [2,1]
    |> List.unique
    |> List.concat_map List.prefixes
    |> List.unique
    |> flip List.remove  []
  in
  let id_fdef =
    let x = Idnt.new_var () in
    (MLExp.name_of_id, Type.mk_top), [x, Type.mk_top], MLExp.mk_var x
  in
  let fdefs =
    fdefs
    @ List.map MLExp.mk_church_proj_def paths
    @ [id_fdef]
  in
  MLExp.mk_letrec fdefs (JavaClass.embed_exp cls exp)


let p (Prog(cls_list,JavaMain.Main(_,_,exp))) =
  let rec exp_to_s exp =
    match exp with
    | JavaExp.Var(_, Idnt.V s)        -> Printf.sprintf "JavaExp.Var([],%s)" s
    | JavaExp.Field(_,e,Idnt.V s)     -> Printf.sprintf "JavaExp.Field([],%s,%s)" (exp_to_s e) s
    | JavaExp.Method(_,e,Idnt.V s,es) -> Printf.sprintf "JavaExp.Method([],%s,%s,(%s))" (exp_to_s e) s (exps_to_s es)
    | JavaExp.New(_,Idnt.V s,es)      -> Printf.sprintf "JavaExp.New([],%s,(%s))" s (exps_to_s es)
    | _ -> ""
  and exps_to_s exps =
    List.map exp_to_s exps
    |> String.concat ","
  and exp_option_to_s exp =
    match exp with
    | None   -> ""
    | Some e -> exp_to_s e
  in
  let rec meths_to_s meths =
    let meth_to_s meth =
      match meth with
      | (Idnt.V s1,Idnt.V s2,args,exp) -> Printf.sprintf "(%s,%s,%s,%s)" s1 s2 (args_to_s args) (exp_option_to_s exp)
      | _ -> failwith "unexpected datatype"
    in
    List.map meth_to_s meths
    |> String.concat ","
  and args_to_s args =
    let arg_to_s arg =
      match arg with
      | (Idnt.V s1,Idnt.V s2) -> Printf.sprintf "(%s,%s)" s1 s2
      | _ -> failwith "unexpected datatype"
    in
    List.map arg_to_s args
    |> String.concat ","
  and cls_list_to_s cls_list =
    let cls_to_s cls =
      match cls with
      | (Idnt.V cls_name, (Idnt.V super_cls_name,args,meths)) ->
         Printf.sprintf "(%s,(%s,%s,%s))" cls_name super_cls_name (args_to_s args) (meths_to_s meths)
      | _ -> ""
    in
    List.map cls_to_s cls_list
    |> String.concat ",\n\t "
  in
  Printf.sprintf "JavaProg.Prog(\n\t{%s},\n\t%s\n)" (cls_list_to_s cls_list) (exp_to_s exp)
  |> Printf.printf "%s"


let pr (Prog(cls_list,JavaMain.Main(_,_,exp))) =
  let rec exp_to_s ppf exp =
    match exp with
    | JavaExp.Var(_, Idnt.V s)        -> Format.fprintf ppf "@[<4>%s@]" s
    | JavaExp.Int(_, n)               -> Format.fprintf ppf "@[<4>%d@]" n
    | JavaExp.Field(_,e,Idnt.V s)     -> Format.fprintf ppf "@[<4>%a.%s@]" exp_to_s e s
    | JavaExp.Method(_,e,Idnt.V s,es) -> Format.fprintf ppf "@[<4>%a.%s(%a)@]" exp_to_s e  s  exp_list_to_s es
    | JavaExp.New(_,Idnt.V s,es)      -> Format.fprintf ppf "@[<4>new %s(%a)@]" s  exp_list_to_s es
    | _ -> failwith "unexpected expression"
  and exp_option_to_s ppf exp =
    match exp with
    | None   -> ()
    | Some e -> exp_to_s ppf e
  and exp_list_to_s ppf exp_list =
    Format.fprintf ppf "%a" (List.pr exp_to_s ",") exp_list
  in
  let rec meth_list_to_s ppf meth_list =
    let meth_to_s ppf meth =
      match meth with
      | (Idnt.V s1,Idnt.V s2,args,exp) -> Format.fprintf ppf "@[<4>@,%s %s(%a){@\nreturn %a@];@\n}"
                                                         s1
                                                         s2
                                                         arg_list_to_s args
                                                         exp_option_to_s exp
      | _ -> Format.fprintf ppf ""
    in
    match meth_list with
    | [] -> Format.fprintf ppf ""
    | _ -> Format.fprintf ppf "@\n@[<4>%a@]" (List.pr meth_to_s "@,") meth_list
  and arg_list_to_s ppf arg_list =
    let arg_to_s ppf arg =
      match arg with
      | (Idnt.V s1,Idnt.V s2) -> Format.fprintf ppf "%s %s" s1 s2
      | _ -> Format.fprintf ppf ""
    in
    Format.fprintf ppf "%a" (List.pr arg_to_s ", ") arg_list
  and arg_list_to_s_for_instance_variable ppf arg_list =
    let arg_to_s ppf arg =
      match arg with
      | (Idnt.V s1,Idnt.V s2) -> Format.fprintf ppf "%s %s;@\n" s1 s2
      | _ -> Format.fprintf ppf ""
    in
    Format.fprintf ppf "%a" (List.pr arg_to_s "") arg_list
  and cls_list_to_s ppf cls_list =
    let cls_to_s ppf cls =
      match cls with
      | (Idnt.V cls_name, (Idnt.V super_cls_name,arg_list,meth_list)) ->
         let subst_instance_var_str =
           List.map (fun args ->
                     match args with
                     | _, Idnt.V s -> Printf.sprintf "this.%s=%s" s s
                     | _ -> "" )
                    arg_list
           |> String.concat "; "
         in
         Format.fprintf ppf "@[<4>class %s extends %s {@\n%a%s(%a) { super(); %s } %a@]@\n}"
                        cls_name
                        super_cls_name
                        arg_list_to_s_for_instance_variable arg_list
                        cls_name
                        arg_list_to_s arg_list
                        subst_instance_var_str
                        meth_list_to_s meth_list
      | _ -> Format.printf ""
    in
    Format.fprintf ppf "@[<4>%a@]" (List.pr cls_to_s "@\n@\n") cls_list
  in
  Format.fprintf (Format.std_formatter) "%a@\n@\n%a" cls_list_to_s cls_list  exp_to_s exp
