open Util
open Combinator

(** Formulas on ADTs *)

(** {6 Auxiliary constructors} *)

let mk_recognizer ty x t =
  ADTAtom.mk_recognizer ty x t |> Formula.of_atom

let eval_accessor = Formula.map_atom (ADTAtom.eval_accessor >> Formula.of_atom)

let lookup x ctenv =
  try List.assoc x ctenv with Not_found -> Type.mk_unknown

let type_of_string x denv =
  if x = "int" then Type.mk_int
  else if x = "bool" then Type.mk_bool
  else if x = "int_array" then Type.mk_array Type.mk_int
  else (* @todo: support function types *)
    try List.assoc (Idnt.make x) denv with Not_found -> Type.mk_unknown
let type_of_string =
  Logger.log_block2 "ADTFormula.type_of_string"
    ~before:(fun x denv ->
        Logger.printf2
          "input: x: %a, denv: %a@," String.pr x TypEnv.pr denv)
    ~after:(Logger.printf "output: %a" Type.pr)
    type_of_string

(** make substitutions which represent accessors without typing *)
let rec mk_accessors_wo_typing p t =
  match p with
  | Pattern.V v -> [v,t]
  | Pattern.K (c, ps) ->
    List.concat_mapi
      (fun i pat ->
         mk_accessors_wo_typing
           pat
           (Term.mk_app
              (Const.Accessor (Type.mk_unknown, c, i) |> Term.mk_const)
              [t]))
      (Pattern.list_of ps)
  | Pattern.T ps ->
    let tys = List.map (fun x -> Type.mk_unknown) ps in
    List.concat_mapi
      (fun i pat ->
         mk_accessors_wo_typing pat (TupTerm.mk_proj tys i t))
      ps
  | Pattern.W -> []
  | Pattern.C _ -> []
  | _ -> assert false
let mk_accessors_wo_typing =
  Logger.log_block2 "ADTFormula.mk_accessors_wo_typing"
    ~before:(fun p t ->
        Logger.printf2
          "input:@,  pat: %a@,  term: %a@,"
          Pattern.pr p Term.pr t)
    ~after:(Logger.printf "output:@, %a" TermSubst.pr)
    mk_accessors_wo_typing

let rec mk_recognizers_wo_typing p t =
  match p with
  | Pattern.V v -> []
  | Pattern.K (c, ps) ->
    let recognizer = mk_recognizer Type.mk_unknown c t in
    recognizer ::
    List.concat_mapi
      (fun i pat ->
         mk_recognizers_wo_typing
           pat
           (ADTTerm.mk_accessor (Type.mk_unknown) c i t))
      (Pattern.list_of ps)
  | Pattern.T ps ->
    let tys = List.map (fun x -> Type.mk_unknown) ps in
    List.concat_mapi
      (fun i pat ->
         mk_recognizers_wo_typing pat (TupTerm.mk_proj tys i t))
      ps
  | Pattern.W -> []
  | Pattern.C (Pattern.Const_int(n)) ->
    [IntFormula.eq t (IntTerm.make n)]
  | Pattern.C (Pattern.Const_bool(b)) ->
    [BoolFormula.eq t (BoolTerm.make b)]
  | _ -> assert false
let mk_recognizers_wo_typing =
  Logger.log_block2 "ADTFormula.mk_recognizers_wo_typing"
    ~before:(fun p t ->
        Logger.printf2
          "input:@,  pat: %a@,  term: %a@,"
          Pattern.pr p
          Term.pr t)
    ~after:(Logger.printf "output:@, %a" Formula.pr_list)
    mk_recognizers_wo_typing

(** perform typing for Recognizers/Accessors *)
let typing_term tenv =
  Term.map_con
    (function
      | Const.Recognizer(ty, constr_id) when Type.is_unknown ty ->
        let recog = Idnt.make @@ "_is_" ^ (Idnt.base constr_id) in
        Const.Recognizer(lookup recog tenv, constr_id)
      | Const.Accessor(ty, constr_id, idx) when Type.is_unknown ty ->
        let access = Idnt.make @@ "_get_" ^ (Idnt.base constr_id)
                                  ^ "_" ^ (string_of_int idx) in
        Const.Accessor(lookup access tenv, constr_id, idx)
      | c -> c)

let mk_accessors p t tcenv =
  let rec aux p t =
    match p with
    | Pattern.V v -> [v,t]
    | Pattern.K (c,pl) ->
      let targs, tret = lookup c tcenv |> Type.args_ret in
      List.concat_map2i
        (fun i p ty ->
           aux p
             (Term.mk_app
                (Const.Accessor (Type.mk_fun [tret; ty],c,i)
                 |> Term.mk_const)
                [t]))
        (Pattern.list_of pl)
        targs
    | _ -> assert false
  in
  aux p t

let mk_recognizers p t tcenv =
  let rec aux p t =
    match p with
    | Pattern.V v -> []
    | Pattern.K (c,pl) ->
      let targs, tret = lookup c tcenv |> Type.args_ret in
      (mk_recognizer(Type.mk_fun [tret; Type.mk_bool]) c t)
      :: List.concat_map2i
        (fun i p ty ->
           aux p (ADTTerm.mk_accessor (Type.mk_fun [tret; ty]) c i t))
        (Pattern.list_of pl)
        targs
    | _ -> assert false
  in
  aux p t

let mk_accessors_recognizers =
  List.concat_map
    (fun (x, ty) ->
       let id = Idnt.string_of x in
       match Type.args_ret ty with
       | [], ty ->
         [Idnt.make ("_is_" ^ id), Type.mk_fun_args_ret [ty] Type.mk_bool]
       | args, ret ->
         let mk_aname n = Idnt.make ("_get_" ^ id ^ "_" ^ string_of_int n) in
         let accesss =
           List.mapi
             (fun i typ -> mk_aname i, Type.mk_fun_args_ret [ret] typ)
             args
         in
         (Idnt.make ("_is_"^id), Type.mk_fun_args_ret [ret] Type.mk_bool)
         :: accesss)

(** remove useless constructors (partially)
    e.g. remove (not (l = Nil)) from ((not (l = Nil)) && (l = Cons(x, xs)))
    (@todo eliminate ((not (l = Nil)) && ((l = Cons(x, xs) && x >= 0)) case) *)
let sanitize phi =
  let is_con t =
    match Term.fun_args t with
    | Term.Const(Const.Con(_,_)), _ -> true
    | _ -> false
  in
  let con_def_var t = (* e.g. x = Nil *)
    match Term.fun_args t with
    | Term.Const(Const.Eq(_)), [t1;t2]
      when is_con t1 && Term.is_var t2 ->
      Some (Term.var_of t2)
    | Term.Const(Const.Eq(_)), [t1;t2]
      when is_con t2 && Term.is_var t1 ->
      Some (Term.var_of t1)
    | _ -> None
  in
  let has_def t =
    match con_def_var t with Some(_) -> true | None -> false
  in
  let rec aux flag t =
    match (Term.fun_args t), flag with
    | (Term.Const(Const.Not), [t]), Some(x) when con_def_var t = flag ->
      Logger.printf "removed: %a@," Term.pr t;
      Formula.mk_true
    | (Term.Const(Const.And), [t1; t2]), _ when has_def t1 ->
      (* (x = Nil) && ___ *)
      Logger.printf2
        "flag on(t1):@,  t1: %a@,  t2: %a@,"
        Term.pr t1 Term.pr t2;
      Formula.mk_and (aux flag t1) (aux (con_def_var t1) t2)
    | (Term.Const(Const.And), [t1; t2]), _ when has_def t2 ->
      (* ____ && (x = Nil) *)
      Logger.printf2
        "flag on(t2):@,  t1: %a@,  t2: %a@,"
        Term.pr t1 Term.pr t2;
      Formula.mk_and (aux (con_def_var t2) t1) (aux flag t2)
    | (t, ts), _ ->
      Term.mk_app t (List.map (aux None >> Formula.term_of) ts)
      |> Formula.of_term
  in
  phi |> Formula.term_of |> aux None
let sanitize =
  Logger.log_block1 "ADTFormula.sanitize"
    ~before:(Logger.printf "input: %a@," Formula.pr)
    ~after:(Logger.printf "output: %a" Formula.pr)
    sanitize
