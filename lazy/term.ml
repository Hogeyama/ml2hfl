open ExtList
open ExtString

type t =
  Var of Attr.t * Id.t
| Const of Attr.t * Const.t
| App of Attr.t * t * t
| Call of Attr.t * t * t list
| Ret of Attr.t * t * t
| Error of Attr.t

let rec fun_args t =
  match t with
    App(_, t1, t2) ->
      let f, args = fun_args t1 in
      f, args @ [t2]
  | _ ->
      t, []

let rec pr ppf t =
  match t with
    Var(a, id) ->
      (try
		      let p = Attr.path a in
		      let arg = Attr.arg a in
		      Format.fprintf ppf "<%a%s%s>" Id.pr id
		        (":" ^ String.concat "" (List.map String.of_int p))
		        (":" ^ String.of_int arg)
      with Not_found ->
      (*Format.fprintf ppf "%a%d" Id.pr id (try Attr.arity a with Not_found -> 0)*)
        Format.fprintf ppf "%a" Id.pr id)
  | Const(a, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(a, t1, t2) ->
      let f, args = fun_args t in
      (match f, args with
        Var(a, id), _ ->
          Format.fprintf ppf "(%a %a)" pr f (Util.pr_list pr " ") args
      | Const(a, c), [t1; t2] when Const.is_binary c ->
          Format.fprintf ppf "(%a %a %a)" pr t1 Const.pr_bin c pr t2
      | Const(a, c), _ ->
          Format.fprintf ppf "(%a %a)" pr f (Util.pr_list pr " ") args)
  | Call(_, f, args) ->
      Format.fprintf ppf "Call(%a, %a)" pr f (Util.pr_list pr ", ") args
  | Ret(_, ret, t) ->
      Format.fprintf ppf "Ret(%a, %a)" pr ret pr t
  | Error(_) ->
      Format.fprintf ppf "Error"

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr t;
  Format.flush_str_formatter ()

let make_var id = Var([], Id.make id)
let make_int n = Const([], Const.Int(n))
let make_unit = Const([], Const.Unit)
let make_true = Const([], Const.True)
let make_false = Const([], Const.False)
let make_event id = Const([], Const.Event(Id.make id))

let rec apply t ts =
  match ts with
    [] ->
      t
  | t'::ts' ->
      apply (App([], t, t')) ts'

let rec logand ts =
  match ts with
    [] -> Const([], Const.True)
  | [t] -> t
  | t::ts' -> apply (Const([], Const.And)) [t; logand ts']

let add t1 t2 = apply (Const([], Const.Add)) [t1; t2]
(*let sub t1 t2 = apply (Const([], Const.Add)) [t1; apply (Const([], Const.Minus)) [t2]]*)
let sub t1 t2 = apply (Const([], Const.Sub)) [t1; t2]
let mul t1 t2 = apply (Const([], Const.Mul)) [t1; t2]
let eq t1 t2 = apply (Const([], Const.Eq)) [t1; t2]
(*let neq t1 t2 = apply (Const([], Const.Not)) [apply (Const([], Const.Eq)) [t1; t2]]*)
let neq t1 t2 = apply (Const([], Const.Neq)) [t1; t2]
let lt t1 t2 = apply (Const([], Const.Lt)) [t1; t2]
let gt t1 t2 = apply (Const([], Const.Gt)) [t1; t2]
let leq t1 t2 = apply (Const([], Const.Leq)) [t1; t2]
let geq t1 t2 = apply (Const([], Const.Geq)) [t1; t2]
(*let gt t1 t2 = apply (Const([], Const.Lt)) [t2; t1]*)
(*let leq t1 t2 = apply (Const([], Const.Or)) [lt t1 t2; eq t1 t2]*)
(*let geq t1 t2 = apply (Const([], Const.Or)) [gt t1 t2; eq t1 t2]*)

let rec subst sub t =
  match t with
    Var(a, x) -> (try sub x with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, subst sub t1, subst sub t2)
  | _ -> assert false

let rec redex_of t =
		match t with
    Const(a, Const.Event(id)) when Id.get id = "fail" ->
      (fun t -> t), Const(a, Const.Event(id))
		| App(_, _, _) ->
				  let f, args = fun_args t in
      let rec r args1 args =
        match args with
          [] -> raise Not_found
        | arg::args2 ->
            (try
              args1, redex_of arg, args2
            with Not_found ->
              r (args1 @ [arg]) args2)
      in
				  (match f with
				    Var(attr, f) ->
          (try
            let args1, (ctx, red), args2 = r [] args in
            (fun t -> apply (Var(attr, f)) (args1 @ [ctx t] @ args2)), red
          with Not_found ->
		          let ar = try Attr.arity attr with Not_found -> (Format.printf "%a@." Id.pr f; assert false) in
		          if List.length args >= ar then
		  				      let args1, args2 = Util.split_at args ar in
		            (fun t -> apply t args2), apply (Var(attr, f)) args1
		          else raise Not_found)
      | Const(attr, c) ->
          (try
            let args1, (ctx, red), args2 = r [] args in
            (fun t -> apply (Const(attr, c)) (args1 @ [ctx t] @ args2)), red
          with Not_found ->
		          raise Not_found))
  | Call(a, f, args) ->
      (fun t -> t), Call(a, f, args)
  | Ret(a, ret, t) ->
      (try
		      let ctx, red = redex_of t in
		      (fun t -> Ret(a, ret, ctx t)), red
      with Not_found ->
        (fun t -> t), Ret(a, ret, t))
  | _ -> raise Not_found

let rec set_arity am t =
  match t with
    Var(a, x) -> (try let ar = am x in Var(Attr.Arity(ar)::a, x) with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, set_arity am t1, set_arity am t2)
		| Call(a, f, args) -> Call(a, set_arity am f, List.map (set_arity am) args)
		| Ret(a, ret, t) -> Ret(a, set_arity am ret, set_arity am t)
		| Error(a) -> Error(a)
