open ExtList
open ExtString

(** Term expressions *)

(** {6 The type of terms} *)

type t =
  Var of Attr.t * Var.t
| Const of Attr.t * Const.t
| App of Attr.t * t * t

| Call of Attr.t * t * t list
| Ret of Attr.t * t * t * SimType.t
| Error of Attr.t

| Forall of Attr.t * (Var.t * SimType.t) list * t
| Exists of Attr.t * (Var.t * SimType.t) list * t

(** {6 Constructors} *)

let rec apply t ts =
  match ts with
    [] ->
      t
  | t' :: ts' ->
      apply (App([], t, t')) ts'

let make_var x = Var([], x)
let new_var () = make_var (Var.new_var ())

let tunit = Const([], Const.Unit)
let tevent id = Const([], Const.Event(Idnt.make id))
let event_fail = "fail"
let tfail = apply (tevent event_fail) [tunit]

(** {5 Integer expressions} *)

let tint n = Const([], Const.Int(n))
let add t1 t2 = apply (Const([], Const.Add)) [t1; t2]
let rec sum ts =
  match ts with
    [] -> tint 0
  | [t] -> t
  | (Const(_, Const.Int(0))) :: ts' -> sum ts'
  | t :: ts' ->
      let t' = sum ts' in
      (match t' with
        Const(_, Const.Int(0)) -> t
      | _ -> add t t')

(*let sub t1 t2 = apply (Const([], Const.Add)) [t1; apply (Const([], Const.Minus)) [t2]]*)
let sub t1 t2 = apply (Const([], Const.Sub)) [t1; t2]
let minus t = apply (Const([], Const.Minus)) [t]
let mul t1 t2 = apply (Const([], Const.Mul)) [t1; t2]
let prod ts =
  let rec aux ts =
    match ts with
      [] -> tint 1
    | [t] -> t
    | (Const(_, Const.Int(1))) :: ts' -> aux ts'
    | (Const(_, Const.Int(0))) :: _ -> raise Not_found
    | t :: ts' ->
        let t' = aux ts' in
        (match t' with
          Const(_, Const.Int(1)) -> t
        | _ -> mul t t')
  in
  try aux ts with Not_found -> tint 0

(** {6 Destructors} *)

let rec fun_args t =
  match t with
    App(_, t1, t2) ->
      let f, args = fun_args t1 in
      f, args @ [t2]
  | _ ->
      t, []

let int_const_of t =
  match t with
    Const(_, Const.Int(n)) -> n
  | _ -> raise Not_found

let var_of t =
  match t with
    Var(_, x) -> x
  | _ -> raise Not_found

let is_int_const t =
  match t with
    Const(_, Const.Int(n)) -> true
  | _ -> false

(** {6 Printers} *)

let rec pr ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
      (*
      Format.fprintf ppf
        "%a%d" Idnt.pr id (try Attr.arity a with Not_found -> 0)
      *)
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf
            "@[<hov2>(%a@ @[<hov>%a@])@]" pr f (Util.pr_list pr "@ ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf "(%a %a)" pr f pr t
      | Const(_, c), [t1; t2] when Const.is_bin c ->
          Format.fprintf ppf
            "@[<hov>(%a %a@ %a)@]" pr t1 Const.pr_bin c pr t2
      | Const(_, _), _ ->
          Format.fprintf ppf
            "@[<hov2>(%a@ @[<hov>%a@])@]" pr f (Util.pr_list pr "@ ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf
        "Call(@[<hov>%a@])" (Util.pr_list pr ",@ ") (f :: args)
  | Ret(_, ret, t, _) ->
      Format.fprintf ppf
        "Ret(@[<hov>%a,@ %a@])" pr ret pr t
  | Error(_) ->
      Format.fprintf ppf "Error"
  | Forall(_, env, t) ->
      Format.fprintf ppf
        "@[<hov2>forall %a,@ %a@]" (Util.pr_list SimType.pr_bind ",") env pr t
  | Exists(_, env, t) ->
      Format.fprintf ppf
        "@[<hov2>exists %a,@ %a@]" (Util.pr_list SimType.pr_bind ",") env pr t

let pr_list ppf ts =
  Format.fprintf ppf "%a" (Util.pr_list pr ",") ts

let rec pr_oneliner ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf
            "(%a %a)" pr_oneliner f (Util.pr_list pr_oneliner " ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf
            "(%a %a)" pr_oneliner f pr_oneliner t
      | Const(_, c), [t1; t2] when Const.is_bin c ->
          Format.fprintf ppf
            "(%a %a %a)" pr_oneliner t1 Const.pr_bin c pr_oneliner t2
      | Const(_, _), _ ->
          Format.fprintf ppf
            "(%a %a)" pr_oneliner f (Util.pr_list pr_oneliner " ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf
        "Call(%a)" (Util.pr_list pr_oneliner ", ") (f :: args)
  | Ret(_, ret, t, _) ->
      Format.fprintf ppf
        "Ret(%a, %a)" pr_oneliner ret pr_oneliner t
  | Error(_) ->
      Format.fprintf ppf "Error"
  | Forall(_, _, _) | Exists(_, _, _) ->
      assert false

(** {6 Basic functions} *)

(** @todo implement equivalence up to alpha conversion and ignoring attributes *)
let equiv t1 t2 = t1 = t2

let rec fvs t =
  match t with
    Var(_, x) -> if Var.is_coeff x then [] else [x]
  | Const(_, _) -> []
  | App(_, t1, t2) -> List.unique (fvs t1 @ fvs t2)
  | Call(_, _, _) | Ret(_, _, _, _) | Error(_) -> assert false
  | Forall(_, env, t) | Exists(_, env, t) -> Util.diff (fvs t) (List.map fst env)

let rec coeffs t =
  match t with
    Var(_, x) -> if Var.is_coeff x then [x] else []
  | Const(_, _) -> []
  | App(_, t1, t2) -> List.unique (coeffs t1 @ coeffs t2)
  | Call(_, _, _) | Ret(_, _, _, _) | Error(_) -> assert false
  | Forall(_, env, t) | Exists(_, env, t) -> Util.diff (coeffs t) (List.map fst env)

let rec map_var f t =
  match t with
    Var(a, x) ->
      Var(a, f x)
  | Const(a, c) ->
      Const(a, c)
  | App(a, t1, t2) ->
      App(a, map_var f t1, map_var f t2)
  | Call(_, _, _) | Ret(_, _, _, _) | Error(_)
  | Forall(_, _, _) | Exists(_, _, _) -> assert false

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr_oneliner t;
  Format.flush_str_formatter ()

let rec redex_of env t =
  match t with
(*
    Const(a, Const.RandInt) ->
      (fun t -> t), Const(a, Const.RandInt)
*)
    App(_, _, _) ->
      let f, args = fun_args t in
      let rec r args1 args =
        match args with
          [] -> raise Not_found
        | arg :: args2 ->
            (try
              args1, redex_of env arg, args2
            with Not_found ->
              r (args1 @ [arg]) args2)
      in
      (try
        let args1, (ctx, red), args2 = r [] args in
        (fun t -> apply f (args1 @ [ctx t] @ args2)), red
      with Not_found ->
        (match f with
          Const(_, Const.Event(id)) when Idnt.string_of id = "fail" ->
            let ar = 1 in
            if List.length args >= ar then
              let args1, args2 = List.split_nth ar args in
              (fun t -> apply t args2), apply f args1
            else raise Not_found
        | Const(_, Const.RandInt) ->
            let ar = 1 in
            if List.length args >= ar then
              let args1, args2 = List.split_nth ar args in
              (fun t -> apply t args2), apply f args1
            else raise Not_found
        | Var(attr, ff) ->
            let ar =
              try
                SimType.arity (env ff)
              with Not_found ->
                raise Not_found (* ff is not a function name *)
                (*(Format.printf "%a@," Var.pr ff; assert false)*)
            in
            if List.length args >= ar then
              let args1, args2 = List.split_nth ar args in
              (fun t -> apply t args2), apply f args1
            else raise Not_found
        | Const(attr, c) ->
            raise Not_found
        | _ -> assert false))
  | Call(a, f, args) ->
      (fun t -> t), Call(a, f, args)
  | Ret(a, ret, t, ty) ->
      (try
        let ctx, red = redex_of env t in
        (fun t -> Ret(a, ret, ctx t, ty)), red
      with Not_found ->
        (fun t -> t), Ret(a, ret, t, ty))
  | _ -> raise Not_found

(** @deprecated attributes are no longer used *)
let rec set_arity am t =
  match t with
    Var(a, x) -> (try let ar = am x in Var(Attr.Arity(ar) :: a, x) with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, set_arity am t1, set_arity am t2)
  | Call(_, _, _)
  | Ret(_, _, _, _)
  | Error(_)
  | Forall(_, _, _) | Exists(_, _, _) -> assert false
