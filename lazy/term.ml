open ExtList
open ExtString

type t =
  Var of Attr.t * Var.t
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
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
      (*Format.fprintf ppf "%a%d" Idnt.pr id (try Attr.arity a with Not_found -> 0)*)
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf "(@[<hov2>%a@ @[<hov>%a@]@])" pr f (Util.pr_list pr "@ ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf "(%a %a)" pr f pr t
      | Const(_, c), [t1; t2] when Const.is_binary c ->
          Format.fprintf ppf "(@[<hov>%a %a@ %a@])" pr t1 Const.pr_bin c pr t2
      | Const(_, _), _ ->
          Format.fprintf ppf "(@[<hov2>%a@ @[<hov>%a@]@])" pr f (Util.pr_list pr "@ ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf "Call(@[<hov>%a@])" (Util.pr_list pr ",@ ") (f::args)
  | Ret(_, ret, t) ->
      Format.fprintf ppf "Ret(@[<hov>%a,@ %a@])" pr ret pr t
  | Error(_) ->
      Format.fprintf ppf "Error"

let rec pr2 ppf t =
  match t with
    Var(_, x) ->
      Format.fprintf ppf "%a" Var.pr x
  | Const(_, c) ->
      Format.fprintf ppf "%a" Const.pr c
  | App(_, _, _) ->
      let f, args = fun_args t in
      (match f, args with
        Var(_, _), _ ->
          Format.fprintf ppf "(%a %a)" pr2 f (Util.pr_list pr2 " ") args
      | Const(_, _), [t] ->
          Format.fprintf ppf "(%a %a)" pr2 f pr2 t
      | Const(_, c), [t1; t2] when Const.is_binary c ->
          Format.fprintf ppf "(%a %a %a)" pr2 t1 Const.pr_bin c pr2 t2
      | Const(_, _), _ ->
          Format.fprintf ppf "(%a %a)" pr2 f (Util.pr_list pr2 " ") args
      | _, _ ->
          assert false)
  | Call(_, f, args) ->
      Format.fprintf ppf "Call(%a)" (Util.pr_list pr2 ", ") (f::args)
  | Ret(_, ret, t) ->
      Format.fprintf ppf "Ret(%a, %a)" pr2 ret pr2 t
  | Error(_) ->
      Format.fprintf ppf "Error"

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr2 t;
  Format.flush_str_formatter ()

let make_var id = Var([], Var.V(Idnt.make id))
let make_var2 x = Var([], x)
let make_int n = Const([], Const.Int(n))
let make_unit = Const([], Const.Unit)
let make_true = Const([], Const.True)
let make_false = Const([], Const.False)
let make_event id = Const([], Const.Event(Idnt.make id))

let rec apply t ts =
  match ts with
    [] ->
      t
  | t'::ts' ->
      apply (App([], t, t')) ts'

let rec band ts =
  match ts with
    [] -> Const([], Const.True)
  | [t] -> t
  | (Const(_, Const.True))::ts' -> band ts'
  | t::ts' ->
      let t' = band ts' in
      (match t' with
        Const(_, Const.True) ->
          t
      | _ ->
          apply (Const([], Const.And)) [t; t'])
let rec bor ts =
  match ts with
    [] -> Const([], Const.False)
  | [t] -> t
  | (Const(_, Const.False))::ts' -> bor ts'
  | t::ts' ->
      let t' = bor ts' in
      (match t' with
        Const(_, Const.False) ->
          t
      | _ ->
          apply (Const([], Const.Or)) [t; t'])
let bnot t = apply (Const([], Const.Not)) [t]

let imply t1 t2 = apply (Const([], Const.Imply)) [t1; t2]

let add t1 t2 = apply (Const([], Const.Add)) [t1; t2]
let rec sum ts =
  match ts with
    [] -> make_int 0
  | [t] -> t
  | (Const(_, Const.Int(0)))::ts' -> sum ts'
  | t::ts' ->
      let t' = sum ts' in
      (match t' with
        Const(_, Const.Int(0)) ->
          t
      | _ ->
          apply (Const([], Const.Add)) [t; t'])

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

let rec fvs t =
  match t with
    Var(_, x) -> [x]
  | Const(_, _) -> []
  | App(_, t1, t2) -> List.unique (fvs t1 @ fvs t2)
  | _ -> assert false

let rec redex_of env t =
		match t with
    Const(a, Const.Event(id)) when id = "fail" ->
      (fun t -> t), Const(a, Const.Event(id))
  | Const(a, Const.RandInt) ->
      (fun t -> t), Const(a, Const.RandInt)
		| App(_, _, _) ->
				  let f, args = fun_args t in
      let rec r args1 args =
        match args with
          [] -> raise Not_found
        | arg::args2 ->
            (try
              args1, redex_of env arg, args2
            with Not_found ->
              r (args1 @ [arg]) args2)
      in
				  (match f with
				    Var(attr, f) ->
          (try
            let args1, (ctx, red), args2 = r [] args in
            (fun t -> apply (Var(attr, f)) (args1 @ [ctx t] @ args2)), red
          with Not_found ->
		          let ar =
              try
                SimType.arity (env f)
              with Not_found ->
                (Format.printf "%a@." Var.pr f; assert false)
            in
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
		      let ctx, red = redex_of env t in
		      (fun t -> Ret(a, ret, ctx t)), red
      with Not_found ->
        (fun t -> t), Ret(a, ret, t))
  | _ -> raise Not_found


let rec dnf t =
  let f, args = fun_args t in
		match f, args with
    Const(_, Const.True), [] ->
      [[]]
  | Const(_, Const.False), [] ->
      []
  | Const(_, Const.And), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnf t2)
  | Const(_, Const.Or), [t1; t2] ->
      dnf t1 @ dnf t2
  | Const(_, Const.Imply), [t1; t2] ->
      dnfn t1 @ dnf t2
  | Const(_, Const.Not), [t] -> 
      dnfn t
  | Const(_, bop), [_; _] ->
      [[t]]
  | _ -> assert false
and dnfn t =
  let f, args = fun_args t in
		match f, args with
    Const(_, Const.True), [] ->
      []
  | Const(_, Const.False), [] ->
      [[]]
  | Const(_, Const.And), [t1; t2] ->
      dnfn t1 @ dnfn t2
  | Const(_, Const.Or), [t1; t2] ->
      let tss1 = dnfn t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Imply), [t1; t2] ->
      let tss1 = dnf t1 in Util.concat_map (fun ts2 -> List.map (fun ts1 -> ts1 @ ts2) tss1) (dnfn t2)
  | Const(_, Const.Not), [t] -> 
      dnf t
  | Const(a, bop), [t1; t2] ->
      [[apply (Const(a, Const.bnot_ibin bop)) [t1; t2]]]
  | _ -> assert false

let term_of_arith nxs n =
  let ts =
		  (if n = 0 then [] else [make_int n]) @
		  (List.filter_map (fun (n, x) -> if n = 0 then None else Some(mul (make_int n) (make_var2 x))) nxs)
  in
  sum ts

let rec arith_of t =
  match fun_args t with
    Var(_, x), [] ->
      [1, x], 0
  | Const(_, Const.Int(n)), [] ->
      [], n
  | Const(_, Const.Add), [t1; t2] ->
      let nxs1, n1 = arith_of t1 in
      let nxs2, n2 = arith_of t2 in
      Arith.canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Sub), [t1; t2] ->
      let nxs1, n1 = arith_of t1 in
      let nxs2, n2 = arith_of t2 in
      let nxs2, n2 =  Arith.minus nxs2, -n2 in
      Arith.canonize (nxs1 @ nxs2), n1 + n2
  | Const(_, Const.Mul), [Const(_, Const.Int(m)); t]
  | Const(_, Const.Mul), [t; Const(_, Const.Int(m))] ->
      let nxs, n = arith_of t in
      Arith.mul m nxs, m * n
  | Const(_, Const.Minus), [t] ->
      let nxs, n = arith_of t in
      Arith.minus nxs, -n
  | Const(_, Const.Unit), [] ->
      [], 0 (*????*)
  | _ ->
      invalid_arg "Term.arith_of"

let int_rel_of t =
		match fun_args t with
    Const(_, c), [t1; t2] when Const.is_int_rel c ->
      let nxs, n = arith_of (sub t1 t2) in
      c, nxs, n
  | _ -> invalid_arg "Term.int_rel_of"

let rec simplify t =
  try
		  (match fun_args t with
		    Const(_, Const.And), [t1; t2] ->
						  (match int_rel_of t1, int_rel_of t2 with
          (Const.Leq, nxs1, n1), (Const.Leq, nxs2, n2)
        | (Const.Geq, nxs1, n1), (Const.Geq, nxs2, n2) ->
            if Arith.equiv nxs1 (Arith.minus nxs2) && n1 = -n2 then
              eq (term_of_arith nxs1 n1) (make_int 0)
            else t
        | _ -> t)
		  | _ -> t)
  with Invalid_argument _ ->
    t


(*
let rec set_arity am t =
  match t with
    Var(a, x) -> (try let ar = am x in Var(Attr.Arity(ar)::a, x) with Not_found -> Var(a, x))
  | Const(a, c) -> Const(a, c)
  | App(a, t1, t2) -> App(a, set_arity am t1, set_arity am t2)
		| Call(a, f, args) -> Call(a, set_arity am f, List.map (set_arity am) args)
		| Ret(a, ret, t) -> Ret(a, set_arity am ret, set_arity am t)
		| Error(a) -> Error(a)
*)

(* up to attributes *)
let equiv t1 t2 = t1 = t2(*ToDo*)
