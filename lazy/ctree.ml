open ExtList
open ExtString

type s =
  Call of (Var.t * int) * Term.t
| Arg of (Var.t * Term.t * SimType.t) list
| Ret of Var.t * Term.t * SimType.t
| Nop
| Error

type t = Node of (int * int list) * Term.t * (s * t) list ref

(* generate id of a function call *)
let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

let ret_args f uid arity = 
  Term.make_var2 (Var.T(f, uid, arity)),
  List.init arity (fun i -> Term.make_var2 (Var.T(f, uid, i)))

let init_ctree_of prog = 
  let uid = gen_id () in
  let ty_main = Prog.type_of prog (Var.V(prog.Prog.main)) in
  let ret, args =
    ret_args
     (Var.V(prog.Prog.main))
     uid
     (SimType.arity ty_main)
  in
  let _, retty = SimType.args_ret ty_main in
  Node((uid, []), Term.Ret([], ret, Term.Call([], Term.make_var prog.Prog.main, args), retty), ref [])


let eq_xtty (x, t, ty) =
  Term.eq_ty ty (Term.make_var2 x) t

let save_as_dot filename rt wl =
  let node_name uid t = (String.of_int uid) ^ ": " ^ Term.string_of t in
  let f s =
    match s with
      Call(_, t) -> Term.string_of t
    | Arg(xttys) -> Term.string_of (Term.band (List.map eq_xtty xttys))
    | Ret(x, t, ty) -> Term.string_of (eq_xtty (x, t, ty))
    | Nop -> ""
    | Error -> ""
  in
  let rec traverse (s, l) (Node((uid, _), t, cs)) =
    let s' = node_name uid t in
    (s, s', l)::
    (List.concat
      (List.map
        (fun (g, n) ->
          traverse
            (s', "[label = \"" ^ (f g) ^ "\"]")
            n)
        !cs))
  in
  let es = List.unique (traverse ("", "") rt) in
  let vs = List.unique (List.concat (List.map (fun (x, y, _) -> [x, ""; y, ""]) es)) in
  let es = List.filter (fun (x, _, _) -> x <> "") es in
  let vs = List.filter (fun (x, _) -> x <> "") vs in
  let vs = List.map
    (fun (x, _) ->
      if List.exists (fun (Node((uid, _), t, _)) -> x = node_name uid t) wl then
        (x, "[style = dashed]")
      else
        (x, ""))
    vs
  in
  Util.save_as_dot filename vs es

let rec paths_of (Node(_, _, cs)) =
  Util.concat_map (fun (g, n) -> let ps = paths_of n in if ps = [] then [[g]] else List.map (fun p -> g::p) ps) !cs

let error_paths_of rt = List.filter (fun p -> List.last p = Error) (paths_of rt)

let rec pr_path ppf p =
  let pr ppf s =
    match s with
      Call(_, t) ->
        Format.fprintf ppf "[@[<hov>%a.@," Term.pr t
    | Arg(xttys) ->
        Format.fprintf ppf "%a@," Term.pr (Term.band (List.map eq_xtty xttys))
    | Ret(x, t, ty) -> 
        Format.fprintf ppf "%a@]]@," Term.pr (eq_xtty (x, t, ty))
    | Nop ->
        Format.fprintf ppf "nop"
    | Error ->
        Format.fprintf ppf "error"
  in
  Format.fprintf ppf "%a" (Util.pr_list pr "") p

let event_fail = "fail"
let expand_node prog fenv (Node((uid, p), t, cs)) =
  let _ =
    if !cs <> [] then
      let _ =  Format.printf "the node is already expanded@." in
      assert false
  in
  try
    let ctx, red = Term.redex_of (Prog.type_of prog) t in
    let fenv, gns =
      match red with
        Term.App(_, Term.Const(_, Const.Event(id)), _(*???*)) when id = event_fail ->
          fenv, [Error, Node((gen_id (), p), Term.Error([]), ref [])]
      | Term.Const(_, Const.RandInt) ->
(*
      | Term.App(_, Term.Const(_, Const.RandInt), (*Term.Const(_, Const.Unit)*)_) ->
*)
          fenv, [Nop, Node((gen_id (), p), ctx (Term.make_var2 (Var.make (Idnt.new_id ()))), ref [])]
      | Term.App(_, _, _) ->
          let func, args = Term.fun_args red in
          let attr, f =
            match func with
              Term.Var(attr, f) -> attr, f
            | _ -> let _ = Format.printf "%a cannot be applied@." Term.pr red in assert false
          in
          let uid = gen_id () in
          let argtys, retty = SimType.args_ret (Prog.type_of prog f) in
          let ret, fargs = ret_args f uid (SimType.arity (Prog.type_of prog f)) in
          let reduct = Term.Ret([], ret, Term.Call([], Term.Var(attr, f), fargs), retty) in
          let faargs =
            try
              List.combine fargs args
            with _ -> begin
              Format.printf "formal args: %a@." (Util.pr_list Term.pr ", ") fargs;
              Format.printf "actual args: %a@." (Util.pr_list Term.pr ", ") args;
              assert false
            end
          in
          let faargs_fun = List.filter
            (function (Term.Var(_, x), _) -> not (Prog.is_base prog x) | _ -> assert false)
            faargs
          in
(*
          let pr ppf (t1, t2) = Format.fprintf ppf "%a: %a" Term.pr t1 Term.pr t2 in
          let _ = Format.printf "faargs_fun: %a@." (Util.pr_list pr ", ") faargs_fun in
*)
          let fenv x =
            try
              Util.find_map
                (function (Term.Var(_, y), aarg) ->
                  if Var.equiv x y then aarg else raise Not_found
                | _ -> assert false)
                faargs_fun
            with Not_found ->
              fenv x
          in
          fenv,
          [Arg
            (List.map2
              (function (Term.Var(_, farg), aarg) ->
                fun argty -> farg, aarg, argty
              | _ -> assert false)
              faargs argtys),
          Node((uid, p), ctx reduct, ref [])]
      | Term.Call(_, Term.Var(_, g), args) ->
          (match g with
            Var.V(f) ->
              let fdefs = Prog.fdefs_of prog f in
              fenv,
              List.mapi
                (fun i fd ->
                  let fargs = List.map (fun arg -> Var.V(arg)) fd.Fdef.args in
                  let faargs =
                    try
                      List.combine fargs args
                    with _ -> begin
                      Format.printf "formal args: %a@." (Util.pr_list Var.pr ", ") fargs;
                      Format.printf "actual args: %a@." (Util.pr_list Term.pr ", ") args;
                      assert false
                    end
                  in
                  let sub x = List.assoc x faargs in
                  Call((g, uid), Term.subst sub fd.Fdef.guard),
                  Node((gen_id (), p @ [i]), ctx (Term.subst sub fd.Fdef.body), ref []))
                fdefs
          | Var.T(_, _, _) ->
              let f = try fenv g with Not_found -> assert false in
              fenv, [Call((g, uid), Term.ttrue), Node((gen_id (), p), ctx (Term.apply f args), ref [])])
      | Term.Ret(_, Term.Var(a, ret), t, ty) ->
          fenv, [Ret(ret, t, ty), Node((gen_id (), p), ctx (Term.Var(a, ret)), ref [])]
      | _ -> begin
          Format.printf "%a@." Term.pr red;
          assert false
         end
    in
    let _ = cs := gns in
    fenv, List.map snd gns
  with Not_found -> (*no redex found*)
    fenv, []

let find_node rt path = assert false

