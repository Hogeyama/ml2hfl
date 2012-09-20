open ExtList
open ExtString
open Trace

(** Computation trees *)

(** {6 Types} *)

(** Computation trees *)
type t = { uid: int; path: int list; term: Term.t; children: (Trace.s * t) list ref }

(** {6 Basic functions} *)

let make uid path term children =
  { uid = uid; path = path; term = term; children = children; }

(** generate a new id of a function call *)
let gen_id =
  let cnt = ref 0 in
  fun () -> cnt := !cnt + 1; !cnt

(** @return the structured variables of the return value and arguments *)
let ret_args f uid arity = 
  Term.make_var (Var.T(f, uid, arity)),
  List.init arity (fun i -> Term.make_var (Var.T(f, uid, i)))

(** @param ct a computation tree 
    @return traces of ct *)
let rec traces_of ct =
  Util.concat_map (fun (g, n) -> let ps = traces_of n in if ps = [] then [[g]] else List.map (fun p -> g::p) ps) !(ct.children)

let error_traces_of ct = List.filter (fun p -> List.last p = Error) (traces_of ct)

let save_as_dot filename ct wl =
  let node_name uid t = (String.of_int uid) ^ ": " ^ Term.string_of t in
  let f s =
    match s with
      Call(_, t) -> Term.string_of t
    | Arg(xttys) -> Term.string_of (Formula.of_subst xttys)
    | Ret(x, t, ty) -> Term.string_of (Formula.of_subst [x, t, ty])
    | Nop -> ""
    | Error -> ""
  in
  let rec traverse (s, l) ct =
    let s' = node_name ct.uid ct.term in
    (s, s', l)::
    (List.concat
      (List.map
        (fun (g, n) ->
          traverse
            (s', "[label = \"" ^ (f g) ^ "\"]")
            n)
        !(ct.children)))
  in
  let es = List.unique (traverse ("", "") ct) in
  let vs = List.unique (List.concat (List.map (fun (x, y, _) -> [x, ""; y, ""]) es)) in
  let es = List.filter (fun (x, _, _) -> x <> "") es in
  let vs = List.filter (fun (x, _) -> x <> "") vs in
  let vs = List.map
    (fun (x, _) ->
      if List.exists (fun ct -> x = node_name ct.uid ct.term) wl then
        (x, "[style = dashed]")
      else
        (x, ""))
    vs
  in
  Util.save_as_dot filename vs es

(** {6 Functions on computation trees} *)

(** @param prog a program
    initialize a computation tree of prog *)
let init prog =
  let uid = gen_id () in
  let ty_main = Prog.type_of prog (Var.V(prog.Prog.main)) in
  let ret, args =
    ret_args
     (Var.V(prog.Prog.main))
     uid
     (SimType.arity ty_main)
  in
  let _, retty = SimType.args_ret ty_main in
  make uid [] (Term.Ret([], ret, Term.Call([], Term.make_var (Var.make prog.Prog.main), args), retty)) (ref [])

let emp_fun_env x =
  let _ = Format.printf "\"%a\" not found@," Var.pr x in
  assert false
let expand_node prog fenv ct =
  let _ =
    if !(ct.children) <> [] then
      let _ =  Format.printf "given node is already expanded@," in
      assert false
  in
  try
    let ctx, red = Term.redex_of (Prog.type_of prog) ct.term in
    let fenv, gns =
      match red with
        Term.App(_, Term.Const(_, Const.Event(id)), _(*???*)) when Idnt.string_of id = Term.event_fail ->
          fenv, [Error, make (gen_id ()) ct.path (Term.Error([])) (ref [])]
      | Term.App(_, Term.Const(_, Const.RandInt), t(*???*)) ->
          fenv, [Nop, make (gen_id ()) ct.path (ctx (Term.apply t [Term.make_var (Var.make (Idnt.new_id ()))])) (ref [])]
      | Term.App(_, _, _) ->
          let func, args = Term.fun_args red in
          let attr, f =
            match func with
              Term.Var(attr, f) -> attr, f
            | _ -> let _ = Format.printf "%a cannot be applied@," Term.pr red in assert false
          in
          let uid = gen_id () in
          let argtys, retty = SimType.args_ret (Prog.type_of prog f) in
          let ret, fargs = ret_args f uid (SimType.arity (Prog.type_of prog f)) in
          let reduct = Term.Ret([], ret, Term.Call([], Term.Var(attr, f), fargs), retty) in
          let faargs =
            try
              List.combine fargs args
            with _ -> begin
              Format.printf "formal args: %a@," (Util.pr_list Term.pr ", ") fargs;
              Format.printf "actual args: %a@," (Util.pr_list Term.pr ", ") args;
              assert false
            end
          in
          let faargs_fun = List.filter
            (function (Term.Var(_, x), _) -> not (Prog.is_base prog x) | _ -> assert false)
            faargs
          in
(*
          let pr ppf (t1, t2) = Format.fprintf ppf "%a: %a" Term.pr t1 Term.pr t2 in
          let _ = Format.printf "faargs_fun: %a@," (Util.pr_list pr ", ") faargs_fun in
*)
          let fenv x =
            try
              Util.find_app
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
          make uid ct.path (ctx reduct) (ref [])]
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
                      Format.printf "formal args: %a@," Var.pr_list fargs;
                      Format.printf "actual args: %a@," Term.pr_list args;
                      assert false
                    end
                  in
                  let sub x = List.assoc x faargs in
                  Call((g, ct.uid), TypSubst.subst sub fd.Fdef.guard),
                  make (gen_id ()) (ct.path @ [i]) (ctx (TypSubst.subst sub fd.Fdef.body)) (ref []))
                fdefs
          | Var.T(_, _, _) ->
              let f = try fenv g with Not_found -> assert false in
              fenv, [Call((g, ct.uid), Formula.ttrue), make (gen_id ()) ct.path (ctx (Term.apply f args)) (ref [])])
      | Term.Ret(_, Term.Var(a, ret), t, ty) ->
          fenv, [Ret(ret, t, ty), make (gen_id ()) ct.path (ctx (Term.Var(a, ret))) (ref [])]
      | _ -> begin
          Format.printf "%a@," Term.pr red;
          assert false
         end
    in
    let _ = ct.children := gns in
    fenv, List.map snd gns
  with Not_found -> (*no redex found*)
    fenv, []
