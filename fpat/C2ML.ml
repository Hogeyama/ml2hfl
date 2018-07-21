open Util
open Combinator

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

let generate_dummy = ref false

(* @todo support vararg
         support function pointers *)

let vars_of_lval ?(rename = id) (v, off) =
  let v' = SSATrans.var_of_lval (v, off) in
  if isScalarType v'.vtype then
    [v'.vname]
  else
    v'.vtype
    |> SSATrans.all_scalar_offsets
    |> List.map
         (fun off ->
          (rename (SSATrans.var_of_lval (v', off))).vname)

let rec of_type = function
  | TVoid(_) ->
     Type.mk_unit(* @todo *)
  | TInt(_, _) ->
     Type.mk_int
  | TFloat(_, _) ->
     Type.mk_real
  | TPtr(_, _) ->
     assert false
  | TArray(_, _, _) ->
     assert false
  | TFun(ret, args, _, _) ->
     args
     |> Option.fold
          [Type.mk_unit]
          (List.map (Triple.snd >> of_type))
     |> flip (@) [of_type ret]
     |> Type.mk_fun
  | TNamed(ti, _) ->
     Logger.printf "TNamed: %a@," String.pr ti.tname;
     (*Type.Ext(Idnt.make ti.tname)*)
     of_type ti.ttype
  | TComp(_, _) ->
     assert false
  | TEnum(_, _) ->
     assert false
  | TBuiltin_va_list(_) ->
     assert false

let of_bop ty = function
  | PlusA -> Const.Add(ty)
  | PlusPI
  | IndexPI ->
     (* @todo this branch is supposed to be unreachable *)
     (*assert false*)
     Const.Add(ty)
  | MinusA -> Const.Sub(ty)
  | MinusPI | MinusPP -> assert false
  | Mult -> Const.Mul(ty)
  | Div -> Const.Div(ty)
  | Mod -> Const.Mod
  | Lt -> Const.Lt(ty)
  | Gt -> Const.Gt(ty)
  | Le -> Const.Leq(ty)
  | Ge -> Const.Geq(ty)
  | Eq -> Const.Eq(ty)
  | Ne -> Const.Neq(ty)
  | Shiftlt -> Const.BitShiftLeft
  | Shiftrt -> Const.BitShiftRight
  | BAnd -> Const.BitAnd
  | BXor -> Const.BitXor
  | BOr -> Const.BitOr
  | LAnd -> Const.And
  | LOr -> Const.Or

let of_uop ty = function
  | Neg -> Const.Neg(ty)
  | BNot -> Const.BitNot
  | LNot -> Const.Not

let mlexp_of_basic ?(rename = id) = function
  | Const(CInt64(i, _, _)) ->
     IntTerm.make (Int64.to_int i)
  | Const(CStr(str)) ->
     MLExp.mk_const (Const.String(str))
  | Const(CWStr(_)) ->
     assert false
  | Const(CChr(c)) ->
     IntTerm.make (Char.code c) (* @todo *)
  | Const(CReal(f, _, _)) ->
     MLExp.mk_const (Const.Real(f))
  | Const(CEnum(_, _, _)) ->
     assert false
  | AddrOf(Var(v), NoOffset) ->
     assert false
  | StartOf(Var(v), NoOffset) ->
     assert false
  | Lval(Var(v), off) ->
     vars_of_lval ~rename:rename (v, off)
     |> List.map Term.var_of_string
     |> TupTerm.mk_tuple2
  | b ->
     printf "error in mlexp_of_basic: %a@," d_exp b;
     assert false

let mlexp_of_exp ?(rename = id) = function
  | Lval(Mem(b), NoOffset) ->
     assert (not !Heap2Array.disable_heap);
     Term.mk_app
       (Term.var_of_string (rename Heap2Array.global_heap_var).vname)
       [mlexp_of_basic ~rename:rename b]
  | BinOp(bop, b1, b2, ty) ->
     Term.mk_app
       (Term.mk_const (of_bop (of_type ty) bop))
       [mlexp_of_basic ~rename:rename b1;
        mlexp_of_basic ~rename:rename b2]
  | UnOp(uop, b, ty) ->
     Term.mk_app
       (Term.mk_const (of_uop (of_type ty) uop))
       [mlexp_of_basic ~rename:rename b]
  | CastE(_, b) ->
     mlexp_of_basic ~rename:rename b
  | e ->
     mlexp_of_basic ~rename:rename e

let fname_of base sid =
  base ^ string_of_int sid

let fname_of_exp f =
  match f with
  | Lval(Var(v), NoOffset) ->
     Term.var_of_string (fname_of v.vname 0)

(*
let idom tr s s' =
  Dominators.children tr s
  |> List.map (fun s -> s.sid)
  |> List.mem s'.sid
 *)

(** @todo check whether the basic block with sid must be translated to a function definition *)
let is_fun phi_map sid =
  try
    List.assoc sid phi_map <> []
  with Not_found ->
    false

let initial_heap = "initial_heap"
let main = "main"

(** [t] is the continuation *)
let rec mlexp_of_instruction
          fd phi_map env_map global_defs
          rename_before rename_after
          e = function
  | Set(lv, ce, _) ->
     begin
       match lv with
       | (Mem(b), NoOffset) ->
          assert false
       | (Var(v), off) ->
          MLExp.mk_let_pat Type.mk_unknown
            (vars_of_lval ~rename:rename_after (v, off)
             |> List.map (fun x -> Pattern.V(Idnt.make x))
             |> Pattern.of_list,
             Type.mk_top)
            (mlexp_of_exp ~rename:rename_before ce)
            e
       | _ ->
          assert false
     end
  | Call(ret, f, args, _) ->
     let f, glob_rets, is_vararg, num_of_args =
       match f with
       | Lval(Var(v), off) ->
          assert (off = NoOffset); (* @todo does this always hold? *)
          if v.vname = Heap2Array.update_heap then
            (* call to update_heap function *)
            Term.var_of_string Heap2Array.update_heap,
            [],
            false,
            3
          else
            (* call to some other function *)
            let TFun(_, args, is_vararg, _) = v.vtype in
            fname_of_exp f,
            List.map
              (fun (v, _) -> (rename_after v).vname)
              global_defs,
            is_vararg,
            List.length (argsToList args)
       | Lval(Mem(b), NoOffset) ->
          raise (Global.NotImplemented
                   "function pointers not supported in C2ML.mlexp_of_dom_tree")
       | _ ->
          assert false
     in
     let ret_pat =
       Pattern.of_list
         (match ret with
          | None ->
             [Pattern.V(Idnt.make "_")(*Idnt.string_of (Idnt.new_id ())*)]
          | Some(Mem(_), NoOffset) ->
             assert false
          | Some(Var(v), off) ->
             vars_of_lval ~rename:rename_after (v, off)
             |> List.map (fun x -> Pattern.V(Idnt.make x))
          | _ ->
             assert false)
       |> if glob_rets = [] then
            id
          else
            List.return
            >> (@) (List.map
                      (fun y -> Pattern.V(Idnt.make y))
                      glob_rets)
            >> Pattern.of_list
     in
     let ret_body =
       let args1, args2 =
         try
           args
           |> List.map (mlexp_of_exp ~rename:rename_before)
           |> List.split_nth num_of_args
         with _ ->
           Format.printf
             "function: %a@,# of args: %d@,"
             Term.pr f
             num_of_args;
           assert false
       in
       Logger.debug_assert
         (fun () -> Bool.imply (not is_vararg) (List.length args2 = 0))
         ~on_failure:
         (fun () ->
          Format.printf
            "function: %a@,# of args: %d@,"
            Term.pr f
            num_of_args);
       MLExp.mk_app
         f
         (args1
          @ (if is_vararg then
               [ADTTerm.mk_list (Type.mk_top(*@todo*)) args2]
             else
               [])
          |> if_ ((=) [])
                 (const [UnitTerm.make])
                 id)
     in
     MLExp.mk_let_pat Type.mk_unknown (ret_pat, Type.mk_top) ret_body e
  | i ->
     printf "instr: %a@," d_instr i;
     assert false
and mlexp_of_jump
      fd phi_map env_map global_defs
      tr
      s s' =
  if is_fun phi_map s'.sid then
    phi_map
    |> List.assoc s'.sid
    |> List.filter_map
         (fun (_, vs) ->
          try
            (List.assoc s.sid vs).vname
            |> Term.var_of_string
            |> Option.return
          with Not_found -> None)
    |> MLExp.mk_app
         (fname_of fd.svar.vname s'.sid
          |> Term.var_of_string)
  else
    mlexp_of_dom_tree fd phi_map env_map global_defs tr s'
and mlexp_of_statement
      fd phi_map env_map global_defs
      tr
      ctx s =
  let envs =
    try
      H.find env_map s.sid
    with Not_found ->
      assert false
  in
  match s.skind with
  | Instr(instrs) ->
     let [s'] = s.succs in
     List.fold_right
       (fun i (env' :: env :: envs, e) ->
        env :: envs,
        mlexp_of_instruction
          fd phi_map env_map global_defs
          (fun v -> SSATrans.lookup_env v env)
          (fun v -> SSATrans.lookup_env v env')
          e
          i)
       instrs
       (List.rev envs,
        ctx (mlexp_of_jump fd phi_map env_map global_defs tr s s'))
     |> snd
  | Return(ret, _) ->
     let [env; _] = envs in
     List.map
       (fun (v, _) -> (SSATrans.lookup_env v env).vname |> Term.var_of_string)
       global_defs
     @ Option.fold
         [UnitTerm.make]
         (mlexp_of_exp
            ~rename:(fun v -> SSATrans.lookup_env v env)
          >> List.return)
         ret
     |> TupTerm.mk_tuple2
     |> ctx
  | If(ce, _, _, _) ->
     let [env; _] = envs in
     let [s1; s2] = s.succs in
     (* @todo: s2 is always then branch? *)
     MLExp.mk_if
       Type.mk_unknown
       (ce
        |> mlexp_of_exp ~rename:(fun v -> SSATrans.lookup_env v env)
        |> Formula.of_term
        |> CunFormula.of_c_formula
        |> Formula.term_of)
       (mlexp_of_jump fd phi_map env_map global_defs tr s s2)
       (mlexp_of_jump fd phi_map env_map global_defs tr s s1)
     |> ctx
  | Switch(ce, _, _, _) ->
     assert false
  | Goto _ ->
     let [s'] = s.succs in
     ctx (mlexp_of_jump fd phi_map env_map global_defs tr s s')
  | Break _
  | Continue _ ->
     assert false
  | Loop _
  | Block _ ->
     let [s'] = s.succs in
     ctx (mlexp_of_jump fd phi_map env_map global_defs tr s s')
  | TryFinally _
  | TryExcept _ ->
     assert false
and mlexp_of_dom_tree
      fd phi_map env_map global_defs
      tr s =
  (*Logger.printf2 "####%a####@,%a@," Integer.pr s.sid d_stmt s;*)
  let ctx =
    Dominators.children tr s
    |> List.filter_map
         (fun s ->
          if is_fun phi_map s.sid then
            Some(mlexp_of_dom_tree fd phi_map env_map global_defs tr s)
          else
            None)
    |> List.fold_right
         (fun fd e2 ->
          MLExp.let_letrec
            fd
            (fun [xty, xtys, e1] e2' [] ->
             MLExp.let_bot
               e2'
               (fun () ->
                MLExp.mk_letrec
                  [xty, xtys, e1]
                  e2)))
  in
  mlexp_of_statement fd phi_map env_map global_defs tr ctx s
  |> (if s.sid = 0 then
        (* compute the set of uninitialized variables *)
        let env :: _ = H.find env_map 0 in
        SSATrans.vars_of fd.slocals
        |> List.filter_map
             (fun v ->
              try
                Some(SSATrans.lookup_env_exc v env)
              with Not_found ->
                Logger.printf "???: %a@," String.pr v.vname;
                None (* @todo why this point is reachable? *)
             (* assert false *))
        |> List.map
             (fun v ->
              Logger.printf "uninitialized var: %a@," String.pr v.vname;
              v)
        |> List.fold_right
             (fun v e ->
              MLExp.mk_rand Type.mk_unknown (Idnt.make v.vname, Type.mk_top) e)
      else
        id)
  |> if is_fun phi_map s.sid then
       let phis = List.assoc s.sid phi_map in
       if s.sid = 0 then
         let TFun(_, _, is_vararg, _) = fd.svar.vtype in
         if fd.svar.vname = main then
           (* add initialization code for global variables
              to the special function "main" *)
           List.fold_right
             (fun (v, e1) e2 ->
              let v' =
                let env :: _ =
                  try
                    H.find env_map s.sid
                  with Not_found ->
                    assert false
                in
                SSATrans.lookup_env v env
              in
              MLExp.mk_let Type.mk_unknown (Idnt.make v'.vname, Type.mk_top) e1 e2)
             global_defs
         else
           begin
             Logger.printf
               "%a is of sid 0! why?@,"
               String.pr fd.svar.vname;
             id
           end
           >> fun e ->
              MLExp.mk_letrec
                [(Idnt.make (fname_of fd.svar.vname s.sid), Type.mk_top),
                 (List.map (fun v -> Idnt.make v.vname, Type.mk_top) fd.sformals
                  @ if is_vararg then
                      [Idnt.make "_"(*@todo*), Type.mk_top]
                    else
                      []),
                 e]
                MLExp.mk_bot
       else
         fun e ->
         MLExp.mk_letrec
           [(Idnt.make (fname_of fd.svar.vname s.sid), Type.mk_top),
            (List.map (fun (v, _) -> Idnt.make v.vname, Type.mk_top) phis),
            e]
           MLExp.mk_bot
     else
       id

class funCVisitor maps global_defs fdefs =
object (self) inherit nopCilVisitor
  method vfunc fd =
    let _, tr =
      Dominators.computeDomTree ~doCFG:false fd
    in
    let fdef =
      let start =
        List.hd fd.sbody.bstmts
      in
      let phi_map, env_map =
        H.find maps fd.svar
      in
      let fdef = mlexp_of_dom_tree fd phi_map env_map global_defs tr start in
      MLExp.let_letrec
        fdef
        (fun [xty, xtys, e1] e2 [] ->
         MLExp.let_bot
           e2
           (fun () ->
            Fdef.make
              (xty |> fst |> Idnt.string_of)
              (List.map (fun (x, _) -> Pattern.V(x)) xtys)
              Formula.mk_true
              e1))
    in
    Logger.printf "(* %a *)@," String.pr fd.svar.vname;
    Logger.printf "%a@," Fdef.pr fdef;
    fdefs := !fdefs @ [fdef];
    SkipChildren
end

let heap_fdefs =
  if !Heap2Array.disable_heap then
    []
  else
    (* heap manipulating functions *)
    [Fdef.make
       initial_heap
       [Pattern.V(Idnt.make "i")]
       Formula.mk_true
       IntTerm.zero;
     Fdef.make
       Heap2Array.update_heap
       [Pattern.V(Idnt.make "f");
        Pattern.V(Idnt.make "i");
        Pattern.V(Idnt.make "x");
        Pattern.V(Idnt.make "j")]
       Formula.mk_true
       (MLExp.mk_if
          Type.mk_unknown
          (IntFormula.eq
             (Term.var_of_string "i")
             (Term.var_of_string "j")
           |> Formula.term_of)
          (Term.var_of_string "x")
          (MLExp.mk_app
             (Term.var_of_string "f")
             [Term.var_of_string "j"]));
    (*
     Fdef.make
       "shift"
       [Pattern.V(Idnt.make "f");
        Pattern.V(Idnt.make "x");
        Pattern.V(Idnt.make "i")]
       (MLExp.mk_app
          (Term.var_of_string "f")
          [IntTerm.add (Term.var_of_string "x") (Term.var_of_string "i")])
     *)
    ]

(** @todo use rand instead *)
let uninitialized = IntTerm.make 555

let dummy_fdef_of_var num_of_globals v =
  match v.vtype with
  | TFun(ret, Some(args), is_vararg, _) ->
     let t =
       match ret with
       | TVoid(_) -> UnitTerm.make
       | TInt(_, _) -> uninitialized
       | _ ->
          Logger.printf "The type of %a is not supported@," String.pr v.vname;
          (*assert false*)
          uninitialized
     in
     let gargs, fargs =
       List.split_nth
         num_of_globals
         (List.map (fun (s, _, _) -> Idnt.make s) args)
     in
     Fdef.make
       (v.vname ^ "0") (* @todo this function name may conflict *)
       (List.map (fun x -> Pattern.V(x)) gargs
        @ List.duplicate
            (Pattern.V(Idnt.make "_"))
            (List.length fargs + if is_vararg then 1 else 0))
       Formula.mk_true
       (TupTerm.make
          (List.duplicate
             Type.mk_top(* @todo *)
             (List.length gargs + 1))
          ((List.map MLExp.mk_var gargs) @ [t]))
     |> Option.return
  | _ ->
     None

(** @require [f] has a function [main]
    @require [main] does not have an argument with the same name as a global variable *)
let do_func_feature (f:file) =
  Logger.printf0 "@[<v>";

  (* heapify local array variables and address taken non-array variables*)
  Heapify.heapifyNonArrays := true;
  Heapify.default_heapify f;

  (* simplification *)
  (* @todo without this, prepareCFG or computeCFGInfo generates a basic block with sid = -1? *)
  iterGlobals f Simplify.doGlobal;
  (* @todo need to apply doGlobal twice to avoid an expression like "*((T)exp)"? *)
  iterGlobals f Simplify.doGlobal;
  Logger.printf0
    "########## after heapifying and simplification ##########@,";
  dumpFile (!printerForMaincil) stdout "stdout" f;

  (* heap to array transformation *)
  Heap2Array.heap2array f;
  Logger.printf0
    "########## after heap to array transformation ##########@,";
  dumpFile (!printerForMaincil) stdout "stdout" f;

  (* CFG construction *)
  Cil.iterGlobals
    f
    (fun glob ->
     match glob with
     | Cil.GFun(fd, _) ->
        Cil.prepareCFG fd;
        Cil.computeCFGInfo fd false
     | _ -> ());

  (* simplification *)
  iterGlobals f Simplify.doGlobal;

  Logger.printf0
    "########## pre-processed program ##########@,";
  dumpFile (!printerForMaincil) stdout "stdout" f;
  Rmtmps.removeUnusedTemps f; (* necessary for (1) *)

  (* process global variables and declaration *)
  let global_defs = ref [] in
  let external_vars = ref [] in
  Cil.iterGlobals
    f
    (fun glob ->
     match glob with
     | Cil.GVar(v, init, _) ->
        let init =
          match init.init (* @todo v.vinit.init does not work *) with
          | Some(SingleInit(e)) ->
             mlexp_of_exp e
          | Some(CompoundInit(ty, offset_init_list)) ->
             assert false
          | None ->
             if v.vname = Heap2Array.global_heap then
               Term.var_of_string initial_heap
             else
               uninitialized (* @todo check v.vtype *)
        in
        Logger.debug_assert
          (fun () -> not (List.mem v (Map_.dom !global_defs)))
          ~on_failure:
          (fun () ->
           Format.printf
             "The global variable %s is already defined@,"
             v.vname);
        global_defs := Map_.update !global_defs v init
     (* @todo do not pass around global variables which are never updated *)
     | Cil.GVarDecl(v, _)
          when v.vstorage = Extern (* ignore non-external variable declarations *)
               && v.vreferenced (* (1) ignore nonused external variable declarations *) ->
        begin
          match v.vtype with
          | TInt(_, _) ->
             Logger.debug_assert
               (fun () -> not (List.mem v !external_vars))
               ~on_failure:
               (fun () ->
                Format.printf
                  "The external variable %s is already declared@,"
                  v.vname);
             external_vars := v :: !external_vars
          | TFun(_, _, _, _) ->
             ()
          | _ ->
             assert false
        end
     | _ -> ());

  (* SSA transformation *)
  let globals =
    List.map fst !global_defs
  in
  let maps =
    SSATrans.do_ssa_feature f globals external_vars
  in

  (* analyze the type of each external function and generate its dummy definition
     @todo the following code assumes that each external function does not access the global heap *)
  let ext_fdefs = ref [] in
  if !generate_dummy then
    Cil.iterGlobals
      f
      (function
        | Cil.GVarDecl(v, _)
             when v.vstorage = Extern (* ignore non-external variable declarations *)
                  && v.vreferenced (* (1) ignore nonused external variable declarations *) ->
           Option.fold
             ()
             (fun fdef -> ext_fdefs := !ext_fdefs @ [fdef])
             (dummy_fdef_of_var (List.length globals) v)
        | _ -> ());

  Logger.printf0 "########## functional program ##########@,";
  let fdefs = ref [] in
  visitCilFileSameGlobals (new funCVisitor maps !global_defs fdefs) f;

  Logger.printf0 "########## inlined functional program ##########@,";
  let fdefs =
    heap_fdefs
    @ !ext_fdefs
    @ List.map
        (Fdef.map_body
           (MLExp.inline []
            >> MLExp.simplify
            >> MLExp.inline []
            >> MLExp.simplify (* @todo compute the fixed-point *)))
        !fdefs
  (*
    @ Fdef.make
        "_"
        []
        Formula.mk_true
        (MLExp.mk_app
           (Term.var_of_string (main ^ "0"))
           [UnitTerm.make(*@todo check type*)])
      *)
  in
  let oc =
    open_out (Filename.chop_extension f.fileName ^ ".ml")
  in
  Format.printf
    "%a@,"
    (List.pr Fdef.pr "@,") fdefs;
  Format.fprintf
    (Format.formatter_of_out_channel oc)
    "%a@."
    (List.pr Fdef.pr "@.") fdefs;
  close_out oc;

  Format.printf "@]"

let feature =
  {
    fd_name = "funC";
    fd_enabled = ref true;
    fd_description = "A Software Model Checker for C based on Functionalization";
    fd_extraopt = [];
    fd_doit = do_func_feature;
    fd_post_check = false
  }
