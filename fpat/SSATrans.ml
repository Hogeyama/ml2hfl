open Util
open Combinator

open Pretty
open Cil
module E = Errormsg
module H = Hashtbl

(** This transformation generates a C program, which is incomplete in the following sense:
    (1) The transformed program ignores phi functions.
    (2) Each function definition and call-site respectively should return and update global variables.
        This is not the case with this transformation.
        (We should have prepared and used a struct to contain all the global variables.)
    The resulting incomplete program, however, can be properly handled by the FunC module.
    @require the input program does not call an external function *)

(** {6 functions for variables of structures} *)

let var_of_lval (v, off) =
  let rec aux path off =
    match off with
    | Field(fi, NoOffset) ->
       makeVarinfo false (path ^ "_" ^ fi.fname) fi.ftype
    | Field(fi, off) ->
       aux (path ^ "_" ^ fi.fname) off
    | Index(e, off) ->
       assert false
  in
  match off with
  | NoOffset -> v
  | _ -> aux v.vname off

let rec all_scalar_offsets typ =
  match typ with
  | TComp(ci, _) when ci.cstruct ->
     List.concat_map
       (fun fi ->
        all_scalar_offsets fi.ftype
        |> List.map (fun off -> Field(fi, off)))
       ci.cfields
  | _ ->
     assert (isScalarType typ);
     [NoOffset]

(** {6 Functions for manipulating environments} *)

let vars_of =
  List.concat_map
    (fun v ->
     all_scalar_offsets v.vtype
     |> List.map (fun off -> var_of_lval (v, off)))

let make_env fd =
  List.concat_map
    (fun v ->
     all_scalar_offsets v.vtype
     |> List.map
          (fun off ->
           let v = var_of_lval (v, off) in
           v, makeTempVar fd v.vtype))

let update_env (v : varinfo) (v' : varinfo) (env : (varinfo * varinfo) list) =
  List.map
    (fun (v1, v2) ->
     if v1.vname = v.vname then v1, v' else v1, v2)
    env

let assign env fd v off =
  let v = var_of_lval (v, off) in
  let vvs =
    all_scalar_offsets v.vtype
    |> List.map
         (fun off ->
          let v = var_of_lval (v, off) in
          v, makeTempVar fd v.vtype)
  in
  List.fold_left
    (fun env (v, v') -> update_env v v' env)
    env vvs,
  if isScalarType v.vtype then
    let [_, v'] = vvs in
    (Var(v'), NoOffset)
  else
    (Var(v), NoOffset)

let lookup_env v env =
  try
    snd (List.find (fun (v', _) -> v.vname = v'.vname) env)
  with Not_found ->
    Format.printf "%s@," v.vname;
    assert false

let lookup_env_exc v env =
  snd (List.find (fun (v', _) -> v.vname = v'.vname) env)

(** {6 Functions for substitution} *)

(*
class substVisitor sub =
object inherit nopCilVisitor
  method vexpr e =
    match e with
    | Lval(Var(v), NoOffset) ->
       (try
           let e' = lookup_env_exc v sub in
           ChangeTo(e')
         with Not_found ->
           DoChildren)
    | _ ->
       DoChildren
end
*)

(** {6 Functions for renaming} *)

class renameVisitor env =
object inherit nopCilVisitor
  (*
  method vvdec v =
    try
      ChangeTo(lookup_env_exc v env)
    with Not_found ->
      DoChildren
  method vvrbl v =
    try
      ChangeTo(lookup_env_exc v env)
    with Not_found ->
      DoChildren
   *)
  method vlval lv =
    match lv with
    | (Mem(Lval(Var(v), off)), NoOffset) ->
       begin
         assert (not !Heap2Array.disable_heap);
         try
           ChangeTo
             (Mem(Lval(Var(lookup_env_exc
                             (var_of_lval (v, off))
                             env), NoOffset)), NoOffset)
         with Not_found ->
           DoChildren
       end
    (*
    | (Mem(CastE(t, Lval(Var(v), off))), NoOffset) ->
       begin
         try
           ChangeTo
             (Mem(CastE(t, Lval(Var(lookup_env_exc
                                      (var_of_lval (v, off))
                                      env), NoOffset))), NoOffset)
         with Not_found ->
           DoChildren
       end
     *)
    | (Var(v), off) ->
       begin
         let v = var_of_lval (v, off) in
         (*Format.printf "from: %s@," v.vname;*)
         try
           (*List.iter (fun (v, v') -> Format.printf "(%s, %s)@," v.vname v'.vname) env;*)
           let v' = lookup_env_exc v env in
           (*Format.printf "to: %s@," v'.vname;*)
           ChangeTo(Var(v'), NoOffset)
         with Not_found ->
           DoChildren
       end
    | _ ->
       ignore (printf "lv: %a\n" d_lval lv);
       assert false
end
let rename_expr env e =
  visitCilExpr (new renameVisitor env) e
let rename_function env fd =
  visitCilFunction (new renameVisitor env) fd

(** {6 Functions for SSA transformation} *)

class ssaVisitor
        fd globals externals locals(*@todo why we need this?*)
        env_map =
object (self) inherit nopCilVisitor
  method vstmt s =
    (* basic block *)
    let env = make_env fd (globals @ locals) in
    let envs =
      match s.skind with
        Instr(instrs) ->
        let instrs', envs =
          List.fold_left
            (fun (instrs, ((env :: _) as envs)) i ->
             match i with
             | Set(lv, e, loc) ->
                let e' = rename_expr env e in
                begin
                  match lv with
                  | (Mem(Lval(Var(v), off)), NoOffset) ->
                     (*| (Mem(CastE(_, Lval(Var(v), off))), NoOffset) ->*)
                     assert (not !Heap2Array.disable_heap);
                     let v' = lookup_env v env in
                     instrs @ [Set((Mem(Lval(Var(v'), off)), NoOffset), e', loc)],
                     env :: envs
                  | (Var(v), off) ->
                     let env', lv' = assign env fd v off in
                     instrs @ [Set(lv', e', loc)],
                     env' :: envs
                  | _ ->
                     assert false
                end
             | Call(Some(Var(v), off), Lval(Var(v'), NoOffset), args, loc)
                  when v'.vname = Heap2Array.update_heap ->
                let args' = List.map (rename_expr env) args in
                let env', lv' = assign env fd v off in
                instrs @ [Call(Some(lv'), Lval(Var(v'), NoOffset), args', loc)],
                env' :: envs
             | Call(opt, f, args, loc) ->
                let f' = rename_expr env f in
                let args' = List.map (rename_expr env) args in
                let globals' =
                  List.map
                    (fun v ->
                     Lval(Var(lookup_env v env), NoOffset))
                    globals
                in
                let vs = List.map (fun v -> makeTempVar fd v.vtype) globals in
                let env' =
                  List.fold_left
                    (fun env (v, v') -> update_env v v' env)
                    env (List.combine globals vs)
                in
                let opt', env' =
                  match opt with
                  | None ->
                     None, env'
                  | Some(Mem(Lval(Var(v), off)), NoOffset) ->
                     (*| Some(Mem(CastE(_, Lval(Var(v), off))), NoOffset) ->*)
                     assert (not !Heap2Array.disable_heap);
                     let v' = lookup_env v env in
                     Some(Mem(Lval(Var(v'), off)), NoOffset), env
                  | Some(Var(v), off) ->
                     let env', lv' = assign env' fd v off in
                     Some(lv'), env'
                  | _ ->
                     assert false
                in
                instrs @ [Call(opt', f', globals' @ args', loc)],
                env' :: envs
             | _ ->
                instrs @ [i], env :: envs)
            ([], [env])
            instrs
        in
        (s.skind <- Instr instrs');
        List.rev envs
      | Return(Some(e), _)
      | If(e, _, _, _)
      | Switch(e, _, _, _) ->
         let sk =
           let e' = rename_expr env e in
           match s.skind with
           | Return(Some(e), loc) ->
              Return(Some(e'), loc)
           | If(e, b1, b2, loc) ->
              If(e', b1, b2, loc)
           | Switch(e, b, ss, loc) ->
              Switch(e', b, ss, loc)
         in
         (s.skind <- sk);
         [env]
      | Return(None, _)
      | Goto _
      | Break _
      | Continue _
      | Loop _
      | Block _ ->
         [env]
      | TryFinally _
      | TryExcept _ ->
         ignore (printf "s: %a\n" d_stmt s);
         assert false
    in
    Format.printf
      "########## basic block %d of %s ##########@,"
      s.sid fd.svar.vname;
    Format.printf
      "before:@,  %a\n"
      (Map_.pr String.pr String.pr) (List.map (fun (v1, v2) -> v1.vname, v2.vname) env);
    printf "code:\n  %a\n" d_stmt s;
    Format.printf
      "after:@,  %a\n"
      (Map_.pr String.pr String.pr) (List.map (fun (v1, v2) -> v1.vname, v2.vname) (List.last envs));
    H.add env_map s.sid (env :: envs);
    DoChildren
end

let print_phis (sid, phis) =
  List.iter
    (fun (v, vs) ->
     Format.printf
       "phi%d: %s -> %s@,"
       sid
       v.vname
       (String.concat " | " (List.map (fun (_, v) -> v.vname) vs)))
    phis

let statement_of fd sid =
  List.find (fun s -> s.sid = sid) fd.sallstmts 

(* transform a function fd to SSA *)
let ssaFun f globals externals fd loc =
  Format.printf "@[<v>";
  (*Cfg.printCfgFilename "test.dot" fd;*)
  (* analyze fd *)
  let env_map = H.create (List.length fd.sallstmts) in
  visitCilFunction
    (new ssaVisitor
         fd globals externals (fd.sformals @ fd.slocals)
         env_map)
    fd;
  (* transform fd *)
  begin
    let env :: _ = H.find env_map 0 (* initial statement *) in
    (* rename formal arguments *)
    fd.sformals <-
      List.map
        (fun v -> lookup_env v env)
        fd.sformals;
    if fd.svar.vname <> "main" then
      (* add formal arguments to receive global variables *)
      List.iter
        (fun v ->
         let v' = lookup_env v env in
         ignore (makeFormalVar ~where:"^" fd v'.vname v'.vtype))
        (List.rev globals)
  end;
  (* Rmtmps.removeUnusedTemps f; is unsound *)

  Format.printf "########## SSA transformed ##########@,";
  printf "%a\n" d_global (GFun(fd, loc));

  (* compute phi functions *)
  let phi_map = ref [] in
  (* compute initial phi functions *)
  Format.printf "########## initial phi functions ##########@,";
  H.iter
    (fun sid (env :: _) ->
     let phis =
       let ss =
         try
           (statement_of fd sid).preds
         with Not_found ->
           assert false
       in
       List.map
         (fun (v1, v2) ->
          v2,
          List.filter_map
            (fun s ->
             try
               let env' :: _ = List.rev (H.find env_map s.sid) in
               Some(s.sid, lookup_env v1 env')
             with Not_found ->
               None)
            ss)
         env
     in
     phi_map := (sid, phis) :: !phi_map;
     print_phis (sid, phis))
    env_map;
  (* minimize phi functions:
     J. Aycock, N. Horspool
     Simple Generation of Static Single-Assignment Form
     Compiler Construction 2000 *)
  let rec minimize phi_map env_map =
    (* filtering phi functions *)
    let phi_map =
      List.map
        (fun (sid, phis) ->
         sid,
         List.filter
           (fun (v, vs) ->
            List.length vs = 0
            || List.exists (fun (_, v') -> v <> v') vs)
           phis)
        phi_map
    in
    (* extract substitution for variables to be eliminated from phi functions *)
    let sub = ref [] in
    let phi_map =
      List.map
        (fun (sid, phis) ->
         let sub', phis' =
           List.partition_map
             (fun (v, vs) ->
              let vs' =
                List.unique ~cmp:(fun v1 v2 -> v1.vname = v2.vname) (Set_.diff ~cmp:(fun v1 v2 -> v1.vname = v2.vname) (List.map snd vs) [v])
              in
              match vs' with
              | [v']
                   when (* we avoid to obtain substitution like {x -> y, y -> z} *)
                     List.for_all
                       (fun (v1, v2) -> v1.vname <> v'.vname && v2.vname <> v.vname (*v1 <> v' && v2 <> v*)) !sub ->
                 `L(v, v')
              | _ ->
                 `R(v, vs))
             phis
         in
         sub := sub' @ !sub;
         (*Format.printf "sub: %s@," (String.concat "," (List.map (fun (v1, v2) -> v1.vname ^ ", " ^ v2.vname) !sub));*)
         sid, phis')
        phi_map
    in
    (* apply substitution to phi functions *)
    let phi_map =
      phi_map
      |> List.map
         @@ Pair.map_snd
         @@ List.map
         @@ Pair.map_snd
         @@ List.map
         @@ Pair.map_snd
              (fun v' ->
               try
                 lookup_env_exc v' !sub
               with Not_found ->
                 v')
    in
    (* apply substitution to env_map *)
    H.iter
      (fun sid envs ->
       H.replace
         env_map
         sid
         (List.map
            (List.map
               (fun (v1, v2) ->
                v1, try lookup_env_exc v2 !sub with Not_found -> v2))
            envs))
      env_map;
    (* apply substitution to the body of the function fd *)
    rename_function !sub fd;

    if !sub = [] then
      phi_map, env_map
    else
      minimize phi_map env_map
  in
  let phi_map, env_map = minimize !phi_map env_map in
  Format.printf "########## minimized phi functions ##########@,";
  List.iter print_phis phi_map;
  Format.printf "@]";
  phi_map, env_map

let do_ssa_feature (f:file) globals externals =
  let maps = H.create 0 in
  Cil.iterGlobals
    f
    (function
      | Cil.GFun(fd, loc) ->
         let phi_map, env_map = ssaFun f globals externals fd loc in
         H.add maps fd.svar (phi_map, env_map)
      | Cil.GVarDecl(v, loc)
           when v.vstorage = Extern (* ignore non-external variable declarations *)
                && v.vreferenced (* (1) ignore nonused external variable declarations *) ->
         begin
           match v.vtype with
           | TFun(ret, opt, is_vararg, attr) ->
              (* translate the type of the external function v *)
              let opt' =
                List.map
                  (fun v ->
                   v.vname, v.vtype, [])
                  (List.rev globals)
                @ Option.fold [] id opt
                |> Option.some
              in
              v.vtype <- TFun(ret, opt', is_vararg, attr)
           | _ -> ()
         end
      | _ -> ());
  maps

(*
let feature : featureDescr = 
  { fd_name = "SSA";
    fd_enabled = ref true;
    fd_description = "transformation to SSA form";
    fd_extraopt = [];
    fd_doit = do_ssa_feature;
    fd_post_check = true;
  }
*)
