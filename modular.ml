open Util
open Syntax
open Term_util
open Type

let debug () = List.mem "Modular" !Flag.debug_module

let remove_ext_def = make_trans2 ()

let remove_ext_def_desc ext desc =
  match desc with
  | Let(flag, bindings, t) ->
      let bindings' = List.filter_out (fun (f,_,_) -> Id.mem f ext) bindings in
      let t' = remove_ext_def.tr2_term ext t in
      if bindings' = [] then t'.desc else Let(flag, bindings', t')
  | _ -> remove_ext_def.tr2_desc_rec ext desc

let () = remove_ext_def.tr2_desc <- remove_ext_def_desc
let remove_ext_def = remove_ext_def.tr2_term

let divide spec t ref_env =
  if !!debug then Format.printf "PROGRAM: %a@." Print.term t;
  if !!debug then Format.printf "ORIG: %a@." (List.print Print.id) @@ get_top_funs t;
  let ext = List.map fst ref_env in
  let t_main = remove_ext_def ext t in
  if !!debug then Format.printf "MAIN: %a@." (List.print Print.id) @@ get_top_funs t_main;
  let make_spec f =
    let ref_env,ext_ref_env = List.partition (Id.same f -| fst) ref_env in
    let aux (_,typ) =
      if not @@ Type.same_shape (Id.typ f) (Ref_type.to_simple typ) then
        begin
          Format.printf "VAR: %a@." Id.print f;
          Format.printf "  Prog: %a@." Print.typ @@ Id.typ f;
          Format.printf "  Spec: %a@." Ref_type.print typ;
          fatal @@ Format.sprintf "Type of %s in the specification is wrong?" @@ Id.name f
        end
    in
    List.iter aux ref_env;
    let spec' = {spec with Spec.ref_env; Spec.ext_ref_env = ext_ref_env @ spec.Spec.ext_ref_env} in
    if !!debug then Format.printf "SUB[%a]: %a@." Print.id f Spec.print spec';
    spec'
  in
  let targets = List.map (fun f -> Id.to_string f, make_spec f, t) ext in
  if !!debug then Format.printf "MAIN: %a@." Print.term t_main;
  ("MAIN", make_spec (Id.new_var ~name:"MAIN" TUnit), t_main)::targets


let main orig spec parsed =
  let verify (s,spec,t) =
    if !!debug then Format.printf "Start verification of %s:@.%a@." s Spec.print spec;
    s, Main_loop.run orig [] ~spec t
  in
  Spec.get_ref_env spec parsed
  |@(not !Flag.only_result)&> Spec.print_ref_env Format.std_formatter
  |> divide spec parsed
  |> List.map verify
  |@> Format.printf "RESULT: %a@." (List.print @@ Pair.print Format.pp_print_string Format.pp_print_bool)
  |> List.for_all snd





(************************************************************************************************************)
(************************************************************************************************************)
(************************************************************************************************************)




type constr =
  | True
  | False
  | And of constr * constr
  | Imply of typed_term * constr
  | Sub of typ_tmp * typ_tmp
and typ_tmp =
  | Var of fun_id
  | Arg of int * typ_tmp
  | App of typ_tmp * typed_term list
  | Singleton of typed_term
  | Const of Ref_type.t
and fun_id = id * (tid * int option * int option) option
and tid = int


type type_template =
  | Base of pred_var
  | Fun of id * type_template * type_template
  | Inter of type_template list
and pred_var = int


let rec print_constr fm = function
  | True -> Format.fprintf fm "true"
  | False -> Format.fprintf fm "false"
  | And(c1, c2) -> Format.fprintf fm "(@[%a@ /\\@ %a@])" print_constr c1 print_constr c2
  | Imply(t, c) -> Format.fprintf fm "(@[%a@ =>@ %a@])" Print.term t print_constr c
  | Sub(typ1, typ2) -> Format.fprintf fm "(@[%a@ <:@ %a@])" print_typ_tmp typ1 print_typ_tmp typ2
and print_typ_tmp fm = function
  | Var f -> print_fun_id fm f
  | Arg(i, typ) -> Format.fprintf fm "#%d(%a)" i print_typ_tmp typ
  | App(typ, ts) -> Format.fprintf fm "%a%a" print_typ_tmp typ (List.print Print.term) ts
  | Singleton t -> Format.fprintf fm "{%a}" Print.term t
  | Const typ -> Format.fprintf fm "%a" Ref_type.print typ
and print_fun_id fm (f, tidn) =
  match tidn with
  | None -> Format.fprintf fm "%a" Id.print f
  | Some (tid, None, None) -> Format.fprintf fm "%a:%a" Id.print f print_tid tid
  | Some (tid, None, Some n) -> Format.fprintf fm "%a:%a*%d" Id.print f print_tid tid n
  | Some (tid, Some i, None) -> Format.fprintf fm "%a:%a-%d" Id.print f print_tid tid i
  | Some (tid, Some _, Some _) -> assert false
and print_tid = Format.pp_print_int

let rec print_template fm = function
  | Base p when p<0 -> Format.fprintf fm "*"
  | Base p -> Format.fprintf fm "P%d" p
  | Fun(x, tmp1, tmp2) -> Format.fprintf fm "@[(%a:%a ->@ %a)@]" Id.print x print_template tmp1 print_template tmp2
  | Inter [] -> Format.fprintf fm "T"
  | Inter tmps -> Format.fprintf fm "@[%a@]" (print_list print_template " /\\@ ") tmps

let _And c1 c2 =
  if c1 = True then
    c2
  else if c2 = True then
    c1
  else
    And(c1, c2)
let _Ands constrs = List.fold_right _And constrs True
let _App typ map = if map=[] then typ else App(typ, map)


let rec add_bind f map typ =
  match typ with
  | Var(g, _) -> if Id.same f g then _App typ map else typ
  | Arg(i, typ) -> Arg(i, add_bind f map typ)
  | App(typ1, map1) -> _App (add_bind f map typ1) map1
  | Singleton _ -> typ
  | Const _ -> typ
let add_bind f map typ =
  let map' = List.filter_out (is_fun_typ -| Id.typ) map in
  add_bind f (List.map make_var map') typ

let typ_of t =
  match t.desc with
  | Syntax.Var f -> Var (f, None)
  | _ -> Singleton t

let rec apply f = function
  | True -> True
  | False -> False
  | And(c1,c2) -> And(apply f c1, apply f c2)
  | Imply(t, c) -> Imply(t, apply f c)
  | Sub(typ1, typ2) -> Sub(f typ1, f typ2)

let rec generate_constraints tid_env (Rose_tree.Node(label, children)) =
  let open Comp_tree in
  let constr () = _Ands @@ List.map (generate_constraints tid_env) children in
  match label with
  | Term t ->
      begin
        match t.desc with
        | If _ -> !!constr
        | App(t1, ts) ->
            let f = Option.get @@ decomp_var t1 in
            let tid = get_id_option t1 in
            let constr1 =
              let n = if Option.exists (List.mem_assoc -$- tid_env) tid then None else Some (List.length children) in
              let fun_id = f, Option.map (fun tid -> tid, List.assoc_option tid tid_env, n) tid in
              let _,_,typs =
                let aux (i,map,typs) t =
                  i+1,
                  (if is_fun_typ t.typ then map else map@[t]),
                  typs@[App(Arg(i, Var fun_id), map)]
                in
                List.fold_left aux (0,[],[]) ts
              in
              _Ands @@ List.map2 (fun typ t -> Sub(typ_of t, typ)) typs ts
            in
            let constr2 =
              if Option.for_all (List.mem_assoc -$- tid_env) tid then
                !!constr
              else
                let tid = Option.get tid in
                _Ands @@ List.mapi (fun i child -> generate_constraints ((tid,i)::tid_env) child) children
            in
            And(constr1, constr2)
        | _ -> assert false
      end
  | Bind map ->
      let add typ = List.fold_left (fun typ (x,env,_) -> add_bind x env typ) typ map in
      Imply(List.fold_right make_and (List.map (fun (x,_,v) -> make_eq (make_var x) v) map) true_term, apply add !!constr)
  | Assume t -> Imply(t, !!constr)
  | Fail ->
      assert (children = []);
      False
let generate_constraints comp_tree = generate_constraints [] comp_tree


let merge_template ts = List.flatten ts
let rec make_template fun_env cnt (Rose_tree.Node(label, children)) =
  let r=
  let mk_templates env = merge_template @@ List.map (make_template env cnt) children in
  let open Comp_tree in
  match label with
  | Term {desc=If _} -> mk_templates fun_env
  | Term {desc=App({desc=Var f} as t1, ts)} when not @@ List.mem (f, get_id_option t1) fun_env ->
      let f = Option.get @@ decomp_var t1 in
      let fun_env' = (f, get_id_option t1)::fun_env in
      let templates = mk_templates fun_env' in
      Format.printf "APP: %a@." print_label label;
      Format.printf "  TEMPLATE: %a@." (List.print @@ Pair.print Id.print print_template) templates;
      let aux (Rose_tree.Node(label', children')) =
        match label', children' with
        | Bind map, [child]  ->
(*
            let maps =
              let aux' = function
                | Rose_tree.Node(Term {desc=App({desc=Var g},_)}, children) ->
                    if Id.same f g
                    then Some (List.flatten_map (function Rose_tree.Node(Bind map, _) -> List.map Triple.fst map | _ -> assert false) children)
                    else None
                | _ -> None
              in
              Rose_tree.filter_map_subtree aux' child
            in
            let maps' =
              if Option.is_none @@ get_id_option t1 then
                List.map new_var_of_term ts::maps
              else
                maps
            in
            if maps' = [] then
              Inter []
            else
              let argss = List.fold_right (List.map2 List.cons) maps' @@ List.map (Fun.const []) @@ List.hd maps' in
              let aux' args =
                if is_fun_typ @@ Id.typ @@ List.hd args then
                  Inter (List.map (Id.assoc -$- templates) args)
                else
                  Base (Counter.gen cnt)
              in
              let r = List.fold_right (fun args tmp  -> Fun(Id.new_var_id @@ List.hd args, aux' args, tmp)) argss (Base(-1)) in
              Format.printf "  argss: %a@." (List.print @@ List.print Id.print) argss;
              Format.printf "  typ: %a@." print_template r;
              r
 *)
            let aux' x =
              if is_fun_typ @@ Id.typ x then
                Inter (List.map (fun (g,_,_) -> Id.assoc g templates) map)
              else
                Base (Counter.gen cnt)
            in
            let r = List.fold_right2 (fun t (x,_,_) tmp  -> Fun(new_var_of_term t, aux' x, tmp)) ts map (Base(-1)) in
            Format.printf "  map: %a@." (List.print Id.print) @@ List.map Triple.fst map;
            Format.printf "  typ: %a@." print_template r;
            r
        | _ ->
            Format.printf "%a@." print_label label;
            assert false
      in
      (f, Inter (List.map aux children))::templates
  | Term {desc=App _} -> mk_templates fun_env
  | Term _ -> assert false
  | Bind _ -> mk_templates fun_env
  | Assume _ -> mk_templates fun_env
  | Fail -> []
  in
  Format.printf "  %a@." Comp_tree.print_label label;
  Format.printf "  LEN: %d@." (List.length r); r
let make_template t = make_template [] (Counter.create()) t


let normalize = make_trans ()
let normalize_term t =
  let t' = normalize.tr_term_rec t in
  match t'.desc with
  | App(t1, ts) ->
      let ts',binds =
        let aux t2 =
          match t2.desc with
          | Fun _ ->
              let x = new_var_of_term t2 in
              make_var x, Some (x, [], t2)
          | _ -> t2, None
        in
        let ts',binds = List.split_map aux ts in
        ts', List.filter_map Fun.id binds
      in
      make_lets binds {t' with desc=App(t1, ts')}
  | _ -> t'
let () = normalize.tr_term <- normalize_term
let normalize = normalize.tr_term


let infer spec parsed ce_set =
  let normalized =
    if true
    then normalize parsed
    else Trans.inline_var @@ Trans.flatten_let @@ Trans.normalize_let parsed
  in
  Format.printf "INPUT: %a@." Print.term normalized;
  let fbindings,main = decomp_prog normalized in
  assert (main.desc = Const Unit);
  List.iter (fun (flag,bindings) -> if flag=Recursive then assert (List.length bindings=1)) fbindings;
  let fun_env = List.flatten_map (snd |- List.map Triple.to_pair_r) fbindings in
  let ce_set =
    let aux =
      match !Flag.filename with
      | "test.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "sum" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | "test2.ml" -> (fun (f,_) -> match Id.name f with "main" -> [f, [1]] | "fsum" -> [f, [0]; f, [1;0]] | "double" -> [f, [0]; f, [1;0]] | _ -> assert false)
      | _ -> assert false
    in
    List.flatten @@ List.map aux fun_env
  in
  let main = Option.get @@ get_last_definition normalized in
  let comp_tree = Comp_tree.from_program fun_env ce_set main in
  Format.printf "%a@." print_constr @@ generate_constraints comp_tree;
(if !!debug then
  let tmp = make_template comp_tree in
  Format.printf "%a@." (List.print @@ Pair.print Id.print print_template) tmp);
  comp_tree
