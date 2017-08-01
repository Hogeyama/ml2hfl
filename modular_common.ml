open Util
open Syntax
open Term_util
open Type


module Debug = Debug.Make(struct let check = make_debug_check __MODULE__ end)


type program =
  {fun_typ_env : Ref_type.env;
   fun_typ_neg_env : Ref_type.neg_env;
   fun_def_env : (id * (id list * term)) list;
   exn_decl : (string * typ list) list}

type label = int
type ce = (label * bool) list
type ce_set = (id * ce) list

let print_typ_env = Ref_type.Env.print
let print_typ_neg_env = Ref_type.NegEnv.print
let print_def_env fm def = List.print (Pair.print Id.print Print.term) fm @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) def

let print_prog fm {fun_typ_env; fun_typ_neg_env; fun_def_env} =
  Format.fprintf fm "@[";
  Format.fprintf fm "fun_typ_env: %a@\n" print_typ_env fun_typ_env;
  Format.fprintf fm "fun_typ_neg_env: %a@\n" print_typ_neg_env fun_typ_neg_env;
  Format.fprintf fm "fun_def_env: %a@\n" print_def_env fun_def_env;
  Format.fprintf fm "@]"

let print_ce_aux fm (l,b) = Format.pp_print_int fm (if b then l else -l)
let print_ce fm ce = (List.print print_ce_aux) fm ce
let print_ce_set fm ce_set = (List.print @@ Pair.print Id.print print_ce) fm ce_set

let rec is_atom t =
  match t.desc with
  | App({desc=Const(RandValue _)}, [_]) -> true
  | App({desc=Event(_, _)}, [_]) -> true
  | BinOp(_, t1, t2) -> is_atom t1 && is_atom t2
  | _ -> false


let rec is_simple_term t =
  match t.desc with
  | Const _ -> true
  | Var _ -> true
  | Tuple ts -> List.for_all is_simple_term ts
  | _ -> is_randint_unit t || is_randbool_unit t

let inline_simple_fun = make_trans2 ()
let inline_simple_fun_desc env desc =
  match desc with
  | Let(bindings, t) ->
      let bindings' = List.map (Triple.map_trd @@ inline_simple_fun.tr2_term env) bindings in
      let env' = List.filter (fun (_,xs,t) -> xs <> [] && is_simple_term t) bindings' @ env in
      Let(bindings', inline_simple_fun.tr2_term env' t)
  | App({desc=Var f}, ts) when List.exists (fun (g,xs,_) -> Id.same g f && List.length xs = List.length ts) env && List.for_all is_simple_term ts ->
      let _,xs,t = List.find (Triple.fst |- Id.same f) env in
      (List.fold_right2 subst xs ts t).desc
  | _ -> inline_simple_fun.tr2_desc_rec env desc
let () = inline_simple_fun.tr2_desc <- inline_simple_fun_desc
let inline_simple_fun t = inline_simple_fun.tr2_term [] t

let normalize add_id t =
  t
  |@> Debug.printf "NORMALIZE0: %a@.@." Print.term
  |> Trans.replace_bottom_def
  |@> Debug.printf "NORMALIZE1: %a@.@." Print.term
  |> Trans.short_circuit_eval
  |@> Debug.printf "NORMALIZE2: %a@.@." Print.term
  |> Trans.normalize_let ~is_atom:is_atom
  |@> Debug.printf "NORMALIZE3: %a@.@." Print.term
  |> Trans.flatten_let
  |@> Debug.printf "NORMALIZE4: %a@.@." Print.term
  |> Trans.remove_no_effect_trywith
  |@> Debug.printf "NORMALIZE5: %a@.@." Print.term
  |> inline_simple_fun
  |@> Debug.printf "NORMALIZE6: %a@.@." Print.term
  |> fixed_point ~eq:same_term
       (Trans.inline_var
        |- Trans.inline_simple_exp
        |- Trans.bool_eta_reduce
        |- Trans.reconstruct
        |- Trans.elim_unused_let ~leave_last:true)
  |@> Debug.printf "NORMALIZE7: %a@.@." Print.term
  |> Trans.add_id_if (function {desc=If _} -> add_id | _ -> false)
  |> snd
  |> Trans.alpha_rename

(* `used_by f prog` returns top-level functions used (directly/indirectly) by f *)
let used_by f prog =
  let rec aux acc rest =
    match rest with
    | [] -> acc
    | f::rest' when Id.mem f acc -> aux acc rest'
    | f::rest' ->
        let xs,t = Id.assoc f prog.fun_def_env in
        aux (f::acc) (List.Set.diff ~eq:Id.eq (get_fv t) (f::xs) @ rest')
  in
  let fs = List.unique ~eq:Id.eq @@ aux [] [f] in
  if Id.mem f @@ get_fv @@ snd @@ Id.assoc f prog.fun_def_env then
    fs
  else
    List.filter_out (Id.same f) fs

let term_of_prog prog =
  prog.fun_def_env
  |> List.last
  |> fst
  |> make_var
  |> make_lets (List.map Triple.of_pair_r prog.fun_def_env)


let take_funs_of_depth env f depth =
  let rec aux acc fs depth =
    if depth <= 0 then
      acc
    else
      let fs' =
        fs
        |> List.flatten_map (fun g -> let xs,t = Id.assoc g env in List.Set.diff ~eq:Id.eq (get_fv t) (g::xs))
        |> List.unique ~eq:Id.eq
      in
      aux (fs@acc) fs' (depth-1)
  in
  aux [] [f] depth
