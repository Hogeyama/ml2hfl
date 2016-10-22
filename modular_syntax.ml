open Util
open Syntax
open Term_util
open Type


module Debug = Debug.Make(struct let check = make_debug_check "Modular_syntax" end)


type program =
  {fun_typ_env : Ref_type.env;
   fun_typ_neg_env : Ref_type.neg_env;
   fun_def_env : (id * (id list * term)) list;
   exn_decl : (string * typ list) list}

type ce = (id * int list) list
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

let print_ce fm ce = (List.print @@ Pair.print Id.print @@ List.print Format.pp_print_int) fm ce
let print_ce_set fm ce_set = (List.print @@ Pair.print Id.print print_ce) fm ce_set

let rec is_atom t =
  match t.desc with
  | App({desc=Const(RandValue _)}, [_]) -> true
  | App({desc=Event(_, _)}, [_]) -> true
  | BinOp(_, t1, t2) -> is_atom t1 && is_atom t2
  | _ -> false

let normalize t =
  let dbg = 0=0 in
  t
  |@dbg&> Debug.printf "NORMALIZE0: %a@.@." Print.term
  |> Trans.replace_bottom_def
  |@dbg&> Debug.printf "NORMALIZE1: %a@.@." Print.term
  |> Trans.short_circuit_eval
  |@dbg&> Debug.printf "NORMALIZE2: %a@.@." Print.term
  |> Trans.normalize_let ~is_atom:is_atom
  |@dbg&> Debug.printf "NORMALIZE3: %a@.@." Print.term
  |> Trans.flatten_let
  |@dbg&> Debug.printf "NORMALIZE4: %a@.@." Print.term
  |> Trans.remove_no_effect_trywith
  |@dbg&> Debug.printf "NORMALIZE5: %a@.@." Print.term
  |> fixed_point ~eq:same_term
       (Trans.inline_var
        |- Trans.inline_simple_exp
        |- Trans.bool_eta_reduce
        |- Trans.reconstruct
        |- Trans.elim_unused_let ~leave_last:true)

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
  let fs = List.unique ~cmp:Id.eq @@ aux [] [f] in
  if Id.mem f @@ get_fv @@ snd @@ Id.assoc f prog.fun_def_env then
    fs
  else
    List.filter_out (Id.same f) fs

let term_of_prog prog =
  prog.fun_def_env
  |> List.last
  |> fst
  |> make_var
  |> make_letrecs (List.map Triple.of_pair_r prog.fun_def_env)
