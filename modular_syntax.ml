open Util
open Syntax
open Term_util
open Type


let debug () = List.mem "Modular_syntax" !Flag.debug_module


type program =
  {fun_typ_env : Ref_type.Env.t;
   fun_def_env : (id * (id list * typed_term)) list}

let print_typ_env = Ref_type.Env.print
let print_def_env fm def = List.print (Pair.print Id.print Print.term) fm @@ List.map (Pair.map_snd @@ Fun.uncurry make_funs) def

let print_prog fm {fun_typ_env; fun_def_env} =
  Format.fprintf fm "@[";
  Format.fprintf fm "fun_typ_env: %a@\n" print_typ_env fun_typ_env;
  Format.fprintf fm "fun_def_env: %a@\n" print_def_env fun_def_env;
  Format.fprintf fm "@]"

let rec is_atom t =
  match t.desc with
  | App({desc=Const(RandValue _)}, [_]) -> true
  | App({desc=Event(_, _)}, [_]) -> true
  | BinOp(_, t1, t2) -> is_atom t1 && is_atom t2
  | _ -> false

let normalize t =
  let dbg = 0=0 && !!debug in
  t
  |@dbg&> Format.printf "NORMALIZE: %a@.@." Print.term
  |> Trans.short_circuit_eval
  |@dbg&> Format.printf "NORMALIZE: %a@.@." Print.term
  |> Trans.normalize_let ~is_atom:is_atom
  |@dbg&> Format.printf "NORMALIZE: %a@.@." Print.term
  |> Trans.flatten_let
  |@dbg&> Format.printf "NORMALIZE: %a@.@." Print.term
  |> Trans.remove_no_effect_trywith
  |@dbg&> Format.printf "NORMALIZE: %a@.@." Print.term
  |> fixed_point ~eq:same_term
       (Trans.inline_var
        |- Trans.inline_simple_exp
        |- Trans.bool_eta_reduce
        |- Trans.reconstruct)
