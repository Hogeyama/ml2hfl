open Util
open CEGAR_syntax
open CEGAR_type
open CEGAR_util
open CEGAR_abst_util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

let abst_arg x ty =
  if false then Debug.printf "abst_arg: %a, %a;;@." CEGAR_print.var x CEGAR_print.typ ty;
  match decomp_base ty with
  | None -> [x]
  | Some(_,ps) ->
      match ps (Var x) with
      | [] -> []
      | [_] -> [x]
      | ps -> List.mapi (fun i _ -> add_name x @@ Format.sprintf "_%d" @@ i+1) ps

let make_pts x ty =
  let xs = abst_arg x ty in
  let ps =
    match decomp_base ty with
    | None -> [Const True] (* to match the length of ps and xs *)
    | Some(_, ps) -> ps (Var x)
  in
  List.filter (fun (p,_) -> p <> Const True) @@ List.map2 (fun p x -> p, Var x) ps xs

let rec decomp_typ_var typ xs =
  match xs with
  | [] -> typ, []
  | x::xs' ->
      let typ1,typ2 = match typ with TFun(typ1,typ2) -> typ1,typ2 (Var x) | _ -> assert false in
      let typ',env' = decomp_typ_var typ2 xs' in
      typ', (x,typ1)::env'

let rec decomp_typ_term ts typ =
  match ts,typ with
  | [], _ when is_typ_result typ -> []
  | t2::ts', TFun(typ1,typ2) ->
      typ1 :: decomp_typ_term ts' (typ2 t2)
  | _,typ ->
      assert false

let rec beta_reduce_term = function
  | Const c -> Const c
  | Var x -> Var x
  | App(App(App(Const If, Const True), t2), _) -> beta_reduce_term t2
  | App(App(App(Const If, Const False), _), t3) -> beta_reduce_term t3
  | App(t1, t2) ->
      let t1' = beta_reduce_term t1 in
      begin
        match t1' with
        | Fun(x,_,t1') -> beta_reduce_term @@ subst x t2 t1'
        | _ -> App(t1', beta_reduce_term t2)
      end
  | Fun(x, typ, t) -> Fun(x, typ, beta_reduce_term t)
  | Let _ -> assert false
let beta_reduce_def def =
  {def with cond=beta_reduce_term def.cond; body=beta_reduce_term def.body}

let rec expand_non_rec {env;defs;main;info} =
  let non_rec = info.non_rec in
  let aux ({args; cond; body} as def) =
    let non_rec' = List.filter_out (fst |- List.mem -$- args) non_rec in
    {def with cond=subst_map non_rec' cond; body=subst_map non_rec' body}
  in
  let defs' =
    defs
    |> List.filter_out (fun def -> List.mem_assoc def.fn non_rec)
    |> List.map aux
    |> List.map beta_reduce_def
  in
  {env; defs=defs'; main; info}




