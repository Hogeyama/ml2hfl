open Util
open Syntax
open Term_util
open Type
open Modular_syntax

type t = {head : term; body : term list}
type horn_clauses = t list

type pred_var = int

let pred_var = Id.make (-1) "v" TInt
let pred_var_term = make_var pred_var
let make_pred_var p ts =
  let typs = List.map Syntax.typ ts in
  let typ = List.fold_right make_tfun typs TBool in
  Id.make p "P" typ
let is_pred_var x = Id.name x = "P"
let get_pred_id x = Id.id x
let get_pred_id_of_term t =
  match t.desc with
  | App({desc=Var x}, ts) -> assert (is_pred_var x); Some (get_pred_id x)
  | _ -> None

let print fm {head;body} =
  let pr_aux fm t =
    if true then
      Print.term fm t
    else (* For rcaml *)
      t
      |> Format.asprintf "%a" Print.term
      |> String.remove_char '_'
      |> String.remove_char '\''
      |> Format.fprintf fm "@[%s@]"
  in
  let pr fm t =
    match t.desc with
    | Var p when is_pred_var p -> Format.fprintf fm "%a()" pr_aux t
    | App(p, ts) -> Format.fprintf fm "@[%a(%a)@]" pr_aux p (print_list pr_aux ",") ts
    | _ -> pr_aux fm t
  in
  if head = false_term
  then Format.fprintf fm "@[?- %a.@]" (print_list pr ",@ ") body
  else Format.fprintf fm "@[<hov 4>%a :-@ %a.@]" pr head (print_list pr ",@ ") body
let print_horn_clauses fm hcs =
  Format.fprintf fm "@[%a@]" (print_list print "@\n") hcs

let of_pair_list xs = List.map (fun (body,head) -> {body;head}) xs

let map f {body;head} = {body = List.map f body; head = f head}
let flatten_map f {body;head} = {body = List.flatten_map f body; head = List.get @@ f head}

let get_pred_ids_hcs hcs =
  hcs
  |> List.flatten_map (fun {body;head} -> List.filter_map get_pred_id_of_term (head::body))
  |> List.unique

let decomp_pred_app t =
  match t.desc with
  | App({desc=Var p}, ts) when is_pred_var p -> Some (p, ts)
  | _ -> None

let inline need hcs =
  let hcs', env =
    let can_inline head hcs1 hcs2 =
      match decomp_pred_app head with
      | None -> false
      | Some (p,_) ->
          let aux {head} = not @@ Id.mem p @@ get_fv head in
          is_pred_var p &&
          not @@ List.mem (get_pred_id p) need &&
          List.for_all aux hcs1 &&
          List.for_all aux hcs2
    in
    let rec aux acc hcs_done hcs =
      match hcs with
      | [] -> hcs_done, acc
      | {head;body}::hcs' when can_inline head hcs_done hcs' ->
          let p,ts = Option.get @@ decomp_pred_app head in
          aux ((p,(ts,body))::acc) hcs_done hcs'
      | hc::hcs' -> aux acc (hc::hcs_done) hcs'
    in
    aux [] [] hcs
  in
  let replace env t =
    try
      let p,ts = Option.get @@ decomp_pred_app t in
      let ts',body = Id.assoc p env in
      List.map2 make_eq ts ts' @ body
    with _ -> [t]
  in
  let env' =
    let rec aux env_acc env_rest =
      match env_rest with
      | [] -> env_acc
      | (p,(ts,body))::env_rest' when List.exists (Id.mem_assoc -$- env) @@ List.flatten_map get_fv body ->
          let body' = List.flatten_map (replace (env_rest@@@env_acc)) body in
          aux env_acc ((p,(ts,body'))::env_rest')
      | x::env_rest' ->
          aux (x::env_acc) env_rest'
    in
    aux [] env
  in
  List.map (flatten_map @@ replace env') hcs'
