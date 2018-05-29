open Util
open Syntax
open Type
open Term_util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type atom = term
type elem = {head:atom; body:atom list}
type t = elem list
type pvar = id

let print_atom fm a = Print.term fm a
let print_elem fm {head;body} = Format.fprintf fm "@[<hov 2>%a |=@ %a@]" (List.print print_atom) body print_atom head
let print fm (constrs:t) = List.print print_elem fm constrs

let of_term_list (constrs : (term list * term) list) =
  List.map (fun (body,head) -> {body; head}) constrs

let to_term_list (constrs : t) =
  List.map (fun {body;head} -> body, head) constrs

let preds_of t = t |> get_fv |> List.filter Id.is_predicate

let is_app_of (a:atom) (p:pvar) =
  match a.desc with
  | App({desc=Var f}, _) -> Id.(p = f)
  | _ -> false

let decomp_app a =
  match a.desc with
  | App({desc=Var f}, _) -> Some f
  | _ -> None

let map_head f {head; body} = {head=f head; body}
let map_body f {head; body} = {head; body=f body}
let map f {head;body} = {head=f head; body=List.map f body}

let replace_with_true (deps, ps, constrs, sol) ps_true =
  let deps' = List.filter_out (fun (p1,p2) -> List.exists (fun p -> Id.(p = p1 || p = p2)) ps_true) deps in
  let sol' = List.map (fun p -> p, Term.true_) ps_true @ sol in
  let ps' = List.Set.diff ps ps_true in
  let constrs' =
    constrs
    |> List.filter_out (fun {head} -> List.exists (is_app_of head) ps_true)
    |> List.map (map_body @@ List.filter_out (fun a -> List.exists (is_app_of a) ps_true))
  in
  Debug.printf "SIMPLIFIED: %a@.@.@." print constrs';
  deps', ps', constrs', sol'

let dummy_pred = Id.new_predicate Ty.int

let normalize constrs =
  unsupported "CHC.normalized"

let rec once acc fv =
  match fv with
  | [] -> acc
  | [x] -> x::acc
  | x1::x2::fv' when Id.(x1 <> x2) -> once (x1::acc) (x2::fv')
  | x::fv' ->
      fv'
      |> List.drop_while (Id.(=) x)
      |> once acc
let once xs = once [] xs

let _aux {head;body} =
  let use_once =
    head::body
    |> List.flatten_map (get_fv ~eq:(fun _ _ -> false))
    |@> Debug.printf "fvs: %a@." Print.(list id)
    |> List.filter_out Id.is_predicate
    |> List.sort Id.compare
    |@> Debug.printf "sorted: %a@." Print.(list id)
    |> once
  in
  if use_once = [] then
    {head; body}
  else
    let consntrained t =
      match t.desc with
      | App _ -> true
      | _ -> List.Set.disjoint ~eq:Id.eq use_once (get_fv t)
    in
    let body' = List.filter consntrained body in
    if List.length body <> List.length body' then
      begin
        Debug.printf "{head;body}:  %a@.@." print_elem {head;body};
        Debug.printf "{head;body'}: %a@." print_elem {head;body=body'};
        Debug.printf "use_once: %a@.@." Print.(list id) use_once;
      end;
    {head; body=body'}

let is_base_const t =
  match t.desc with
  | Const _ -> is_base_typ t.typ
  | _ -> false

let is_simple_expr t =
  is_simple_bexp t || is_simple_aexp t || is_base_const t

let rec simplify_trivial need_rerun body1 body2 head head_fv =
  match body2 with
  | [] ->
      if head = true_term then
        None
      else
        begin
          match head.desc with
          | BinOp(Eq, t1, t2) when is_simple_expr t1 && same_term t1 t2 -> None
          | _ -> Some (need_rerun, {body=body1; head})
        end
  | p::body2' ->
      match p.desc with
      | Const True -> simplify_trivial need_rerun body1 body2' head head_fv
      | Const False -> None
      | BinOp(Eq, t1, t2) when is_simple_expr t1 && same_term t1 t2 -> simplify_trivial true body1 body2' head head_fv
      | BinOp(And, p1, p2) -> simplify_trivial need_rerun body1 (p1::p2::body2') head head_fv
      | BinOp(Eq, {desc=Var x}, t) when not @@ Id.mem x head_fv || List.Set.disjoint (get_fv t) head_fv || not @@ is_app head ->
          let sb = subst x t in
          let sbs = List.map sb in
          let head' = sb head in
          simplify_trivial true [] (sbs body1 @ sbs body2') head' (get_fv head')
      | _ -> simplify_trivial need_rerun (p::body1) body2' head head_fv
let simplify_trivial {body;head} = simplify_trivial false [] body head (get_fv head)

(* Trivial simplification *)
let simplify1 (deps,ps,constrs,sol) =
  let need_rerun,constrs' =
    let aux constr (b,acc) =
      match simplify_trivial constr with
      | None -> true, acc
      | Some(need_rerun,constr') -> b||need_rerun, constr'::acc
    in
    List.fold_right aux constrs (false,[])
  in
  Some (need_rerun, (deps,ps,constrs',sol))

(* Remove constraint whose body is empty *)
let simplify2 (deps,ps,constrs,sol as x) =
  let ps' = List.filter_map (fun {head;body} -> if body = [] then decomp_app head else None) constrs in
  if ps' = [] then
    None
  else
    let () = Debug.printf "REMOVE2: %a@." Print.(list id) ps' in
    Some (true, replace_with_true x ps')

(* Remove predicates which do not occur in a body *)
let simplify3 (deps,ps,constrs,sol as x) =
  let ps1,ps2 = List.partition (fun p -> List.exists (fun (p1,_) -> Id.(p = p1)) deps) ps in
  if ps2 = [] then
    None
  else
    let () = Debug.printf "deps: %a@." Print.(list (pair id id)) deps in
    let () = Debug.printf "REMOVE1: %a@." Print.(list id) ps2 in
    Some (true, replace_with_true x ps2)


type data = (pvar * pvar) list * pvar list * elem list * (pvar * Syntax.term) list
let simplifiers : (data -> (bool * data) option) list = [simplify3; simplify2; simplify1]

let simplify ?(normalized=false) (constrs:t) =
  let constrs = if normalized then constrs else normalize constrs in
  Debug.printf "dummy_pred: %a@." Id.print dummy_pred;
  Debug.printf "INPUT: %a@." print constrs;
  let constrs =
    constrs
    |> List.filter_out (fun {head} -> head.desc = Const True)
    |> List.map (map_body @@ List.filter_out (fun t -> t.desc = Const True))
    |*> List.map _aux
  in
  let deps : (pvar * pvar) list =
    List.flatten_map (fun {body;head} -> Combination.product (List.flatten_map preds_of body) (dummy_pred::preds_of head)) constrs
  in
  let ps =
    deps
    |> List.flatten_map Pair.to_list
    |> List.unique ~eq:Id.eq
    |> List.filter_out (Id.(=) dummy_pred)
  in
  let rec loop orig rest x =
    match rest with
    | [] -> x
    | f::rest' ->
        match f x with
        | None -> loop orig rest' x
        | Some(true, x') -> loop orig orig x'
        | Some(false, x') -> loop orig rest' x'
  in
  let loop orig x = loop orig orig x in
  let deps',ps',constrs',sol = loop simplifiers (deps, ps, constrs, []) in
  Debug.printf "REMOVED: %a@." Print.(list id) @@ List.map fst sol;
  Debug.printf "SIMPLIFIED: %a@." print constrs';
  sol, constrs'
