open Util
open Combinator
open Syntax

(* e の中の(EVar (Var _))を table に対応したものに置き換える *)
let subst e table =
  let rec loop = function
    | App (e1, e2) -> App (loop e1, loop e2)
    | Circle (e1, e2) -> Circle (loop e1, loop e2)
    | Plus es -> Plus (List.map loop es)
    | InvTri es -> InvTri (List.map loop es)
    | Tuple es -> Tuple (List.map loop es)
    | Tag (i, e') -> Tag (i, loop e')
    | Lambda (e1, e2) -> Lambda (e1, loop e2)
    | Case (e', pts) -> Case (loop e', List.map (fun (p, t) -> (p, loop t)) pts)
    | Banana (e', f) -> Banana (loop e', f)
    | EVar v as var -> (try List.assoc var table with Not_found -> var)
    | other -> other
  in loop e

(*
  expr * expr -> (expr * expr) list
 パターン * 項 -> (EVar (Var v) * 置き換え後のexpr) list
*)
let rec pmatch p t =
  match p with
  | App (pe1, pe2) ->
    begin
      match t with
      | App (te1, te2) -> pmatch pe1 te1 @ pmatch pe2 te2
      | _ -> failwith "Match_failure1"
    end
  | Circle (pe1, pe2) ->
    begin
      match t with
      | Circle (te1, te2) -> pmatch pe1 te1 @ pmatch pe2 te2
      | _ -> failwith "Match_failure2"
    end
  | Plus ps ->
    begin
      match t with
      | Plus ts -> List.concat (List.map2 pmatch ps ts)
      | _ -> failwith "Match_failure3"
    end
  | InvTri ps ->
    begin
      match t with
      | InvTri ts -> List.concat (List.map2 pmatch ps ts)
      | _ -> failwith "Match_failure4"
    end
  | Tuple ps ->
    begin
      match t with
      | Tuple ts -> List.concat (List.map2 pmatch ps ts)
      | _ -> failwith "Match_failure5"
    end
  | Tag (pi, pe) ->
    begin
      match t with
      | Tag (ti, te) when pi = ti -> pmatch pe te
      | _ -> failwith "Match_failure6"
    end
  | Const pc ->
    begin
      match t with
      | Const tc when pc = tc -> []
      | _ -> failwith "Match_failure7"
    end
  | EVar pv ->
    begin
      match pv with
      | Var s -> [(p, t)]
      | x -> 
	begin
	  match t with
	  | EVar tv when x = tv -> []
	  | _ -> failwith "Match_failure8"
	end
    end
  | _ -> failwith "Pattern is wrong"

    
let rec reduct = function
  | App (e1, e2) ->
    begin
      match (reduct e1, reduct e2) with
      | (Lambda (e1_1', e1_2'), e2') ->
	let table = pmatch e1_1' e2' in
	reduct (subst e1_2' table)
      | (Id, e2') -> e2'
      | (e1', Id) -> e1'
      | (e1', e2') -> App (e1', e2')
    end
  | Circle (e1, e2) ->
    begin
      match (reduct e1, reduct e2) with
      | (InvTri e1s, Plus e2s) when List.length e1s = List.length e2s ->
	InvTri (List.map reduct (List.map2 (fun x y -> Circle (x, y)) e1s e2s))
      | (e1', InvTri e2s) ->
	InvTri (List.map reduct (List.map (fun x -> Circle (e1', x)) e2s))
      | (Lambda (e1_1', e1_2'), Lambda (e2_1', e2_2')) ->
	let table = pmatch e1_1' e2_2' in
	reduct (Lambda (e2_1', subst e1_2' table))
      | (Id, e2') -> e2'
      | (e1', Id) -> e1'
      | (e1', e2') -> Circle (e1', e2')
    end
  | Plus es ->
    Plus (List.map reduct es)
  | InvTri es ->
    (try InvTri (List.map reduct es) with | Failure "Match_failure4" -> InvTri es)
  | Tag (i, e) ->
    Tag (i, reduct e)
  | Lambda (e1, e2) ->
    let e2' = reduct e2 in
    if e1 = e2' then Id
    else Lambda (e1, e2')
  | Tuple es ->
    Tuple (List.map reduct es)
  | e -> e
