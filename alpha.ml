
open Util
open Format
open Syntax



let free_map : ((string*int)*ident) list ref = ref []
let free : ident list ref = ref []


let key x = x.name, x.id

let rec alpha_pat = function
    PVar x ->
      let x' = new_var_id x in
        PVar x', [key x,x']
  | PConst c -> PConst c, []
  | PConstruct(name,pats) ->
      let aux pat (pats,map) =
        let pat',map' = alpha_pat pat in
          pat'::pats, map'@@map
      in
      let pats',map = List.fold_right aux pats ([],[]) in
        PConstruct(name,pats'), map
  | PNil -> PNil, []
  | PCons(p1,p2) ->
      let p1',map1 = alpha_pat p1 in
      let p2',map2 = alpha_pat p2 in
        PCons(p1',p2'), map1@@map2
  | PRecord(b,pats) ->
      let aux (i,(f,s,pat)) (pats,map) =
        let pat',map' = alpha_pat pat in
          (i,(f,s,pat'))::pats, map'@@map
      in
      let pats',map = List.fold_right aux pats ([],[]) in
        PRecord(b,pats'), map
  | POr(p1,p2) ->
      let p1',map1 = alpha_pat p1 in
      let p2',map2 = alpha_pat p2 in
        POr(p1',p2'), map1@@map2

let rec alpha map = function
    Unit -> Unit
  | True -> True
  | False -> False
  | Unknown -> Unknown
  | Int n -> Int n
  | NInt x ->
      let x' = new_var_id x in
        NInt x'
  | RandInt None -> RandInt None
  | RandInt (Some t) -> RandInt (Some (alpha map t))
  | Var x when is_external x -> Var x
  | Var x ->
      begin
        try
          Var{(List.assoc (key x) map) with typ=x.typ}
        with Not_found ->
          try
            Var (List.assoc (key x) !free_map)
          with Not_found ->
            let x' = new_var_id x in
              free_map := (key x,x')::!free_map;
              Var x'
      end
  | Fun(x, t) ->
      let x' = new_var_id x in
      let t' = alpha ((key x,x')::map) t in
        Fun(x', t')
  | App(t, ts) ->
      let t' = alpha map t in
      let ts' = List.map (alpha map) ts in
        App(t', ts')
  | If(t1, t2, t3) ->
      let t1' = alpha map t1 in
      let t2' = alpha map t2 in
      let t3' = alpha map t3 in
        If(t1', t2', t3')
  | Branch(t1, t2) ->
      let t1' = alpha map t1 in
      let t2' = alpha map t2 in
        Branch(t1', t2')
  | Let(flag, f, xs, t1, t2) ->
      if flag = Nonrecursive
      then
        let f' = new_var_id f in
        let map', xs' =
          let g y (map, ys) =
            let y' = new_var_id y in
              ((key y,y')::map, y'::ys)
          in
            List.fold_right g xs (map,[])
        in
        let t1' = alpha map' t1 in
        let t2' = alpha ((key f,f')::map) t2 in
          Let(flag, f', xs', t1', t2')
      else
        let f' = new_var_id f in
        let map', xs' =
          let g y (map, ys) =
            let y' = new_var_id y in
              ((key y,y')::map, y'::ys)
          in
            List.fold_right g xs (map,[])
        in
        let t1' = alpha ((key f,f')::map') t1 in
        let t2' = alpha ((key f,f')::map) t2 in
          Let(flag, f', xs', t1', t2')
  | BinOp(op, t1, t2) ->
      let t1' = alpha map t1 in
      let t2' = alpha map t2 in
        BinOp(op, t1', t2')
  | Not t ->
      let t' = alpha map t in
        Not t'
  | Label _ -> assert false
  | Fail -> Fail
  | Event s -> Event s
  | Record(b,fields) -> Record(b, List.map (fun (s,(f,t)) -> s,(f,alpha map t)) fields)
  | Proj(n,i,f,s,t) -> Proj(n,i,f,s,alpha map t)
  | SetField(n,i,f,s,t1,t2) -> SetField(n,i,f,s,alpha map t1,alpha map t2)
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(alpha map t1, alpha map t2)
  | Constr(s,ts) -> Constr(s, List.map (alpha map) ts)
  | Match(t1,t2,x,y,t3) ->
      let x' = new_var_id x in
      let y' = new_var_id y in
      let map' = (key x,x')::(key y,y')::map in
        Match(alpha map t1, alpha map t2, x', y', alpha map' t3)
  | Match_(t,pats) ->
      let aux (pat,cond,t) =
        let pat',map' = alpha_pat pat in
        let map'' = map'@@map in
          pat', apply_opt (alpha map'') cond, alpha map'' t
      in
        Match_(alpha map t, List.map aux pats)
  | TryWith(t,pats) ->
      let aux (pat,cond,t) =
        let pat',map' = alpha_pat pat in
        let map'' = map'@@map in
          pat', apply_opt (alpha map'') cond, alpha map'' t
      in
        TryWith(alpha map t, List.map aux pats)
  | Type_decl(decls,t) -> Type_decl(decls, alpha map t)
  | Exception(exc,typs,t) -> Exception(exc,typs,alpha map t)


let alpha t =
  set_counter 1;
  alpha [] t


