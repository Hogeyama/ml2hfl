
open Util
open Format
open Syntax



let new_id x =
  {x with id = new_int ()}



let free_map : (string*ident) list ref = ref []
let free : ident list ref = ref []


let rec alpha_pat = function
    PVar x ->
      let x' = new_id x in
        PVar x', [x.name,x']
  | PConst c -> PConst c, []
  | PTuple pats ->
      let aux pat (pats,map) =
        let pat',map' = alpha_pat pat in
          pat'::pats, map'@@map
      in
      let pats',map = List.fold_right aux pats ([],[]) in
        PTuple pats', map
  | PConstruct(name,pats) ->
      let aux pat (pats,map) =
        let pat',map' = alpha_pat pat in
          pat'::pats, map'@@map
      in
      let pats',map = List.fold_right aux pats ([],[]) in
        PConstruct(name,pats'), map
  | PRecord spats ->
      let aux (s,pat) (spats,map) =
        let pat',map' = alpha_pat pat in
          (s,pat')::spats, map'@@map
      in
      let spats',map = List.fold_right aux spats ([],[]) in
        PRecord spats', map
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
      let x' = new_id x in
        NInt x'
  | Var x ->
      begin
        try
          Var (List.assoc x.name map)
        with Not_found ->
          try
            Var (List.assoc x.name !free_map)
          with Not_found ->
            let x' = new_id x in
              free_map := (x.name,x')::!free_map;
              Var x'
      end
  | Fun(x, t) ->
      let x' = new_id x in
      let t' = alpha ((x.name,x')::map) t in
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
        let f' = new_id f in
        let map', xs' =
          let g y (map, ys) =
            let y' = new_id y in
              ((y.name,y')::map, y'::ys)
          in
            List.fold_right g xs (map,[])
        in
        let t1' = alpha map' t1 in
        let t2' = alpha ((f.name,f')::map) t2 in
          Let(flag, f', xs', t1', t2')
      else
        let f' = new_id f in
        let map', xs' =
          let g y (map, ys) =
            let y' = new_id y in
              ((y.name,y')::map, y'::ys)
          in
            List.fold_right g xs (map,[])
        in
        let t1' = alpha ((f.name,f')::map') t1 in
        let t2' = alpha ((f.name,f')::map) t2 in
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
  | Nil -> Nil
  | Cons(t1,t2) -> Cons(alpha map t1, alpha map t2)
  | Constr(s,ts) -> Constr(s, List.map (alpha map) ts)
  | Match(t1,t2,x,y,t3) ->
      let x' = new_id x in
      let y' = new_id y in
      let map' = (x.name,x')::(y.name,y')::map in
        Match(alpha map t1, alpha map t2, x', y', alpha map' t3)
  | Match_(t,pats) ->
      let aux (pat,t) =
        let pat',map' = alpha_pat pat in
          pat', alpha (map'@@map) t
      in
        Match_(alpha map t, List.map aux pats)
  | Type_decl(decls,t) ->
      Type_decl(decls, alpha map t)


let add_assert t =
  let f = new_var "assert" in
  let x = new_var "b" in
    Let(Nonrecursive, f, [x], If(Var x, Unit, App(Fail, [Unit])), t)

let alpha t =
  let t' = add_assert t in
    alpha [] t'


