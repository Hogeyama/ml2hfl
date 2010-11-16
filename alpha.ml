
open Format
open Syntax



let new_id x =
  {x with id = new_int ()}



let free_map : (ident*ident) list ref = ref []
let free : ident list ref = ref []



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
          Var (List.assoc x map)
        with Not_found ->
          try
            Var (List.assoc x !free_map)
          with Not_found ->
            let x' = new_id x in
              free_map := (x,x')::!free_map;
              Var x'
      end
  | Fun(x, t) ->
      let x' = new_id x in
      let t' = alpha ((x,x')::map) t in
        Fun(x', t')
  | App(t, ts) ->
      let t' = alpha map t in
      let ts' = List.map (alpha map) ts in
        App(t', ts')
  | If(t1, t2, t3, _) ->
      let t1' = alpha map t1 in
      let t2' = alpha map t2 in
      let t3' = alpha map t3 in
        If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let t1' = alpha map t1 in
      let t2' = alpha map t2 in
        Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let f' = new_id f in
      let map', xs' =
        let g y (map, ys) =
          let y' = new_id y in
            ((y,y')::map, y'::ys)
        in
          List.fold_right g xs (map,[])
      in
      let t1' = alpha map' t1 in
      let t2' = alpha ((f,f')::map) t2 in
        Let(f', xs', t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let f' = new_id f in
      let map', xs' =
        let g y (map, ys) =
          let y' = new_id y in
            ((y,y')::map, y'::ys)
        in
          List.fold_right g xs (map,[])
      in
      let t1' = alpha ((f,f')::map') t1 in
      let t2' = alpha ((f,f')::map) t2 in
        Letrec(f', xs', t1', t2')
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




let rec add_nint t x =
  let aux t =
    let x1 = new_var' x.origin in
    let x2 = new_var' x.origin in
    let f = new_var' "f" in
      free := x2::!free;
      (*Let(x, [], NInt x2, t)*)
      Let(x, [], App(Let(f, [x1], Var x1, Var f), [NInt x2]), t)
  in
  match t with
    Unit
  | True
  | False
  | Unknown
  | Int _
  | NInt _ -> assert false
  | Var y ->
      assert (x=y);
      aux (Var x)
  | Fun(y, t) ->
      let t' = add_nint t x in
        Fun(y, t')
  | App(t, ts) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 (t::ts) in
        if n>1
        then aux (App(t, ts))
        else
          let ts' = List.map (fun t -> if aux2 t then add_nint t x else t) (t::ts) in
            assert (n=1);
            App(List.hd ts', List.tl ts')
  | If(t1, t2, t3, _) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 [t1; t2; t3] in
        if n>1
        then aux (If(t1, t2, t3, Unit))
        else
          let aux3 t = if aux2 t then add_nint t x else t in
          let t1' = aux3 t1 in
          let t2' = aux3 t2 in
          let t3' = aux3 t3 in
            assert (n=1);
            If(t1', t2', t3', Unit)
  | Branch(t1, t2) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 [t1; t2] in
        if n>1
        then aux (Branch(t1, t2))
        else
          let aux3 t = if aux2 t then add_nint t x else t in
          let t1' = aux3 t1 in
          let t2' = aux3 t2 in
            assert (n=1);
            Branch(t1', t2')
  | Let(f, xs, t1, t2) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 [t1; t2] in
        if n>1
        then aux (Let(f, xs, t1, t2))
        else
          let aux3 t = if aux2 t then add_nint t x else t in
          let t1' = aux3 t1 in
          let t2' = aux3 t2 in
            assert (n=1);
            Let(f, xs, t1', t2')
  | Letrec(f, xs, t1, t2) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 [t1; t2] in
        if n>1
        then aux (Letrec(f, xs, t1, t2))
        else
          let aux3 t = if aux2 t then add_nint t x else t in
          let t1' = aux3 t1 in
          let t2' = aux3 t2 in
            assert (n=1);
            Letrec(f, xs, t1', t2')
  | BinOp(op, t1, t2) ->
      let aux2 t = List.mem x (get_fv t) in
      let n = List.fold_left (fun n t -> if aux2 t then n+1 else n) 0 [t1; t2] in
        if n>1
        then aux (BinOp(op, t1, t2))
        else
          let aux3 t = if aux2 t then add_nint t x else t in
          let t1' = aux3 t1 in
          let t2' = aux3 t2 in
            assert (n=1);
        BinOp(op, t1', t2')
  | Not t ->
      let t' = add_nint t x in
        Not t'
  | Label _ -> assert false
  | Fail -> Fail



let add_assert t =
  let f = new_var "assert" in
  let x = new_var "b" in
    Let(f, [x], If(Var x, Unit, App(Fail, [Unit]), Unit), t)

let alpha t =
  let t' = add_assert t in
  let t'' = alpha [] t' in
  let fv = List.map snd !free_map in
  let t''' = List.fold_left add_nint t'' fv in
    !free, t'''














