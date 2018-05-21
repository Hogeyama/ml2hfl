open Util
open Combinator

include Term

(** {6 Auxiliary constructors} *)

let make ty ts = mk_app (mk_const (Const.Vector(ty, List.length ts))) ts
let make_fvec = make Type.mk_real

(** {6 Operators} *)

let add size t1 t2 =
  match fun_args t1, fun_args t2 with
  | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
    (* size = size1 = size2 *)
    make_fvec (List.map2 RealTerm.add ts1 ts2)
  | _, _ ->
    mk_app (mk_const (Const.Add(Type.mk_vector Type.mk_real size))) [t1; t2]
let sub size t1 t2 =
  match fun_args t1, fun_args t2 with
  | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
    (* size = size1 = size2 *)
    make_fvec (List.map2 RealTerm.sub ts1 ts2)
  | _, _ ->
    mk_app (mk_const (Const.Sub(Type.mk_vector Type.mk_real size))) [t1; t2]
let mul size t1 t2 =
  match fun_args t1, fun_args t2 with
  | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
    (* size = size1 = size2 *)
    make_fvec (List.map2 RealTerm.mul ts1 ts2)
  | _, _ ->
    mk_app (mk_const (Const.Mul(Type.mk_vector Type.mk_real size))) [t1; t2]
let max size t1 t2 =
  match fun_args t1, fun_args t2 with
  | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
    (* size = size1 = size2 *)
    make_fvec (List.map2 RealTerm.max ts1 ts2)
  | _, _ ->
    mk_app (mk_const (Const.Max(Type.mk_vector Type.mk_real size))) [t1; t2]
let min size t1 t2 =
  match fun_args t1, fun_args t2 with
  | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
    (* size = size1 = size2 *)
    make_fvec (List.map2 RealTerm.min ts1 ts2)
  | _, _ ->
    mk_app (mk_const (Const.Min(Type.mk_vector Type.mk_real size))) [t1; t2]

(*let dup size t = mk_app (mk_const (Const.FDup(size))) [t]*)
let rnorm size t =
  mk_app (mk_const (Const.VRnorm(Type.mk_real, size))) [t]
let normalize size t =
  mk_app (mk_const (Const.VNormalize(Type.mk_real, size))) [t]
let dot size t1 t2 =
  mk_app (mk_const (Const.VDot(Type.mk_real, size))) [t1; t2]

let elem size t i =
  match fun_args t with
  | Const(fvec1), ts ->
    begin
      (* size = size1 *)
      try
        List.nth ts i
      with
      | _ ->
        Format.printf "%a.%d@," Term.pr t i;
        assert false
      (*mk_app (mk_const (Const.VElem(Type.mk_real, size, i))) [t]*)
    end
  | _ -> assert false

(** {6 Inspectors} *)

let is_const = fun_args >> function Const(fvec1), _ -> true | _ -> false

(** {6 Operators} *)

let rec simplify t =
  match fun_args t with
  | Var(_), [] | Const(_), [] -> t
  | Const(Const.Add(ty)), [t1; t2] ->
    let t1 = simplify t1 in
    let t2 = simplify t2 in
    add (Type.size_of ty) t1 t2
  | Const(Const.Mul(ty)), [t1; t2] ->
    let t1 = simplify t1 in
    let t2 = simplify t2 in
    mul (Type.size_of ty) t1 t2
  | Const(Const.Max(ty)), [t1; t2] ->
    let t1 = simplify t1 in
    let t2 = simplify t2 in
    max (Type.size_of ty) t1 t2
  | Const(Const.Min(ty)), [t1; t2] ->
    let t1 = simplify t1 in
    let t2 = simplify t2 in
    min (Type.size_of ty) t1 t2
  (*
  | Const(Const.FDup(n)), [t] ->
      make_fvec (List.duplicate t n)
   *)
  | Const(Const.VElem(ty, size, i)), [t] when Type.is_real ty ->
    elem size (simplify t) i
  | t, ts ->
    mk_app (simplify t) (List.map simplify ts)


let vshuffle size t1 t2 t3 =
  let ts =
    match fun_args (simplify t1), fun_args (simplify t2) with
    | (Const(fvec1), ts1), (Const(fvec2), ts2) ->
      (* size1 = size2 *)
      ts1 @ ts2
    | (Const(fvec1), ts1), (Const(Const.Undef), []) ->
      ts1
    | _, _ -> assert false
  in
  match fun_args (simplify t3) with
  | (Const(fvec3), idxs) ->
    (* @todo relate size3 with size1 *)
    idxs
    |> List.map (function Const(Const.Int(i)) -> i | _ -> assert false)
    |> List.map
      (fun i ->
         try
           List.nth ts i
         with _ ->
           begin
             Format.printf "%a.%d@," Term.pr (make_fvec ts) i;
             assert false
           end)
    |> make_fvec
  | _ ->
    assert false
let vshuffle =
  Logger.log_block4
    "VecTerm.vshuffle"
    ~before:(fun _ t1 t2 t3 ->
        Logger.printf "t1: %a@," Term.pr t1;
        Logger.printf "t2: %a@," Term.pr t2;
        Logger.printf "t3: %a@," Term.pr t3)
    vshuffle

let vinsert_element size t1 t2 t3 =
  let ts =
    match fun_args (simplify t1) with
    | Const(fvec1), ts -> ts
    | Const(Const.Undef), [] -> List.duplicate RealTerm.zero size
  in
  match t3 with
  | Const(Const.Int(i)) ->
    make_fvec (List.mapi (fun j t -> if i = j then t2 else t) ts)
  | _ ->
    assert false

let vextract_element size t1 t2 =
  let ts =
    match fun_args (simplify t1) with
    | Const(fvec1), ts -> ts
    | Const(Const.Undef), [] -> List.duplicate RealTerm.zero size
  in
  match t2 with
  | Const(Const.Int(i)) ->
    (try
       List.nth ts i
     with _ ->
       Format.printf "%a.%d@," Term.pr (make_fvec ts) i;
       assert false)
  | _ -> assert false

let sum = simplify >> fun_args >> function
  | Const(fvec1), ts -> RealTerm.sum ts
  | _ -> raise (Global.NotImplemented "VecTerm.sum")
