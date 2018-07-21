open Util
open Combinator

(* let rec f = ... in のような引数のない定義はcps変換とかliftingしてはいけない？*)

type 'a t =
    Idnt.t
    * (Idnt.t * (Idnt.t * Idnt.t) list
       * (Idnt.t * Idnt.t * (Idnt.t * Idnt.t) list * 'a JavaExp.t option) list)

let class_of exp = "" (*TODO*)

(* 別のクラスで同じ名前のフィールドがあったらまずい ^ "@" ^ id1*)
let embed_field id1 id2 =
  Idnt.make @@ "$get_" ^ Idnt.string_of id2,
  Type.mk_top

(* 別のクラスで同じ名前のメソッドがあったらまずい ^ "@" ^ id1*)
let embed_method id1 id2 =
  Idnt.make @@ "$get_" ^ Idnt.string_of id2,
  Type.mk_top

let comp id1 id2 =
  Idnt.make @@ Idnt.string_of id1 ^ "@" ^ Idnt.string_of id2,
  Type.mk_top

(*new Objectはできない*)
let embed_new cls id lexps =
  let mtds, _flds =
    try
      List.assoc id cls
    with Not_found ->
      Format.printf "%a@," Idnt.pr id;
      failwith "embed_new"
  in
  let rec f fldss lexps =
    match fldss with
    | [] ->
       assert(lexps = []);
       []
    | flds :: fldss ->
       let lexps1, lexps2 =
         List.split_at (List.length flds) lexps
       in
       lexps1 :: f fldss lexps2
  in
  let flds = f _flds lexps in
  MLExp.mk_church_tuple
    [(List.fold_left
        (fun f lexps ->
         fun x ->
         f (MLExp.mk_church_tuple (lexps @ [x])))
        (fun x -> x)
        flds)
       (MLExp.mk_var MLExp.name_of_id);
     (List.fold_left
        (fun f xs ->
         fun x ->
         f (MLExp.mk_church_tuple
              ((List.map
                  (fun (id1, id2) ->
                   MLExp.mk_var (comp id1 id2 |> fst))
                  xs)
               @ [x])))
        (fun x -> x)
        mtds)
       (MLExp.mk_var MLExp.name_of_id)]

let rec embed_exp cls exp =
  match exp with
  | JavaExp.Var(_, x) ->
     MLExp.mk_var x
  | JavaExp.Field(_, exp, id) ->
     MLExp.mk_app
       (MLExp.mk_church_fst (embed_exp cls exp))
       [MLExp.mk_var (embed_field (class_of exp) id |> fst)]
  | JavaExp.Method(_, exp, id, exps) ->
     let lexp = embed_exp cls exp in
     let id_obj = Idnt.new_var () in
     MLExp.mk_letrec
       ([(id_obj, Type.mk_top), [], lexp])
       (MLExp.mk_app
          (MLExp.mk_app
             (MLExp.mk_church_snd (MLExp.mk_var (id_obj)))
             [MLExp.mk_var (embed_method (class_of exp) id |> fst)])
          (MLExp.mk_var id_obj :: List.map (embed_exp cls) exps))
  | JavaExp.New(_, id, exps) ->
     embed_new cls id (List.map (embed_exp cls) exps)
  | JavaExp.Cast(_, id, exp) ->
     embed_exp cls exp

let embed cls (id1, (_, _, methods)) =
  let mtds, flds = List.assoc id1 cls in
  let _, ms =
    List.fold_left
      (fun (path, ms) xs ->
       let n = List.length xs + 1 in
       path @ [n, n],
       List.mapi
         (fun i (id, _) -> id, path @ [n, i + 1])
         xs
       @ ms)
      ([], [])
      mtds
  in
  let _, fs =
    List.fold_left
      (fun (path, fs) xs ->
       let n = List.length xs + 1 in
       path @ [n, n],
       (List.mapi (fun i id -> id, path @ [n, i + 1]) xs) @ fs)
      ([], [])
      flds
  in
  List.map
    (fun (id, path) ->
     embed_method id1 id, [], MLExp.mk_var(MLExp.name_of_proj path))
    ms
  @ List.map
      (fun (id, path) ->
       embed_field id1 id, [], MLExp.mk_var(MLExp.name_of_proj path))
      fs
  @ List.map
      (fun (_, id2, args, exp_opt) ->
       comp id2 id1,
       (Idnt.make "this", Type.mk_top) :: List.map (fun (_, x) -> x, Type.mk_top) args,
       match exp_opt with
       | None ->
          MLExp.mk_var (Idnt.make "fail")
       | Some(exp) ->
          embed_exp cls exp)
      methods,
  List.map snd ms
  @ List.map snd fs
