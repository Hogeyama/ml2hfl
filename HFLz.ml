open Util

module Debug = Debug.Make(struct let check = Flag.Debug.make_check __MODULE__ end)

type var = V of string
  [@@deriving show]

type op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt
  | And
  | Or
  [@@deriving show]

type hflz =
  | Bool   of bool
  | Int    of int
  | Var    of var
  | Abs    of var * hflz
  | App    of hflz * hflz
  | Op     of op   * hflz * hflz
  [@@deriving show]

type rule = { fn : var; args: var list; body : hflz }
  [@@deriving show]

type hes = rule list
  [@@deriving show]


module OfCEGAR = struct (*{{{*)
  type target = Reachability | NonTermination
  let mk_var ~toplevel s =
    let s = Re2.replace_exn ~f:(fun _ -> "_") (Re2.create_exn "'") s in
    if List.mem s toplevel
    then V (String.uppercase_ascii s)
    else V s
  let rec negate = function
    | Bool b -> Bool (not b)
    | Op (Neq, x, y) -> Op (Eq , x, y)
    | Op (Eq , x, y) -> Op (Neq, x, y)
    | Op (Geq, x, y) -> Op (Lt , x, y)
    | Op (Leq, x, y) -> Op (Gt , x, y)
    | Op (Gt , x, y) -> Op (Leq, x, y)
    | Op (Lt , x, y) -> Op (Geq, x, y)
    | Op (And, x, y) -> Op (Or , negate x, negate y)
    | Op (Or , x, y) -> Op (And, negate x, negate y)
    | t ->
        Format.eprintf "Cannot negate %a@." pp_hflz t;
        failwith "negate"



  let rec term ~target ~toplevel : CEGAR_syntax.t -> hflz =
    let rec aux : CEGAR_syntax.t -> hflz = fun t ->
      match t with
      | Var v -> Var (mk_var ~toplevel v)
      | Let _ -> assert false
      | Const Unit -> Bool true
      | Const CPS_result ->
          begin match target with
          | Reachability -> Bool true
          | NonTermination -> Bool false
          end
      | Const Bottom -> Bool true
      | Const True -> Bool true
      | Const False -> Bool false
      | Const (Int   i) -> Int i
      | Const (Int32 i) -> Int (Int32.to_int i)
      | Const (Int64 i) -> Int (Int64.to_int i)
      | Const (Rand (TInt, None)) ->
          begin match target with
          | Reachability -> Var (V "Forall")
          | NonTermination -> Var (V "Exists")
          end
      | App (App ((App (Const If, x)), y), z) ->
          (* (not x \/ y) /\ (x \/ z) *)
          begin match aux x, aux y, aux z with
          | Bool true , y, _ -> y
          | Bool false, _, z -> z
          | x, Bool true, z  -> Op (Or, x , z)
          | x, Bool false, z -> Op (And, negate x, z)
          | x, y, Bool true  -> Op (Or, negate x, y)
          | x, y, Bool false -> Op (And, x, y)
          | x,y,z -> Op (And, (Op (Or, negate x, y))
                            , (Op (Or, x       , z)))
          end
      | App ((App (Const Add, x)), y) -> Op (Add, aux x, aux y)
      | App ((App (Const Sub, x)), y) -> Op (Sub, aux x, aux y)
      | App ((App (Const Mul, x)), y) -> Op (Mul, aux x, aux y)
      | App ((App (Const Div, x)), y) -> Op (Div, aux x, aux y)
      | App ((App (Const And, x)), y) -> Op (And, aux x, aux y)
      | App ((App (Const Or , x)), y) -> Op (Or , aux x, aux y)
      | App ((App (Const Lt , x)), y) -> Op (Lt , aux x, aux y)
      | App ((App (Const Gt , x)), y) -> Op (Gt , aux x, aux y)
      | App ((App (Const Leq, x)), y) -> Op (Leq, aux x, aux y)
      | App ((App (Const Geq, x)), y) -> Op (Geq, aux x, aux y)
      | App ((App (Const EqInt, x)), y) -> Op (Eq, aux x, aux y)
      | App (Const Not, x) -> negate (aux x)
      | App (Const (TreeConstr _), x) -> aux x
      | App (Const (Label _), x) -> aux x
      | App (x,y) -> App (aux x, aux y)
      | Fun (f,_,x) -> Abs (mk_var ~toplevel f, aux x)
      | t ->
        Format.eprintf "%a@.%a@."
          CEGAR_syntax.pp t
          CEGAR_print.term t;
        assert false
    in aux

  (* fun_def list は同じものが複数入っている *)
  let rule ~target ~toplevel : CEGAR_syntax.fun_def -> rule = fun def  ->
    let fn = mk_var ~toplevel def.fn in
    let args = List.map (mk_var ~toplevel) def.args in
    let body =
      if List.mem (CEGAR_syntax.Event "fail") def.events then
        Bool false
      else
        match negate (term ~target ~toplevel def.cond) with
        | Bool false -> term ~target ~toplevel def.body
        | p -> Op (Or, p, term ~target ~toplevel def.body)
        | exception _ -> failwith "negate @cond"
    in { fn; args; body}

  let hes : target:target -> CEGAR_syntax.prog -> hes = fun ~target ({defs; main=orig_main; _} as prog) ->
    Debug.eprintf "%a" CEGAR_print.prog prog;
    let toplevel = List.map (fun x -> x.CEGAR_syntax.fn) defs in
    let rules =
      let fold_left1 f = function
        | x::xs -> List.fold_left f x xs
        | [] -> assert false
      in
      let merge rule1 rule2 =
        let body = Op (And, rule1.body, rule2.body) in
        { rule1 with body }
      in
      defs
      |> List.map (rule ~target ~toplevel)
      |> List.sort (fun x y -> compare x.fn y.fn)
      |> List.group_consecutive (fun x y -> x.fn = y.fn)
      |> List.map (fold_left1 merge)
    in
    let main, others =
      let main_var = mk_var ~toplevel orig_main in
      let others = List.remove_if (fun x -> x.fn = main_var) rules in
      let main = List.find (fun x -> x.fn = main_var) rules in
      let remove_forall : hflz -> hflz * var list =
        let rec go acc = function
          | App (Var (V "Forall"), Abs (v, t)) -> go (v::acc) t
          | t -> (t, acc)
        in
        go []
      in
      let main =
        let body, args = remove_forall main.body in
        (* { main' with args; body } *) (* 鈴木さんのはこっちに非対応 *)
        { main with body }
      in
      main, others
    in
    main :: others
end(*}}}*)

module OfLifted = struct (*{{{*)
  type def = Syntax.id * (Syntax.id list * Syntax.term)
  type term = Syntax.term
  type target = Reachability | NonTermination

  let var ~toplevel v =
    let s = Re2.replace_exn ~f:(fun _ -> "_") (Re2.create_exn "'") (Id.to_string v) in
    if List.mem s toplevel
    then V (String.uppercase_ascii s)
    else V s

  let rec negate = function
    | Var (V x) when BatHashtbl.mem Trans.negate_bool_pairs x ->
        Var (V (BatHashtbl.find Trans.negate_bool_pairs x))
    | Bool b -> Bool (not b)
    | Op (Neq, x, y) -> Op (Eq , x, y)
    | Op (Eq , x, y) -> Op (Neq, x, y)
    | Op (Geq, x, y) -> Op (Lt , x, y)
    | Op (Leq, x, y) -> Op (Gt , x, y)
    | Op (Gt , x, y) -> Op (Leq, x, y)
    | Op (Lt , x, y) -> Op (Geq, x, y)
    | Op (And, x, y) -> Op (Or , negate x, negate y)
    | Op (Or , x, y) -> Op (And, negate x, negate y)
    | t ->
        Format.eprintf "Cannot negate %a@." pp_hflz t;
        failwith "negate"

  let rec const ~target : Syntax.const -> hflz = function
    | Unit -> Bool true
    | True -> Bool true
    | False -> Bool false
    | CPS_result ->
          begin match target with
          | Reachability -> Bool true
          | NonTermination -> Bool false
          end
    | Int n -> Int n
    | Rand(TBase TInt,true) -> failwith "rand_int_cps"
    | Rand(TBase TBool,true) -> failwith "rand_bool_cps"
    | _ -> assert false

  let term ~toplevel ~target : term -> hflz =
    let rec aux = fun (t : term) ->
      match t.desc with
      | Var v -> Var (var ~toplevel v)
      | Bottom -> Bool true
      | Const Unit -> Bool true
      | Const True -> Bool true
      | Const False -> Bool false
      | Const CPS_result ->
          begin match target with
          | Reachability -> Bool true
          | NonTermination -> Bool false
          end
      | Const Int n -> Int n
      | BinOp (Eq  , x, y) -> Op (Eq , aux x, aux y)
      | BinOp (Neq , x, y) -> Op (Neq, aux x, aux y)
      | BinOp (Lt  , x, y) -> Op (Lt , aux x, aux y)
      | BinOp (Gt  , x, y) -> Op (Gt , aux x, aux y)
      | BinOp (Leq , x, y) -> Op (Leq, aux x, aux y)
      | BinOp (Geq , x, y) -> Op (Geq, aux x, aux y)
      | BinOp (And , x, y) -> Op (And, aux x, aux y)
      | BinOp (Or  , x, y) -> Op (Or , aux x, aux y)
      | BinOp (Add , x, y) -> Op (Add, aux x, aux y)
      | BinOp (Sub , x, y) -> Op (Sub, aux x, aux y)
      | BinOp (Mult, x, y) -> Op (Mul, aux x, aux y)
      | BinOp (Div , x, y) -> Op (Div, aux x, aux y)
      | Fun (f,x) -> Abs (var ~toplevel f, aux x)
      | If (x,y,z) -> (* (not x \/ y) /\ (x \/ z) *)
          begin match aux x, aux y, aux z with
          | Bool true , y, _ -> y
          | Bool false, _, z -> z
          | _, Bool true , Bool true  -> Bool true
          | x, Bool true , Bool false -> x
          | x, Bool false, Bool true  -> negate x
          | _, Bool false, Bool false -> Bool false
          | x, Bool true, z  -> Op (Or, x , z)
          | x, y, Bool true  -> Op (Or, negate x, y)
          (* | x, Bool false, z -> Op (And, negate x, z) *)
          (* | x, y, Bool false -> Op (And, x, y) *)
          | x, Bool false, z -> Op (And, negate x, (Op (Or, x, z)))
          | x, y, Bool false -> Op (And, (Op (Or, negate x, y)), x)
          | x,y,z -> Op (And, (Op (Or, negate x, y))
                            , (Op (Or, x       , z)))
          end
      | App ({desc=Const Rand(TBase TInt,true)}, [_;k]) ->
          let f =
            begin match target with
            | Reachability -> Var (V "Forall")
            | NonTermination -> Var (V "Exists")
            end
          in App (f, aux k)
      | App ({desc=(Syntax.Event ("fail", true))},_) -> Bool false
      | App (x,xs) -> List.fold_left (fun x y -> App (x, aux y)) (aux x) xs
      | _ ->
          Format.eprintf "%a@.%a@."
            Print.term t
            Syntax.pp_term t;
          assert false
    in aux

  let hes ~target : def list * term -> hes = fun (defs, main) ->
    let toplevel =
      List.map (fst |- Id.to_string) defs
    in
    let rules =
      List.Labels.map defs ~f:begin fun (f, (args, body)) ->
        let fn = var ~toplevel f in
        let args = List.map (var ~toplevel) args in
        let body = term ~toplevel ~target body in
        { fn; args; body }
      end
    in
    let main =
      let fn = V "MAIN" in
      let args = [] in
      let body = term ~toplevel ~target main in
      let body, _args =
        let remove_forall : hflz -> hflz * var list =
          let rec go acc = function
            | App (Var (V "Forall"), Abs (v, t)) -> go (v::acc) t
            | t -> (t, acc)
          in
          go []
        in remove_forall body
      in { fn; args; body }
    in
    main :: rules
end(*}}}*)

module Print = struct(*{{{*)
  open Fmt
  open Format
  let list_comma : 'a Fmt.t -> 'a list Fmt.t =
    fun format_x ppf xs ->
      let sep ppf () = Fmt.pf ppf ",@," in
      Fmt.pf ppf "[@[%a@]]" Fmt.(list ~sep format_x) xs
  let list_semi : 'a Fmt.t -> 'a list Fmt.t =
    fun format_x ppf xs ->
      let sep ppf () = Fmt.pf ppf ";@," in
      Fmt.pf ppf "[@[%a@]]" Fmt.(list ~sep format_x) xs
  let list_set : 'a Fmt.t -> 'a list Fmt.t =
    fun format_x ppf xs ->
      let sep ppf () = Fmt.pf ppf ",@," in
      Fmt.pf ppf "{@[%a@]}" Fmt.(list ~sep format_x) xs
  let list_ketsucomma : 'a Fmt.t -> 'a list Fmt.t =
    fun format_x ppf xs ->
      let sep = fun ppf () -> pf ppf "@,, " in
      pf ppf "[ @[<hv -2>%a@] ]" (list ~sep format_x) xs

  module Prec = struct(*{{{*)
    type t = int
    let succ x = x + 1
    let succ_if b x = if b then x + 1 else x

    let zero  = 0
    let arrow = 1
    let abs   = 1
    let or_   = 2
    let and_  = 3
    let eq    = 4
    let add   = 6
    let mul   = 7
    let div   = 8
    let neg   = 9
    let app   = 10

    let of_op = function
      | Add -> add
      | Sub -> add
      | Mul -> mul
      | Div -> div
      | Eq  -> eq
      | Neq -> eq
      | Leq -> eq
      | Geq -> eq
      | Lt  -> eq
      | Gt  -> eq
      | And -> and_
      | Or  -> or_
    let op_is_leftassoc = function
      | Add -> true
      | Sub -> true
      | Mul -> true
      | Div -> true
      | And -> true
      | Or  -> true
      | Eq  -> false
      | Neq -> false
      | Leq -> false
      | Geq -> false
      | Lt  -> false
      | Gt  -> false
    let op_is_rightassoc = function
      | _ -> false
  end(*}}}*)
  type prec = Prec.t
  type 'a t_with_prec = Prec.t -> 'a t
  let ignore_prec : 'a t -> 'a t_with_prec =
    fun orig ->
      fun _prec ppf x ->
        orig ppf x
  let show_paren
       : bool
      -> formatter
      -> ('a, formatter, unit) format
      -> 'a =
    fun b ppf fmt ->
      if b
      then Fmt.pf ppf ("(" ^^ fmt ^^ ")")
      else Fmt.pf ppf fmt

  let var ppf (V x) = Fmt.string ppf x
  let rec hflz_ prec ppf (phi : hflz) = match phi with
    | Bool true -> Fmt.string ppf "true"
    | Bool false -> Fmt.string ppf "false"
    | Int n when n >= 0 -> Fmt.int ppf n
    | Int n -> Fmt.pf ppf "(%d)" n
    | Var v -> var ppf v
    | App (psi1, psi2) ->
        show_paren (prec > Prec.app) ppf "@[<1>%a@ %a@]"
          (hflz_ Prec.app) psi1
          (hflz_ Prec.(succ app)) psi2
    | Abs (x, psi) ->
        show_paren (prec > Prec.abs) ppf "@[<1>\\%a.@,%a@]"
          var x
          (hflz_ Prec.abs) psi
    | Op (op, psi1, psi2) ->
        let op_prec = Prec.of_op op in
        let nx_prec = op_prec + 1 in
        let pr = show_paren (prec > op_prec) in
        let recur = hflz_ nx_prec in
        begin match op with
        | Add -> pr ppf "@[<hv 0>%a@ + %a@]"   recur psi1 recur psi2
        | Sub -> pr ppf "@[<hv 0>%a@ - %a@]"   recur psi1 recur psi2
        | Mul -> pr ppf "@[<hv 0>%a@ * %a@]"   recur psi1 recur psi2
        | Div -> pr ppf "@[<hv 0>%a@ / %a@]"   recur psi1 recur psi2
        | Eq  -> pr ppf "@[<hv 0>%a@ = %a@]"   recur psi1 recur psi2
        | Neq -> pr ppf "@[<hv 0>%a@ != %a@]"  recur psi1 recur psi2
        | Leq -> pr ppf "@[<hv 0>%a@ <= %a@]"  recur psi1 recur psi2
        | Geq -> pr ppf "@[<hv 0>%a@ >= %a@]"  recur psi1 recur psi2
        | Lt  -> pr ppf "@[<hv 0>%a@ < %a@]"   recur psi1 recur psi2
        | Gt  -> pr ppf "@[<hv 0>%a@ > %a@]"   recur psi1 recur psi2
        | And -> pr ppf "@[<hv 0>%a@ /\\ %a@]" recur psi1 recur psi2
        | Or  -> pr ppf "@[<hv 0>%a@ \\/ %a@]" recur psi1 recur psi2
        end
  let hflz : hflz Fmt.t = hflz_ Prec.zero

  let rule : rule Fmt.t = fun ppf rule ->
    Fmt.pf ppf "@[<2>%a %a@ =v@ %a.@]"
      var rule.fn
      (list ~sep:(fun ppf () -> string ppf " ") var) rule.args
      hflz rule.body

  let hes : hes Fmt.t = fun ppf hes ->
    Fmt.pf ppf "%%HES@.";
    Fmt.pf ppf "@[<v>%a@]@." (Fmt.list rule)  hes;
    Fmt.pf ppf "Forall p      =v ForallAux p 0.@.";
    Fmt.pf ppf "ForallAux p x =v p x /\\ p (0-x) /\\ ForallAux p (x+1).@.";
    (* Fmt.pf ppf "Exists p        =v p 1000 \\/ p 8 \\/ p 5 \\/ p 3 \\/ p (-200).@."; *)
    Fmt.pf ppf "Exists p      =v ExistsAux 1000 p.@.";
    Fmt.pf ppf "ExistsAux x p =v x > 0 /\\ (p x \\/ p (0-x) \\/ ExistsAux (x-1) p).@."
end(*}}}*)

let of_cegar : CEGAR_syntax.prog -> hes = fun prog ->
  let target : OfCEGAR.target =
    match !Flag.Method.mode with
    | NonTermination -> NonTermination
    | Reachability   -> Reachability
    | _ -> unsupported "HFLz.of_cegar: mode"
  in OfCEGAR.hes ~target prog

let of_lifted : (Syntax.id * (Syntax.id list * Syntax.term)) list * Syntax.term -> hes =
  fun lifted ->
    let target : OfLifted.target =
      match !Flag.Method.mode with
      | NonTermination -> NonTermination
      | Reachability   -> Reachability
      | _ -> unsupported "HFLz.of_cegar: mode"
    in OfLifted.hes ~target lifted

