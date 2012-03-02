
open Format
open Utilities
open Type

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

type typ = typed_term Type.t
and id = typ Id.t

and typed_term = {desc:term; typ:typ}
and term =
    Unit
  | True
  | False
  | Unknown
  | Int of int
  | NInt of id
  | RandInt of bool
  | RandValue of typ * bool
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of Flag.rec_flag * (id * id list * typed_term) list * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Event of string * bool
  | Record of (string * (Flag.mutable_flag * typed_term)) list
  | Proj of int * string * Flag.mutable_flag * typed_term
  | SetField of int option * int * string * Flag.mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term option * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Pair of typed_term * typed_term
  | Fst of typed_term
  | Snd of typed_term
  | Bottom
(*
  | TAbs of (typ -> typed_term)
*)


and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (Flag.mutable_flag * typ)) list

and pred = term

and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PVar of id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PPair of typed_pattern * typed_pattern
  | PRecord of (int * (string * Flag.mutable_flag * typed_pattern)) list
  | POr of typed_pattern * typed_pattern

type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int


exception Feasible of typed_term
exception Infeasible

type literal = Cond of typed_term | Pred of (id * int * id * typed_term list)


(*** PRINTING FUNCTIONS ***)

let rec print_typ t = Type.print (print_term 0 false) t
and print_ids fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" Id.print x print_ids xs

(*
  and print_id fm x = fprintf fm "(%a:%a)" Id.print x print_typ (Id.typ x)
*)
and print_id = Id.print

and print_id_typ fm x =
  let typ = Id.typ x in
    fprintf fm "(%a:%a)" Id.print x print_typ typ

and print_ids_typ fm = function
    [] -> ()
  | x::xs -> fprintf fm "%a %a" print_id_typ x print_ids_typ xs

(* priority (low -> high)
   1 : If, Let, Letrec, Match
   2 : Fun
   3 : Or
   4 : And
   5 : Eq, Lt, Gt, Leq, Geq
   6 : Add, Sub
   7 : Cons
   8 : App
*)

and paren pri p = if pri < p then "","" else "(",")"

and print_binop fm = function
    Eq -> fprintf fm "="
  | Lt -> fprintf fm "<"
  | Gt -> fprintf fm ">"
  | Leq -> fprintf fm "<="
  | Geq -> fprintf fm ">="
  | And -> fprintf fm "&&"
  | Or -> fprintf fm "||"
  | Add -> fprintf fm "+"
  | Sub -> fprintf fm "-"
  | Mult -> fprintf fm "*"

and print_termlist pri typ fm = List.iter (fun bd -> fprintf fm "@;%a" (print_term pri typ) bd)
and print_term pri typ fm t =
  match t.desc with
      Unit -> fprintf fm "unit"
    | True -> fprintf fm "true"
    | False -> fprintf fm "false"
    | Unknown -> fprintf fm "***"
    | Int n -> fprintf fm "%d" n
    | NInt x -> fprintf fm "?%a?" print_id x
    | RandInt false -> fprintf fm "rand_int"
    | RandInt true -> fprintf fm "rand_int_cps"
    | RandValue(typ',false) -> fprintf fm "rand_val[%a]()" print_typ typ'
    | RandValue(typ',true) -> fprintf fm "rand_val_cps[%a]" print_typ typ'
    | Var x -> print_id fm x
    | Fun(x, t) ->
        let p = 2 in
        let s1,s2 = paren pri p in
          fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term p typ) t s2
    | App(t, ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%s%a%a%s" s1 (print_term p typ) t (print_termlist p typ) ts s2
    | If(t1, t2, t3) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
          fprintf fm "%s@[@[if %a@]@;then @[%a@]@;else @[%a@]@]%s"
            s1 (print_term p typ) t1 (print_term p typ) t2 (print_term p typ) t3 s2
    | Branch(t1, t2) ->
        let p = 8 in
        let s1,s2 = paren pri p in
          fprintf fm "%sbr %a %a%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
    | Let(_, [], _) -> assert false
    | Let(flag, bindings, t2) ->
        let s_rec = match flag with Flag.Nonrecursive -> "" | Flag.Recursive -> " rec" in
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let p_ids fm xs =
          if typ
          then fprintf fm "%a@ %a" print_id (List.hd xs) print_ids_typ (List.tl xs)
          else fprintf fm "%a" print_ids xs
        in
        let b = ref true in
        let print_binding fm (f,xs,t1) =
          let pre = if !b then "let" ^ s_rec else "and" in
            Format.printf "%s %a=@ %a@ " pre p_ids (f::xs) (print_term p typ) t1;
            b := false
        in
        let print_bindings fm = List.iter (print_binding fm) in
          begin
            match t2.desc with
                Let _ -> fprintf fm "%s@[<v>@[<hov 2>%a@]@ in@ %a@]%s"
                  s1 print_bindings bindings (print_term p typ) t2 s2
              | _ -> fprintf fm     "%s@[<v>@[<hov 2>%a@]@ @[<v 2>in@ @]@[<hov>%a@]@]%s"
                  s1 print_bindings bindings (print_term p typ) t2 s2
          end
    | BinOp(op, t1, t2) ->
        let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
        let s1,s2 = paren pri p in
          fprintf fm "%s%a %a %a%s" s1 (print_term p typ) t1 print_binop op (print_term p typ) t2 s2
    | Not t ->
        let p = 6 in
        let s1,s2 = paren pri p in
          fprintf fm "%snot %a%s" s1 (print_term p typ) t s2
    | Event(s,false) -> fprintf fm "{%s}" s
    | Event(s,true) -> fprintf fm "{|%s|}" s
    | Record fields ->
        let rec aux fm = function
            [] -> ()
          | (s,(f,t))::fields ->
              if fields=[]
              then fprintf fm "%s=%a" s (print_term 0 typ) t
              else fprintf fm "%s=%a; %a" s (print_term 0 typ) t aux fields
        in
          fprintf fm "{%a}" aux fields
    | Proj(_,s,_,t) -> fprintf fm "%a.%s" (print_term 9 typ) t s
    | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s <- %a" (print_term 9 typ) t1 s (print_term 3 typ) t2
    | Nil -> fprintf fm "[]"
    | Cons(t1,t2) ->
        let p = 7 in
        let s1,s2 = paren pri p in
          fprintf fm "%s%a::@,%a%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
    | Constr(s,ts) ->
        let p = 8 in
        let s1,s2 = paren pri p in
        let aux fm = function
            [] -> ()
          | [t] -> fprintf fm "(%a)" (print_term 1 typ) t
          | t::ts ->
              fprintf fm "(%a" (print_term 1 typ) t;
              List.iter (fun t -> fprintf fm ",%a" (print_term 1 typ) t) ts;
              pp_print_string fm ")"
        in
          fprintf fm "%s%s%a%s" s1 s aux ts s2
    | Match(t,pats) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
        let aux = function
            (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern pat (print_term p typ) t
          | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
              print_pattern pat (print_term p typ) cond (print_term p typ) t
        in
          fprintf fm "%smatch %a with@;" s1 (print_term p typ) t;
          List.iter aux pats;
          pp_print_string fm s2
    | Raise t ->
        let p = 4 in
        let s1,s2 = paren pri (p+1) in
          fprintf fm "%sraise %a%s" s1 (print_term 1 typ) t s2
    | TryWith(t1,t2) ->
        let p = 1 in
        let s1,s2 = paren pri (p+1) in
          fprintf fm "%stry %a with@;%a%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
    | Pair(t1,t2) ->
        fprintf fm "(%a,%a)" (print_term 2 typ) t1 (print_term 2 typ) t2
    | Fst t ->
        let p = 8 in
        let s1,s2 = paren pri (p+1) in
          fprintf fm "%sfst %a%s" s1 (print_term p typ) t s2
    | Snd t ->
        let p = 8 in
        let s1,s2 = paren pri (p+1) in
          fprintf fm "%ssnd %a%s" s1 (print_term p typ) t s2
    | Bottom -> fprintf fm "_|_"

and print_pattern fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
        PVar x -> print_id fm x
      | PConst c -> print_term 1 false fm c
      | PConstruct(c,pats) ->
          let aux' = function
              [] -> ()
            | [pat] -> fprintf fm "(%a)" aux pat
            | pat::pats ->
                fprintf fm "(%a" aux pat;
                List.iter (fun pat -> fprintf fm ",%a" aux pat) pats;
                pp_print_string fm ")"
          in
            pp_print_string fm c;
            aux' pats
      | PNil -> fprintf fm "[]"
      | PCons(p1,p2) -> fprintf fm "%a::%a" aux p1 aux p2
      | PRecord pats ->
          let aux' = function
              [] -> ()
            | [_,(_,_,pat)] -> fprintf fm "(%a)" aux pat
            | (_,(_,_,pat))::pats ->
                fprintf fm "(%a" aux pat;
                List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" aux pat) pats;
                pp_print_string fm ")"
          in
            aux' pats
      | POr(pat1,pat2) -> fprintf fm "(%a | %a)" aux pat1 aux pat2
      | PPair(pat1,pat2) -> fprintf fm "(%a, %a)" aux pat1 aux pat2
  in
    fprintf fm "| %a" aux pat
let print_term typ fm = print_term 0 typ fm

let rec print_term' pri fm t =
  fprintf fm "(";(
    match t.desc with
        Unit -> fprintf fm "unit"
      | True -> fprintf fm "true"
      | False -> fprintf fm "false"
      | Unknown -> fprintf fm "***"
      | Int n -> fprintf fm "%d" n
      | NInt x -> fprintf fm "?%a?" print_id x
      | RandInt false -> fprintf fm "rand_int"
      | RandInt true -> fprintf fm "rand_int_cps"
      | RandValue(typ',false) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%srand_val(%a)%s" s1 print_typ typ' s2
      | RandValue(typ',true) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%srand_val(%a)%s" s1 print_typ typ' s2
      | Var x -> print_id_typ fm x
      | Fun(x, t) ->
          let p = 2 in
          let s1,s2 = paren pri p in
            fprintf fm "%sfun %a -> %a%s" s1 print_id x (print_term' p) t s2
      | App(t, ts) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a%a%s" s1 (print_term' p) t (print_termlist' p) ts s2
      | If(t1, t2, t3) ->
          let p = 1 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%s@[@[if %a@]@;then @[%a@]@;else @[%a@]@]%s"
              s1 (print_term' p) t1 (print_term' p) t2 (print_term' p) t3 s2
      | Branch(t1, t2) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%sbr %a %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
      | Let(flag, [f, xs, t1], t2) ->
          let s_rec = match flag with Flag.Nonrecursive -> "" | Flag.Recursive -> " rec" in
          let p = 1 in
          let s1,s2 = paren pri (p+1) in
          let p_ids fm () =
            fprintf fm "%a" print_ids_typ (f::xs)
          in
            begin
              match t2.desc with
                  Let _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a@]@;in@;%a@]%s"
                    s1 s_rec p_ids () (print_term' p) t1 (print_term' p) t2 s2
                | _ -> fprintf fm "%s@[<v>@[<hov 2>let%s %a= @,%a @]@;@[<v 2>in@;@]@[<hov>%a@]@]%s"
                    s1 s_rec p_ids () (print_term' p) t1 (print_term' p) t2 s2
            end
      | Let _ -> assert false
      | BinOp(op, t1, t2) ->
          let p = match op with Add|Sub|Mult -> 6 | And -> 4 | Or -> 3 | _ -> 5 in
          let s1,s2 = paren pri p in
            fprintf fm "%s%a %a %a%s" s1 (print_term' p) t1 print_binop op (print_term' p) t2 s2
      | Not t ->
          let p = 6 in
          let s1,s2 = paren pri p in
            fprintf fm "%snot %a%s" s1 (print_term' p) t s2
      | Event(s,b) -> fprintf fm "{%s}" s
      | Record fields ->
          let rec aux fm = function
              [] -> ()
            | (s,(f,t))::fields ->
                if fields=[]
                then fprintf fm "%s=%a" s (print_term' 0) t
                else fprintf fm "%s=%a; %a" s (print_term' 0) t aux fields
          in
            fprintf fm "{%a}" aux fields
      | Proj(_,s,_,t) -> fprintf fm "%a.%s" (print_term' 9) t s
      | SetField(_,_,s,_,t1,t2) -> fprintf fm "%a.%s <- %a" (print_term' 9) t1 s (print_term' 3) t2
      | Nil -> fprintf fm "[]"
      | Cons(t1,t2) ->
          let p = 7 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%s%a::%a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
      | Constr(s,ts) ->
          let p = 8 in
          let s1,s2 = paren pri p in
          let aux fm = function
              [] -> ()
            | [t] -> fprintf fm "(%a)" (print_term' 1) t
            | t::ts ->
                fprintf fm "(%a" (print_term' 1) t;
                List.iter (fun t -> fprintf fm ",%a" (print_term' 1) t) ts;
                pp_print_string fm ")"
          in
            fprintf fm "%s%s%a%s" s1 s aux ts s2
      | Match(t,pats) ->
          let p = 1 in
          let s1,s2 = paren pri (p+1) in
          let aux = function
              (pat,None,t) -> fprintf fm "%a -> %a@;" print_pattern' pat (print_term' p) t
            | (pat,Some cond,t) -> fprintf fm "%a when %a -> %a@;"
                print_pattern' pat (print_term' p) cond (print_term' p) t
          in
            fprintf fm "%smatch %a with@;" s1 (print_term' p) t;
            List.iter aux pats;
            pp_print_string fm s2
      | Raise t ->
          let p = 4 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%sraise %a%s" s1 (print_term' 1) t s2
      | TryWith(t1,t2) ->
          let p = 1 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%stry %a with@;%a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
      | Pair(t1,t2) ->
          fprintf fm "(%a,%a)" (print_term' 2) t1 (print_term' 2) t2
      | Fst t ->
          let p = 4 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%sfst %a%s" s1 (print_term' 1) t s2
      | Snd t ->
          let p = 4 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%ssnd %a%s" s1 (print_term' 1) t s2
      | Bottom -> fprintf fm "_|_"
  );fprintf fm ":%a)" print_typ t.typ
and print_pattern' fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
        PVar x -> print_id_typ fm x
      | PConst c -> print_term' 1 fm c
      | PConstruct(c,pats) ->
          let aux' = function
              [] -> ()
            | [pat] -> fprintf fm "(%a)" aux pat
            | pat::pats ->
                fprintf fm "(%a" aux pat;
                List.iter (fun pat -> fprintf fm ",%a" aux pat) pats;
                pp_print_string fm ")"
          in
            pp_print_string fm c;
            aux' pats
      | PNil -> fprintf fm "[]"
      | PCons(p1,p2) -> fprintf fm "%a::%a" aux p1 aux p2
      | PRecord pats ->
          let aux' = function
              [] -> ()
            | [_,(_,_,pat)] -> fprintf fm "(%a)" aux pat
            | (_,(_,_,pat))::pats ->
                fprintf fm "(%a" aux pat;
                List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" aux pat) pats;
                pp_print_string fm ")"
          in
            aux' pats
      | POr(pat1,pat2) -> fprintf fm "(%a | %a)" aux pat1 aux pat2
      | PPair(pat1,pat2) -> fprintf fm "(%a, %a)" aux pat1 aux pat2
  in
    fprintf fm "| %a" aux pat

and print_termlist' pri fm = List.iter (fun bd -> fprintf fm "@;%a" (print_term' pri) bd)
let print_term' fm = print_term' 0 fm
let pp_print_term' = print_term'

let string_of_node = function
    BrNode -> assert false
  | LabNode true -> "then"
  | LabNode false -> "else"
  | FailNode -> "fail"
  | PatNode n -> "br" ^ string_of_int n
  | EventNode s -> s

let print_constr fm = function
    Cond t -> print_term false fm t
  | Pred(f,n,x,ts) -> Format.printf "P_{%a_%d}^%a(%a)" print_id f n print_id x (print_termlist 0 false) ts
let print_constr_list fm = List.iter (fun c -> Format.fprintf fm "@;[%a]" print_constr c)

let pp_print_typ = print_typ
let pp_print_term = print_term false
let pp_print_term_typ = print_term true

let print_defs fm (defs:(id * (id list * typed_term)) list) =
  let print_fundef (f, (xs, t)) =
    fprintf fm "%a %a-> %a.\n" print_id f print_ids xs pp_print_term t
  in
    List.iter print_fundef defs



(*** TERM CONSTRUCTORS ***)

let dummy_var = Id.make (-1) "" TInt
let abst_var = Id.make (-1) "v" TUnknown
let length_var =
  let x = Id.make (-1) "l" (TList TUnknown) in
    Id.make (-1) "length" (TFun(x, TInt))

let typ_event = TFun(Id.new_var "" TUnit, TUnit)
let typ_event_cps =
  let u = Id.new_var "" TUnit in
  let r = Id.new_var "" TUnit in
  let k = Id.new_var "" (TFun(r,TUnit)) in
    TFun(u, TFun(k, TUnit))
let typ_excep = ref (TConstr("exn",true))

let unit_term = {desc=Unit; typ=TUnit}
let true_term = {desc=True;typ=TBool}
let false_term = {desc=False;typ=TBool}
let fail_term = {desc=Event("fail",false);typ=typ_event}
let randint_term = {desc=RandInt false; typ=TFun(Id.new_var "" TUnit,TInt)}
let make_bottom typ = {desc=Bottom;typ=typ}
let make_event s = {desc=Event(s,false);typ=typ_event}
let make_event_cps s = {desc=Event(s,true);typ=typ_event_cps}
let make_var x = {desc=Var x; typ=Id.typ x}
let make_int n = {desc=Int n; typ=TInt}
let make_randint_cps typ =
  let u = Id.new_var "" TUnit in
  let r = Id.new_var "" TInt in
  let k = Id.new_var "" (TFun(r,typ)) in
    {desc=RandInt true; typ=TFun(u,TFun(k,typ))}
let rec make_app t ts =
  match t,ts with
    | t,[]_ -> t
    | {desc=App(t1,ts1);typ=TFun(x,typ)}, t2::ts2 ->
        assert (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ);
        make_app {desc=App(t1,ts1@[t2]); typ=typ} ts2
    | {typ=TFun(x,typ)}, t2::ts ->
        if not (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ)
        then (Format.printf "make_app: %a <=/=> %a, %a@."
                print_typ (Id.typ x)
                print_typ t2.typ
                pp_print_term {desc=App(t,ts);typ=TUnit};
              assert false);
        make_app {desc=App(t,[t2]); typ=typ} ts
    | _ when not Flag.check_typ -> {desc=App(t,ts); typ=TUnknown}
    | _ -> Format.printf "Untypable(make_app): %a@." pp_print_term {desc=App(t,ts);typ=TUnknown}; assert false
let make_let bindings t2 =
  {desc=Let(Flag.Nonrecursive,bindings,t2); typ=t2.typ}
let make_letrec bindings t2 =
  {desc=Let(Flag.Recursive,bindings,t2); typ=t2.typ}
let make_let_f flag bindings t2 =
  {desc=Let(flag,bindings,t2); typ=t2.typ}
let rec make_loop typ =
  let u = Id.new_var "u" TUnit in
  let f = Id.new_var "loop" (TFun(u,typ)) in
  let t = make_app (make_var f) [make_var u] in
    make_letrec [f, [u], t] (make_app (make_var f) [unit_term])
let make_fail typ =
  let u = Id.new_var "u" typ_event in
    make_let [u, [], fail_term] (make_bottom typ)
let make_fun x t = {desc=Fun(x,t); typ=TFun(x,t.typ)}
let make_not t = {desc=Not t; typ=TBool}
let make_and t1 t2 =
  if t1 = true_term
  then t2
  else
    if t1 = false_term
    then false_term
    else
      if t2 = true_term
      then t1
      else {desc=BinOp(And, t1, t2); typ=TBool}
let make_or t1 t2 =
  if t1 = true_term
  then true_term
  else
    if t1 = false_term
    then t2
    else {desc=BinOp(Or, t1, t2); typ=TBool}
let make_add t1 t2 = {desc=BinOp(Add, t1, t2); typ=TInt}
let make_sub t1 t2 = {desc=BinOp(Sub, t1, t2); typ=TInt}
let make_mul t1 t2 = {desc=BinOp(Mult, t1, t2); typ=TInt}
let make_neg t = make_sub (make_int 0) t
let make_if_ t1 t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TBool);
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  match t1.desc with
      True -> t2
    | False -> t3
    | _ ->
        let typ =
          match has_pred t2.typ, has_pred t3.typ with
              false, false -> t2.typ
            | true, false -> t2.typ
            | false, true -> t3.typ
            | true, true ->
                if t2.typ <> t3.typ
                then Format.printf "make_if_ (%a) (%a)@." pp_print_typ t2.typ pp_print_typ t3.typ;
                t2.typ
        in
          {desc=If(t1, t2, t3); typ=typ}
let make_branch t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  {desc=Branch(t2, t3); typ=t2.typ}
let make_eq t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ t2.typ);
  {desc=BinOp(Eq, t1, t2); typ=TBool}
let make_neq t1 t2 =
  make_not (make_eq t1 t2)
let make_lt t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Lt, t1, t2); typ=TBool}
let make_gt t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Gt, t1, t2); typ=TBool}
let make_leq t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Leq, t1, t2); typ=TBool}
let make_geq t1 t2 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TInt);
  assert (not Flag.check_typ || Type.can_unify t2.typ TInt);
  {desc=BinOp(Geq, t1, t2); typ=TBool}
let make_fst t =
  let typ =
    match elim_tpred t.typ with
        TPair(typ,_) -> typ
      | TUnknown -> TUnknown
      | _ -> assert false
  in
    {desc=Fst t; typ=typ}
let make_snd t =
  let typ =
    match elim_tpred t.typ with
        TPair(_,typ) -> typ
      | TUnknown -> TUnknown
      | typ -> Format.printf "%a@." print_typ typ; assert false
  in
    {desc=Snd t; typ=typ}
let make_pair t1 t2 = {desc=Pair(t1,t2); typ=TPair(t1.typ,t2.typ)}
let make_nil typ = {desc=Nil; typ=typ}
let make_cons t1 t2 =
  assert (not Flag.check_typ || Type.can_unify (TList t1.typ) t2.typ);
  {desc=Cons(t1,t2); typ=t2.typ}
let make_match t1 pats = {desc=Match(t1,pats); typ=(fun (_,_,t) -> t.typ) (List.hd pats)}

let make_pvar x = {pat_desc=PVar x; pat_typ=Id.typ x}
let make_pconst t = {pat_desc=PConst t; pat_typ=t.typ}
let make_pnil typ = {pat_desc=PNil; pat_typ=typ}
let make_pcons p1 p2 = {pat_desc=PCons(p1,p2); pat_typ=p2.pat_typ}


let imply t1 t2 = {desc=BinOp(Or, {desc=Not t1;typ=TBool}, t2); typ=TBool}
let and_list ts = match ts with
    [] -> {desc=True; typ=TBool}
  | [t] -> t
  | t::ts -> List.fold_left (fun t1 t2 -> {desc=BinOp(And,t1,t2);typ=TBool}) t ts





(*** AUXILIARY FUNCTIONS ***)

let rec decomp_fun = function
    {desc=Fun(x,t)} ->
      let xs,t' = decomp_fun t in
        x::xs, t'
  | t -> [], t

let rec get_nint t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> [x]
    | Var x -> []
    | App(t, ts) -> get_nint t @@ (rev_map_flatten get_nint ts)
    | If(t1, t2, t3) -> get_nint t1 @@ get_nint t2 @@ get_nint t3
    | Branch(t1, t2) -> get_nint t1 @@ get_nint t2
    | Let(flag, bindings, t2) -> List.fold_left (fun acc (_,_,t) -> get_nint t @@ acc) (get_nint t2) bindings
    | BinOp(op, t1, t2) -> get_nint t1 @@ get_nint t2
    | Not t -> get_nint t
    | Fun(x,t) -> diff (get_nint t) [x]
    | Event _ -> []
    | Nil -> []
    | Cons(t1,t2) -> get_nint t1 @@ get_nint t2
    | RandInt _ -> []
    | Fst _ -> assert false
    | Snd _ -> assert false
    | Pair _ -> assert false
    | TryWith _ -> assert false
    | Raise _ -> assert false
    | Match _ -> assert false
    | Constr _ -> assert false
    | SetField _ -> assert false
    | Proj _ -> assert false
    | Record _ -> assert false
    | RandValue _ -> assert false
    | Bottom -> []
let get_nint t = uniq (get_nint t)

let rec get_int t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> [n]
    | NInt x -> []
    | Var x -> []
    | App(t, ts) -> get_int t @@ (rev_map_flatten get_int ts)
    | If(t1, t2, t3) -> get_int t1 @@ get_int t2 @@ get_int t3
    | Branch(t1, t2) -> get_int t1 @@ get_int t2
    | Let(flag, bindings, t2) -> List.fold_left (fun acc (_,_,t) -> get_int t @@ acc) (get_int t2) bindings
    | BinOp(Mult, t1, t2) -> [] (* non-linear expressions not supported *)
    | BinOp(_, t1, t2) -> get_int t1 @@ get_int t2
    | Not t -> get_int t
    | Fun(_,t) -> get_int t
    | Event _ -> []
    | Nil -> []
    | Cons(t1,t2) -> get_int t1 @@ get_int t2
    | RandInt _ -> []
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | Match (_, _) -> assert false
    | Constr (_, _) -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | Bottom -> []
let get_int t = uniq (get_int t)

let rec get_fv vars t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> []
    | RandInt _ -> []
    | Var x -> if List.mem x vars then [] else [x]
    | App(t, ts) -> get_fv vars t @@ (rev_map_flatten (get_fv vars) ts)
    | If(t1, t2, t3) -> get_fv vars t1 @@ get_fv vars t2 @@ get_fv vars t3
    | Branch(t1, t2) -> get_fv vars t1 @@ get_fv vars t2
    | Let(flag, bindings, t2) ->
        let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
        let vars' = match flag with Flag.Nonrecursive -> vars | Flag.Recursive -> vars_with_fun in
        let aux fv (_,xs,t) = get_fv (xs@@vars') t @@ fv in
        let fv_t2 = get_fv vars_with_fun t2 in
          List.fold_left aux fv_t2 bindings
    | BinOp(op, t1, t2) -> get_fv vars t1 @@ get_fv vars t2
    | Not t -> get_fv vars t
    | Fun(x,t) -> get_fv (x::vars) t
    | Event(s,_) -> []
    | Record fields -> List.fold_left (fun acc (_,(_,t)) -> get_fv vars t @@ acc) [] fields
    | Proj(_,_,_,t) -> get_fv vars t
    | SetField(_,_,_,_,t1,t2) -> get_fv vars t1 @@ get_fv vars t2
    | Nil -> []
    | Cons(t1, t2) -> get_fv vars t1 @@ get_fv vars t2
    | Constr(_,ts) -> List.fold_left (fun acc t -> get_fv vars t @@ acc) [] ts
    | Match(t,pats) ->
        let aux acc (_,_,t) = get_fv vars t @@ acc in
          List.fold_left aux (get_fv vars t) pats
    | TryWith(t1,t2) -> get_fv vars t1 @@ get_fv vars t2
    | Bottom -> []
    | Pair(t1,t2) -> get_fv vars t1 @@ get_fv vars t2
    | Fst t -> get_fv vars t
    | Snd t -> get_fv vars t
    | Raise t -> get_fv vars t
    | RandValue _ -> assert false
let get_fv t = uniq (get_fv [] t)

let rec get_fv2 vars t =
  match t.desc with
      Unit -> []
    | True -> []
    | False -> []
    | Unknown -> []
    | Int n -> []
    | NInt x -> [x]
    | RandInt _ -> []
    | Var x -> if List.mem x vars then [] else [x]
    | App(t, ts) -> get_fv2 vars t @@ (rev_map_flatten (get_fv2 vars) ts)
    | If(t1, t2, t3) -> get_fv2 vars t1 @@ get_fv2 vars t2 @@ get_fv2 vars t3
    | Branch(t1, t2) -> get_fv2 vars t1 @@ get_fv2 vars t2
    | Let(flag, bindings, t) ->
        let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
        let vars' = match flag with Flag.Nonrecursive -> vars | Flag.Recursive -> vars_with_fun in
        let aux fv (_,xs,t) = get_fv2 (xs@@vars') t @@ fv in
        let fv_t = get_fv2 vars_with_fun t in
          List.fold_left aux fv_t bindings
            (*
              | Let(flag, bindings, t) ->
              let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
              let vars' = match flag with Nonrecursive -> vars | Recursive -> vars_with_fun in
              let aux fv (_,xs,t) = get_fv2 (xs@@vars') t @@ fv in
              let fv_t = get_fv2 vars_with_fun t in
              List.fold_left aux fv_t bindings
            *)
    | BinOp(op, t1, t2) -> get_fv2 vars t1 @@ get_fv2 vars t2
    | Not t -> get_fv2 vars t
    | Fun(x,t) -> get_fv2 (x::vars) t
    | Event(s,_) -> []
    | Nil -> []
    | Cons(t1,t2) -> get_fv2 vars t1 @@ get_fv2 vars t2
    | Constr(_,ts) -> List.fold_left (fun acc t -> get_fv2 vars t @@ acc) [] ts
    | Match(t,pats) ->
        let aux acc (_,_,t) = get_fv2 vars t @@ acc in
          List.fold_left aux (get_fv2 vars t) pats
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | Bottom -> []
let get_fv2 t = uniq (get_fv2 [] t)



let rec get_vars_pat pat =
  match pat.pat_desc with
      PVar x -> [x]
    | PConst _ -> []
    | PConstruct(_,pats) -> List.fold_left (fun acc pat -> get_vars_pat pat @@ acc) [] pats
    | PRecord pats -> List.fold_left (fun acc (_,(_,_,pat)) -> get_vars_pat pat @@ acc) [] pats
    | POr(p1,p2) -> get_vars_pat p1 @@ get_vars_pat p2
    | PPair _ -> assert false
    | PCons _ -> assert false
    | PNil -> []



let rec get_args = function
    TFun(x,typ) -> x :: get_args typ
  | _ -> []

let rec get_argvars = function
    TFun(x,typ) -> x :: get_argvars (Id.typ x) @ get_argvars typ
  | _ -> []

let rec get_argtyps = function
    TFun(x,typ) -> Id.typ x :: get_argtyps typ
  | _ -> []








let rec subst_var x t y = Id.set_typ y (subst_type x t (Id.typ y))

(* [x |-> t], [t/x] *)
and subst x t t' =
(*
  let t'' =
*)
    match t'.desc with
        Unit -> t'
      | True -> t'
      | False -> t'
      | Unknown -> t'
      | Int n -> t'
      | NInt y -> if Id.same x y then t else t'
      | Bottom -> t'
      | RandInt _ -> t'
      | Var y when Id.same x y -> t
      | Var y -> make_var (subst_var x t y)
      | Fun(y, t1) when Id.same x y -> t'
      | Fun(y, t1) ->
          let t1' = subst x t t1 in
          let y' = subst_var x t y in
            make_fun y' t1'
      | App(t1, ts) ->
          let t1' = subst x t t1 in
          let ts' = List.map (subst x t) ts in
            make_app t1' ts'
      | If(t1, t2, t3) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
          let t3' = subst x t t3 in
            make_if_ t1' t2' t3'
      | Branch(t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            make_branch t1' t2'
      | Let(Flag.Nonrecursive, bindings, t2) ->
          let aux (f,xs,t1) =
            subst_var x t f,
            List.map (subst_var x t) xs,
            if List.exists (Id.same x) xs then t else subst x t t1 in
          let bindings' = List.map aux bindings in
          let t2' =
            if List.exists (fun (f,_,_) -> Id.same f x) bindings
            then t2
            else subst x t t2
          in
            make_let bindings' t2'
      | Let(Flag.Recursive, bindings, t2) when List.exists (fun (f,_,_) -> Id.same f x) bindings -> t'
      | Let(Flag.Recursive, bindings, t2) ->
          let aux (f,xs,t1) =
            subst_var x t f,
            List.map (subst_var x t) xs,
            if List.exists (Id.same x) xs
            then t
            else subst x t t1
          in
          let bindings' = List.map aux bindings in
          let t2' = subst x t t2 in
            make_letrec bindings' t2'
      | BinOp(op, t1, t2) ->
          let t1' = subst x t t1 in
          let t2' = subst x t t2 in
            {desc=BinOp(op, t1', t2'); typ=t'.typ}
      | Not t1 ->
          let t1' = subst x t t1 in
            make_not t1'
      | Event(s,_) -> t'
      | Record fields -> {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,subst x t t1)) fields); typ=t'.typ}
      | Proj(i,s,f,t1) -> {desc=Proj(i,s,f,subst x t t1); typ=t'.typ}
      | SetField(n,i,s,f,t1,t2) -> {desc=SetField(n,i,s,f,subst x t t1,subst x t t2); typ=t'.typ}
      | Nil -> t'
      | Cons(t1,t2) -> {desc=Cons(subst x t t1, subst x t t2); typ=t'.typ}
      | Constr(s,ts) -> {desc=Constr(s, List.map (subst x t) ts); typ=t'.typ}
      | Match(t1,pats) ->
          let aux (pat,cond,t1) = pat, cond, subst x t t1 in
            {desc=Match(subst x t t1, List.map aux pats); typ=t'.typ}
      | Raise t1 -> {desc=Raise(subst x t t1); typ=t'.typ}
      | TryWith(t1,t2) -> {desc=TryWith(subst x t t1, subst x t t2); typ=t'.typ}
      | Pair(t1,t2) -> make_pair (subst x t t1) (subst x t t2)
      | Fst t1 -> make_fst (subst x t t1)
      | Snd t1 -> make_snd (subst x t t1)
      | RandValue _ -> assert false
(*
  in
  let t''' =    {t'' with typ=subst_type x t t''.typ} in
  if Id.to_string x = "xs_31" then Format.printf "[xs_31 |-> %a](%a) = %a@.@." pp_print_term t pp_print_term' t' pp_print_term' t''';
    t'''
*)

and subst_int n t t' =
  let desc =
    match t'.desc with
        Unit -> Unit
      | True -> True
      | False -> False
      | Unknown -> Unknown
      | Int m -> if n = m then t.desc else BinOp(Add, t, {desc=Int(m-n); typ=TInt})
      | NInt y -> NInt y
      | Var y -> Var y
      | Bottom -> Bottom
      | Fun(y, t1) -> Fun(y, subst_int n t t1)
      | App(t1, ts) ->
          let t1' = subst_int n t t1 in
          let ts' = List.map (subst_int n t) ts in
            begin
              match t1'.desc with
                  App(t, ts) -> App(t, ts@ts')
                | _ -> App(t1', ts')
            end
      | If(t1, t2, t3) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
          let t3' = subst_int n t t3 in
            If(t1', t2', t3')
      | Branch(t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            Branch(t1', t2')
              (*
                | Let(flag, bindings, t2) ->
                let bindings' = List.map (fun (f,xs,t1) -> f,xs,subst_int n t t1) bindings in
                let t2' = subst_int n t t2 in
                Let(flag, bindings', t2')
              *)
      | Let(flag, bindings, t2) ->
          let bindings' = List.map (fun (f,xs,t1) -> f, xs, subst_int n t t1) bindings in
          let t2' = subst_int n t t2 in
            Let(flag, bindings', t2')
      | BinOp(Mult, t1, t2) -> (* non-linear expressions not supported *)
          BinOp(Mult, t1, t2)
      | BinOp(op, t1, t2) ->
          let t1' = subst_int n t t1 in
          let t2' = subst_int n t t2 in
            BinOp(op, t1', t2')
      | Not t1 ->
          let t1' = subst_int n t t1 in
            Not t1'
      | Event(s,b) -> Event(s,b)
      | Nil -> Nil
      | Cons(t1,t2) -> Cons(subst_int n t t1, subst_int n t t2)
      | Snd _ -> assert false
      | Fst _ -> assert false
      | Pair (_, _) -> assert false
      | TryWith (_, _) -> assert false
      | Raise _ -> assert false
      | Match (_, _) -> assert false
      | Constr (_, _) -> assert false
      | SetField (_, _, _, _, _, _) -> assert false
      | Proj (_, _, _, _) -> assert false
      | Record _ -> assert false
      | RandInt _ -> assert false
      | RandValue (_, _) -> assert false
  in
    {desc=desc; typ=t'.typ}

and subst_map map t =
  match t.desc with
      Unit -> t
    | True -> t
    | False -> t
    | Unknown -> t
    | Int n -> t
    | NInt y -> if Id.mem_assoc y map then Id.assoc y map else t
    | Bottom -> t
    | RandInt _ -> t
    | Var y -> if Id.mem_assoc y map then Id.assoc y map else t
    | Fun(y, t1) ->
        let map' = List.filter (fun (x,_) -> Id.same x y) map in
        let t1' = subst_map map' t1 in
          make_fun y t1'
    | App(t1, ts) ->
        let t1' = subst_map map t1 in
        let ts' = List.map (subst_map map) ts in
          make_app t1' ts'
    | If(t1, t2, t3) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
        let t3' = subst_map map t3 in
          make_if_ t1' t2' t3'
    | Branch(t1, t2) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
          make_branch t1' t2'
    | Let(Flag.Nonrecursive, bindings, t2) ->
        let rec aux map acc = function
            [] -> map, List.rev acc
          | (f,xs,t1)::bindings ->
              let map' = List.filter (fun (x,_) -> Id.mem x xs) map in
                aux map' ((f, xs, subst_map map' t1)::acc) bindings in
        let map',bindings' = aux map [] bindings in
        let t2' = subst_map map' t2 in
          make_let bindings' t2'
    | Let(Flag.Recursive, bindings, t2) ->
        let map' = List.filter (fun (x,_) -> List.exists (fun (f,_,_) -> Id.same f x) bindings) map in
        let aux (f,xs,t1) =
          let map'' = List.filter (fun (x,_) -> Id.mem x xs) map' in
            f, xs, subst_map map'' t1
        in
        let bindings' = List.map aux bindings in
        let t2' = subst_map map' t2 in
          make_letrec bindings' t2'
    | BinOp(op, t1, t2) ->
        let t1' = subst_map map t1 in
        let t2' = subst_map map t2 in
          {desc=BinOp(op, t1', t2'); typ=t.typ}
    | Not t1 ->
        let t1' = subst_map map t1 in
          make_not t1'
    | Event(s,_) -> t
    | Record fields -> {desc=Record (List.map (fun (f,(s,t1)) -> f,(s,subst_map map t1)) fields); typ=t.typ}
    | Proj(i,s,f,t1) -> {desc=Proj(i,s,f,subst_map map t1); typ=t.typ}
    | SetField(n,i,s,f,t1,t2) -> {desc=SetField(n,i,s,f,subst_map map t1,subst_map map t2); typ=t.typ}
    | Nil -> t
    | Cons(t1,t2) -> {desc=Cons(subst_map map t1, subst_map map t2); typ=t.typ}
    | Constr(s,ts) -> {desc=Constr(s, List.map (subst_map map) ts); typ=t.typ}
    | Match(t1,pats) ->
        let aux (pat,cond,t1) = pat, cond, subst_map map t1 in
          {desc=Match(subst_map map t1, List.map aux pats); typ=t.typ}
    | Raise t1 -> {desc=Raise(subst_map map t1); typ=t.typ}
    | TryWith(t1,t2) -> {desc=TryWith(subst_map map t1, subst_map map t2); typ=t.typ}
    | Pair(t1,t2) -> make_pair (subst_map map t1) (subst_map map t2)
    | Fst t1 -> make_fst (subst_map map t1)
    | Snd t1 -> make_snd (subst_map map t1)
    | RandValue _ -> assert false

and subst_type x t = function
    TUnit -> TUnit
  | TAbsBool -> TAbsBool
  | TBool -> TBool
  | TInt -> TInt
  | TPred(typ,ps) -> TPred(subst_type x t typ, List.map (subst x t) ps)
  | TRInt t' -> TRInt (subst x t t')
  | TVar y -> TVar y
  | TFun(y,typ) ->
      let y' = Id.set_typ y (subst_type x t (Id.typ y)) in
      let typ' = subst_type x t typ in
        TFun(y', typ')
  | TUnknown -> TUnknown
  | TList typ -> TList (subst_type x t typ)
  | TVariant _ -> assert false
  | TConstr _ -> assert false
  | TPair(typ1,typ2) -> TPair(subst_type x t typ1, subst_type x t typ2)
  | TPredAuto(y,typ) -> TPredAuto(y, subst_type x t typ)












let rec max_pat_num t =
  match t.desc with
      Unit -> 0
    | True -> 0
    | False -> 0
    | Unknown -> 0
    | Int _ -> 0
    | NInt _ -> 0
    | Var _ -> 0
    | Fun(_, t) -> max_pat_num t
    | App(t, ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) (max_pat_num t) ts
    | If(t1, t2, t3) -> max (max (max_pat_num t1) (max_pat_num t2)) (max_pat_num t3)
    | Branch(t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Let(_, bindings, t2) -> List.fold_left (fun m (_,_,t) -> max m (max_pat_num t)) (max_pat_num t2) bindings
    | BinOp(_, t1, t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Not t -> max_pat_num t
    | Event _ -> 0
    | Nil -> 0
    | Cons(t1,t2) -> max (max_pat_num t1) (max_pat_num t2)
    | Constr(_,ts) -> List.fold_left (fun acc t -> max acc (max_pat_num t)) 0 ts
    | Match(t,pats) ->
        let m = max (List.length pats) (max_pat_num t) in
        let aux acc = function
            (_,None,t) -> max acc (max_pat_num t)
          | (_,Some cond,t) -> max acc (max (max_pat_num t) (max_pat_num cond))
        in
          List.fold_left aux m pats
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | RandInt _ -> assert false
    | Bottom -> assert false

let rec max_label_num t =
  match t.desc with
      Unit -> -1
    | True -> -1
    | False -> -1
    | Unknown -> -1
    | Int _ -> -1
    | NInt _ -> -1
    | RandInt _ -> -1
    | Var _ -> -1
    | Fun(_, t) -> max_label_num t
    | App(t, ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (max_label_num t) ts
    | If(t1, t2, t3) ->
        max (max (max_label_num t1) (max_label_num t2)) (max_label_num t3)
    | Branch(t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Let(_, bindings, t2) -> List.fold_left (fun m (_,_,t) -> max m (max_pat_num t)) (max_pat_num t2) bindings
    | BinOp(_, t1, t2) -> max (max_label_num t1) (max_label_num t2)
    | Not t -> max_label_num t
    | Event _ -> -1
    | Nil -> -1
    | Cons(t1,t2) -> max (max_label_num t1) (max_label_num t2)
    | Constr(_,ts) ->
        List.fold_left (fun acc t -> max acc (max_label_num t)) (-1) ts
    | Match(t,pats) ->
        let aux acc = function
            (_,None,t) -> max acc (max_label_num t)
          | (_,Some cond,t) -> max acc (max (max_label_num t) (max_label_num cond))
        in
          List.fold_left aux (-1) pats
    | Snd _ -> assert false
    | Fst _ -> assert false
    | Pair (_, _) -> assert false
    | TryWith (_, _) -> assert false
    | Raise _ -> assert false
    | SetField (_, _, _, _, _, _) -> assert false
    | Proj (_, _, _, _) -> assert false
    | Record _ -> assert false
    | RandValue (_, _) -> assert false
    | Bottom -> assert false


let is_external x = String.contains (Id.name x) '.'


let rec is_value t =
  match t.desc with
      Unit | True | False | Int _ | NInt _ | Var _ | Nil -> true
    | _ -> false



let rec merge_typ typ1 typ2 =
  match typ1,typ2 with
      TVar{contents=Some typ1}, typ2
    | typ1, TVar{contents=Some typ2} -> merge_typ typ1 typ2
    | TVar({contents=None} as r1), TVar({contents=None} as r2) when r1 == r2 -> typ1
    | TUnit, TUnit -> TUnit
    | TBool, TBool -> TBool
    | TInt, TInt -> TInt
    | TPred(typ1,ps1), TPred(typ2,ps2) -> TPred(merge_typ typ1 typ2, ps1@@ps2)
    | TPred(typ1,ps), typ2
    | typ1, TPred(typ2,ps) -> TPred(merge_typ typ1 typ2, ps)
    | TFun(x1,typ1), TFun(x2,typ2) ->
        let x_typ = merge_typ (Id.typ x1) (Id.typ x2) in
        let x = Id.new_var (Id.name x1) x_typ in
        let typ = merge_typ (subst_type x1 (make_var x) typ1) (subst_type x2 (make_var x) typ2) in
          TFun(x, typ)
    | TList typ1, TList typ2 -> TList(merge_typ typ1 typ2)
    | TPair(typ11,typ12), TPair(typ21,typ22) -> TPair(merge_typ typ11 typ21, merge_typ typ12 typ22)
    | typ, TUnknown
    | TUnknown, typ -> typ
    | _ -> Format.printf "typ1:%a, %a@." pp_print_typ typ1 pp_print_typ typ2; assert false


let make_if t1 t2 t3 =
  assert (not Flag.check_typ || Type.can_unify t1.typ TBool);
  assert (not Flag.check_typ || Type.can_unify t2.typ t3.typ);
  match t1.desc with
      True -> t2
    | False -> t3
    | _ -> {desc=If(t1, t2, t3); typ=merge_typ t2.typ t3.typ}
let rec make_app t ts =
  match t,ts with
    | t,[]_ -> t
    | {desc=App(t1,ts1);typ=TFun(x,typ)}, t2::ts2 ->
        let typ' = subst_type x t2 typ in
          assert (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ);
          make_app {desc=App(t1,ts1@[t2]); typ=typ'} ts2
    | {typ=TFun(x,typ)}, t2::ts ->
        let typ' = subst_type x t2 typ in
          if not (not Flag.check_typ || Type.can_unify (Id.typ x) t2.typ)
          then (Format.printf "make_app: %a <=/=> %a, %a@."
                  print_typ (Id.typ x)
                  print_typ t2.typ
                  pp_print_term {desc=App(t,ts);typ=TUnit};
                assert false);
          make_app {desc=App(t,[t2]); typ=typ'} ts
    | _ when not Flag.check_typ -> {desc=App(t,ts); typ=TUnknown}
    | _ -> Format.printf "Untypable(make_app): %a@." pp_print_term {desc=App(t,ts);typ=TUnknown}; assert false
