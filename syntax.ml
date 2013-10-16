
open Format
open Util
open Type

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult

type typ = typed_term Type.t
and id = typ Id.t

and const = (* only base type constants *)
    Unit
  | True
  | False
  | Int of int
  | Char of char
  | String of string
  | Float of string
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | CPS_result

and typed_term = {desc:term; typ:typ}
and term =
    Const of const
  | Unknown
  | RandInt of bool
  | RandValue of typ * bool
  | Var of id
  | Fun of id * typed_term
  | App of typed_term * typed_term list
  | If of typed_term * typed_term * typed_term
  | Branch of typed_term * typed_term
  | Let of rec_flag * (id * id list * typed_term) list * typed_term
  | BinOp of binop * typed_term * typed_term
  | Not of typed_term
  | Event of string * bool
  | Record of (string * (mutable_flag * typed_term)) list
  | Proj of int * string * mutable_flag * typed_term
  | SetField of int option * int * string * mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Pair of typed_term * typed_term
  | Fst of typed_term
  | Snd of typed_term
  | Bottom
  | Label of info * typed_term

and info =
    InfoInt of int
  | InfoString of string
  | InfoId of id
  | InfoTerm of typed_term

and rec_flag = Nonrecursive | Recursive
and mutable_flag = Immutable | Mutable


and type_kind =
    KAbstract
  | KVariant of (string * typ list) list
  | KRecord of (string * (mutable_flag * typ)) list

and pred = term

and typed_pattern = {pat_desc:pattern; pat_typ:typ}
and pattern =
    PAny
  | PVar of id
  | PAlias of typed_pattern * id
  | PConst of typed_term
  | PConstruct of string * typed_pattern list
  | PNil
  | PCons of typed_pattern * typed_pattern
  | PPair of typed_pattern * typed_pattern
  | PRecord of (int * (string * mutable_flag * typed_pattern)) list
  | POr of typed_pattern * typed_pattern

type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int


exception Feasible of typed_term
exception Infeasible


let rec get_vars_pat pat =
  match pat.pat_desc with
      PAny -> []
    | PVar x -> [x]
    | PAlias(p,x) -> x :: get_vars_pat p
    | PConst _ -> []
    | PConstruct(_,pats) -> List.fold_left (fun acc pat -> get_vars_pat pat @@@ acc) [] pats
    | PRecord pats -> List.fold_left (fun acc (_,(_,_,pat)) -> get_vars_pat pat @@@ acc) [] pats
    | POr(p1,p2) -> get_vars_pat p1 @@@ get_vars_pat p2
    | PPair(p1,p2) -> get_vars_pat p1 @@@ get_vars_pat p2
    | PCons(p1,p2) -> get_vars_pat p1 @@@ get_vars_pat p2
    | PNil -> []

let rec get_fv vars t =
  match t.desc with
      Const _ -> []
    | Unknown -> []
    | RandInt _ -> []
    | Var x -> if Id.mem x vars then [] else [x]
    | App(t, ts) -> get_fv vars t @@@ rev_map_flatten (get_fv vars) ts
    | If(t1, t2, t3) -> get_fv vars t1 @@@ get_fv vars t2 @@@ get_fv vars t3
    | Branch(t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Let(flag, bindings, t2) ->
        let vars_with_fun = List.fold_left (fun vars (f,_,_) -> f::vars) vars bindings in
        let vars' = match flag with Nonrecursive -> vars | Recursive -> vars_with_fun in
        let aux fv (_,xs,t) = get_fv (xs@@@vars') t @@@ fv in
        let fv_t2 = get_fv vars_with_fun t2 in
          List.fold_left aux fv_t2 bindings
    | BinOp(op, t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Not t -> get_fv vars t
    | Fun(x,t) -> get_fv (x::vars) t
    | Event(s,_) -> []
    | Record fields -> List.fold_left (fun acc (_,(_,t)) -> get_fv vars t @@@ acc) [] fields
    | Proj(_,_,_,t) -> get_fv vars t
    | SetField(_,_,_,_,t1,t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Nil -> []
    | Cons(t1, t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Constr(_,ts) -> List.fold_left (fun acc t -> get_fv vars t @@@ acc) [] ts
    | Match(t,pats) ->
        let aux acc (pat,cond,t) =
          let vars' = get_vars_pat pat @@@ vars in
          get_fv vars' cond @@@ get_fv vars' t @@@ acc
        in
          List.fold_left aux (get_fv vars t) pats
    | TryWith(t1,t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Bottom -> []
    | Pair(t1,t2) -> get_fv vars t1 @@@ get_fv vars t2
    | Fst t -> get_fv vars t
    | Snd t -> get_fv vars t
    | Raise t -> get_fv vars t
    | RandValue _ -> []
    | Label _ -> assert false
let get_fv ?(cmp=Id.compare) t = uniq ~cmp (get_fv [] t)


let rec occur (x:id) = function
    TUnit -> false
  | TBool -> false
  | TAbsBool -> false
  | TInt -> false
  | TRInt p -> assert false
  | TVar{contents=None} -> false
  | TVar{contents=Some typ} -> occur x typ
  | TFun(y,typ) -> occur x (Id.typ y) || occur x typ
  | TList typ -> occur x typ
  | TPair(y,typ) -> occur x (Id.typ y) || occur x typ
  | TConstr(s,b) -> false
  | TPred(y,ps) -> List.exists (fun p -> List.exists (Id.same x) (get_fv p)) ps || occur x (Id.typ y)



(*** PRINTING FUNCTIONS ***)

let rec print_typ t = Type.print ~occur (print_term 0 false) t
and print_ids fm xs =
  if !Flag.web then
    let rec aux xs =
      match xs with
	[] -> ()
      | [x] ->
	fprintf fm "%a" Id.print x
      | x1 :: x2 :: xs ->
	let _ =
	  if is_fun_typ (Id.typ x2) then
	    fprintf fm "$%a$ " Id.print x1
	  else
	    fprintf fm "%a " Id.print x1
	in
	aux (x2 :: xs)
    in
    aux xs
  else
    match xs with
      [] -> ()
    | x::xs ->
      fprintf fm "%a %a" Id.print x print_ids xs

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
   10 : Let, Letrec, If, Match, TryWith
   20 : Fun
   30 : Or
   40 : And
   50 : Eq, Lt, Gt, Leq, Geq
   60 : Add, Sub
   70 : Cons, Raise
   80 : App
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

and print_termlist pri typ fm ts =
  if !Flag.web then
    let rec aux ts =
      match ts with
	[] -> ()
      | [t] -> fprintf fm "@ %a" (print_term pri typ) t
      | t1 :: t2 :: ts' ->
	let _ =
	  if is_fun_typ t2.typ then
	    fprintf fm "@ $%a$" (print_term pri typ) t1
	  else
	    fprintf fm "@ %a" (print_term pri typ) t1
	in
	aux (t2 :: ts')
    in
    aux ts
  else
    List.iter
      (fun bd ->
	fprintf fm "@ %a" (print_term pri typ) bd) ts
and print_const fm = function
    Unit -> fprintf fm "()"
  | True -> fprintf fm "true"
  | False -> fprintf fm "false"
  | Int n -> fprintf fm "%d" n
  | Char c -> fprintf fm "%C" c
  | String s -> fprintf fm "%S" s
  | Float s -> fprintf fm "%s" s
  | Int32 n -> fprintf fm "%ldl" n
  | Int64 n -> fprintf fm "%LdL" n
  | Nativeint n -> fprintf fm "%ndn" n
  | CPS_result -> fprintf fm "end"

and print_term pri typ fm t =
  match t.desc with
    Const c -> print_const fm c
  | Unknown -> fprintf fm "***"
  | RandInt false -> fprintf fm "rand_int"
  | RandInt true -> fprintf fm "rand_int_cps"
  | RandValue(typ',false) -> fprintf fm "rand_val[%a] ()" print_typ typ'
  | RandValue(typ',true) -> fprintf fm "rand_val_cps[%a]" print_typ typ'
  | Var x -> print_id fm x
  | Fun(x, t) ->
    let p = 20 in
    let s1,s2 = paren pri (p+1) in
    fprintf fm "%s@[<hov 2>fun %a ->@ %a%s@]" s1 print_id x (print_term p typ) t s2
  | App(t, ts) ->
    let p = 80 in
    let s1,s2 = paren pri p in
    fprintf fm "@[<hov 2>%s%a%a%s@]" s1 (print_term p typ) t (print_termlist p typ) ts s2
  | If(t1, t2, t3) ->
    let p = 10 in
    let s1,s2 = paren pri (p+1) in
    fprintf fm "%s@[<v>if %a then@   @[%a@]@ else@   @[%a@]@]%s"
      s1 (print_term p typ) t1 (print_term p typ) t2 (print_term p typ) t3 s2
  | Branch(t1, t2) ->
    let p = 80 in
    let s1,s2 = paren pri p in
    fprintf fm "@[%sbr %a %a%s@]" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Let(_, [], _) -> assert false
  | Let(flag, bindings, t2) ->
    let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
    let p = 10 in
    let s1,s2 = paren pri (p+1) in
    let p_ids fm xs =
      if typ
      then fprintf fm "%a@ %a" print_id (List.hd xs) print_ids_typ (List.tl xs)
      else fprintf fm "%a" print_ids xs
    in
    let b = ref true in
    let print_binding fm (f,xs,t1) =
      let pre = if !b then "let" ^ s_rec else "and" in
      Format.printf "@[<hov 2>%s %a=@ %a@ @]" pre p_ids (f::xs) (print_term p typ) t1;
      b := false
    in
    let print_bindings bs = print_list print_binding "" bs in
    begin
      match t2.desc with
        Let _ -> fprintf fm "%s@[<v>@[<hov 2>%a@]@ in@ %a@]%s"
          s1 print_bindings bindings (print_term p typ) t2 s2
      | _ -> fprintf fm     "%s@[<v>@[<hov 2>%a@]@ @[<v 2>in@ @]@[<hov>%a@]@]%s"
        s1 print_bindings bindings (print_term p typ) t2 s2
    end
  | Not{desc = BinOp(Eq, t1, t2)} ->
    let p = 50 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[%a@ <>@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | BinOp(Mult, {desc=Const (Int -1)}, {desc=Var x}) ->
    let p = 60 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[-%a@]%s" s1 print_id x s2
  | BinOp(op, t1, t2) ->
    let p = match op with Add|Sub|Mult -> 60 | And -> 40 | Or -> 30 | _ -> 50 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[%a@ %a@ %a@]%s" s1 (print_term p typ) t1 print_binop op (print_term p typ) t2 s2
  | Not t ->
    let p = 60 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[not %a@]%s" s1 (print_term p typ) t s2
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
    let p = 70 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[%a::@,%a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Constr(s,ts) ->
    let p = 80 in
    let s1,s2 = paren pri p in
    if ts = []
    then pp_print_string fm s
    else fprintf fm "%s@[%s(%a)@]%s" s1 s (print_list (print_term 20 typ) ",") ts s2
  | Match(t,pats) ->
    let p = 10 in
    let s1,s2 = paren pri p in
    let aux = function
    (pat,{desc=Const True},t) -> fprintf fm "@ @[<hov 4>| @[<hov 2>%a ->@ %a@]@]"
      print_pattern pat (print_term p typ) t
      | (pat,cond,t) -> fprintf fm "@ @[<hov 4>| @[<hov 2>%a @[<hov 2>when@ %a@] ->@ %a@]@]"
        print_pattern pat (print_term p typ) cond (print_term p typ) t
    in
    fprintf fm "%s@[<v 2>match @[%a@] with" s1 (print_term p typ) t;
    List.iter aux pats;
    fprintf fm "@]%s" s2
  | Raise t ->
    let p = 70 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[raise %a@]%s" s1 (print_term p typ) t s2
  | TryWith(t1,t2) ->
    let p = 10 in
    let s1,s2 = paren pri (p+1) in
    fprintf fm "%s@[try %a with@ %a@]%s" s1 (print_term p typ) t1 (print_term p typ) t2 s2
  | Pair(t1,t2) ->
    let p = 20 in
    fprintf fm "@[(%a,@ %a)@]" (print_term p typ) t1 (print_term p typ) t2
  | Fst t ->
    let p = 80 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[fst %a@]%s" s1 (print_term p typ) t s2
  | Snd t ->
    let p = 80 in
    let s1,s2 = paren pri p in
    fprintf fm "%s@[snd %a@]%s" s1 (print_term p typ) t s2
  | Bottom -> fprintf fm "_|_"
  | Label(InfoId x, t) ->
    fprintf fm "(@[label %a %a@])" Id.print x (print_term 80 typ) t
  | Label(InfoString s, t) ->
    fprintf fm "(@[label %s %a@])" s (print_term 80 typ) t
  | Label(InfoInt n, t) ->
    fprintf fm "(@[label %d %a@])" n (print_term 80 typ) t
  | Label(InfoTerm t', t) ->
    fprintf fm "(@[label %a %a@])" (print_term 80 typ) t' (print_term 80 typ) t


and print_pattern fm pat =
  match pat.pat_desc with
    PAny -> pp_print_string fm "_"
  | PVar x -> print_id fm x
  | PAlias(p,x) -> fprintf fm "(%a as %a)" print_pattern p print_id x
  | PConst c -> print_term 1 false fm c
  | PConstruct(c,pats) ->
    let aux' = function
    [] -> ()
      | [pat] -> fprintf fm "(%a)" print_pattern pat
      | pat::pats ->
        fprintf fm "(%a" print_pattern pat;
        List.iter (fun pat -> fprintf fm ",%a" print_pattern pat) pats;
        pp_print_string fm ")"
    in
    pp_print_string fm c;
    aux' pats
  | PNil -> fprintf fm "[]"
  | PCons(p1,p2) -> fprintf fm "%a::%a" print_pattern p1 print_pattern p2
  | PRecord pats ->
    let aux' = function
    [] -> ()
      | [_,(_,_,pat)] -> fprintf fm "(%a)" print_pattern pat
      | (_,(_,_,pat))::pats ->
        fprintf fm "(%a" print_pattern pat;
        List.iter (fun (_,(_,_,pat)) -> fprintf fm ",%a" print_pattern pat) pats;
        pp_print_string fm ")"
    in
    aux' pats
  | POr(pat1,pat2) -> fprintf fm "(%a | %a)" print_pattern pat1 print_pattern pat2
  | PPair(pat1,pat2) -> fprintf fm "(%a, %a)" print_pattern pat1 print_pattern pat2
let print_term typ fm = print_term 0 typ fm

let rec print_term' pri fm t =
  fprintf fm "(";(
    match t.desc with
        Const c -> print_const fm c
      | Unknown -> fprintf fm "***"
      | RandInt false -> fprintf fm "rand_int"
      | RandInt true -> fprintf fm "rand_int_cps"
      | RandValue(typ',false) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%srand_val(%a)%s" s1 print_typ typ' s2
      | RandValue(typ',true) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%srand_val_cps(%a)%s" s1 print_typ typ' s2
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
            fprintf fm "%s@[@[if %a@]@ then @[%a@]@ else @[%a@]@]%s"
              s1 (print_term' p) t1 (print_term' p) t2 (print_term' p) t3 s2
      | Branch(t1, t2) ->
          let p = 8 in
          let s1,s2 = paren pri p in
            fprintf fm "%sbr %a %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
      | Let(flag, bindings, t2) ->
          let s_rec = match flag with Nonrecursive -> "" | Recursive -> " rec" in
          let p = 10 in
          let s1,s2 = paren pri (p+1) in
          let p_ids fm xs =
            fprintf fm "%a" print_ids_typ xs
          in
          let b = ref true in
          let print_binding fm (f,xs,t1) =
            let pre = if !b then "let" ^ s_rec else "and" in
            Format.printf "@[<hov 2>%s %a=@ %a@ @]" pre p_ids (f::xs) (print_term' p) t1;
            b := false
          in
          let print_bindings bs = print_list print_binding "" bs in
          begin
            match t2.desc with
              Let _ -> fprintf fm "%s@[<v>@[<hov 2>%a@]@ in@ %a@]%s"
                s1 print_bindings bindings (print_term' p) t2 s2
            | _ -> fprintf fm     "%s@[<v>@[<hov 2>%a@]@ @[<v 2>in@ @]@[<hov>%a@]@]%s"
                s1 print_bindings bindings (print_term' p) t2 s2
          end
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
              (pat,{desc=Const True},t) -> fprintf fm "%a -> %a@ " print_pattern' pat (print_term' p) t
            | (pat,cond,t) -> fprintf fm "%a when %a -> %a@ "
                print_pattern' pat (print_term' p) cond (print_term' p) t
          in
            fprintf fm "%smatch %a with@ " s1 (print_term' p) t;
            List.iter aux pats;
            pp_print_string fm s2
      | Raise t ->
          let p = 4 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%sraise %a%s" s1 (print_term' 1) t s2
      | TryWith(t1,t2) ->
          let p = 1 in
          let s1,s2 = paren pri (p+1) in
            fprintf fm "%stry %a with@ %a%s" s1 (print_term' p) t1 (print_term' p) t2 s2
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
      | Label _ -> assert false
  );fprintf fm ":%a)" print_typ t.typ
and print_pattern' fm pat =
  let rec aux fm pat =
    match pat.pat_desc with
        PAny -> pp_print_string fm "_"
      | PVar x -> print_id_typ fm x
      | PAlias(p,x) -> fprintf fm "(%a as %a)" print_pattern p print_id x
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

and print_termlist' pri fm = List.iter (fun bd -> fprintf fm "@ %a" (print_term' pri) bd)
let print_term' fm = print_term' 0 fm
let pp_print_term' = print_term'

let string_of_const = make_string_of print_const
let string_of_binop = make_string_of print_binop
let string_of_typ = make_string_of print_typ
let string_of_node = function
    BrNode -> assert false
  | LabNode true -> "then"
  | LabNode false -> "else"
  | FailNode -> "fail"
  | PatNode n -> "br" ^ string_of_int n
  | EventNode s -> s

let pp_print_typ = print_typ
let pp_print_term = print_term false
let pp_print_term_typ = print_term true

let print_defs fm (defs:(id * (id list * typed_term)) list) =
  let print_fundef (f, (xs, t)) =
    fprintf fm "%a %a-> %a.\n" print_id f print_ids xs pp_print_term t
  in
    List.iter print_fundef defs
