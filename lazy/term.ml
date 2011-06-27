open ExtList

type 'a t =
  Var of 'a * 'a Id.t
| Event of 'a * 'a Id.t
| True of 'a | False of 'a | And of 'a * 'a t * 'a t | Or of 'a * 'a t * 'a t | Not of 'a * 'a t | Lt of 'a * 'a t * 'a t | Eq of 'a * 'a t * 'a t
| Int of 'a * int | Add of 'a * 'a t * 'a t | Mul of 'a * 'a t * 'a t | Minus of 'a * 'a t
| App of 'a * 'a t * 'a t

let rec pr ppf t =
  match t with
    Var(a, x) -> Format.fprintf ppf "%a" Id.pr x
  | Event(a, x) -> Format.fprintf ppf "%a" Id.pr x
  | True(a) -> Format.fprintf ppf "true"
  | False(a) -> Format.fprintf ppf "false"
  | And(a, t1, t2) -> Format.fprintf ppf "(%a && %a)" pr t1 pr t2
  | Or(a, t1, t2) -> Format.fprintf ppf "(%a || %a)" pr t1 pr t2
  | Not(a, t) -> Format.fprintf ppf "(not %a)" pr t
  | Lt(a, t1, t2) -> Format.fprintf ppf "(%a < %a)" pr t1 pr t2
  | Eq(a, t1, t2) -> Format.fprintf ppf "(%a = %a)" pr t1 pr t2
  | Int(a, n) -> Format.fprintf ppf "%d" n
  | Add(a, t1, t2) -> Format.fprintf ppf "(%a + %a)" pr t1 pr t2
  | Mul(a, t1, t2) -> Format.fprintf ppf "(%a * %a)" pr t1 pr t2
  | Minus(a, t) -> Format.fprintf ppf "-%a" pr t
  | App(a, t1, t2) -> Format.fprintf ppf "(%a %a)" pr t1 pr t2

let string_of t =
  Format.fprintf Format.str_formatter "%a" pr t;
  Format.flush_str_formatter ()

let make_var id = Var([], Id.make id)
let make_int n = Int([], n)
let make_true = True([])
let make_false = False([])
let make_event id = Event([], Id.make id)

let add t1 t2 = Add([], t1, t2)
let mul t1 t2 = Mul([], t1, t2)
let sub t1 t2 = Add([], t1, Minus([], t2))
let eq t1 t2 = Eq([], t1, t2)
let neq t1 t2 = Not([], Eq([], t1, t2))
let lt t1 t2 = Lt([], t1, t2)
let leq t1 t2 = Or([], lt t1 t2, eq t1 t2)
let gt t1 t2 = Lt([], t2, t1)
let geq t1 t2 = Or([], gt t1 t2, eq t1 t2)

let rec apply t ts =
  match ts with
    [] ->
      t
  | t'::ts' ->
      apply (App([], t, t')) ts'

let rec fun_args t =
  match t with
    App(_, t1, t2) ->
      let f, args = fun_args t1 in
      f, args @ [t2]
  | _ ->
      t, []

let rec subst sub t =
  match t with
    Var(a, x) -> (try List.assoc x.Id.id sub with Not_found -> Var(a, x))
  | Event(a, x) -> Event(a, x)
  | True(a) -> True(a)
  | False(a) -> False(a)
  | And(a, t1, t2) -> And(a, subst sub t1, subst sub t2)
  | Or(a, t1, t2) -> Or(a, subst sub t1, subst sub t2)
  | Not(a, t) -> Not(a, subst sub t)
  | Lt(a, t1, t2) -> Lt(a, subst sub t1, subst sub t2)
  | Eq(a, t1, t2) -> Eq(a, subst sub t1, subst sub t2)
  | Int(a, n) -> Int(a, n)
  | Add(a, t1, t2) -> Add(a, subst sub t1, subst sub t2)
  | Mul(a, t1, t2) -> Mul(a, subst sub t1, subst sub t2)
  | Minus(a, t) -> Minus(a, subst sub t)
  | App(a, t1, t2) -> App(a, subst sub t1, subst sub t2)
