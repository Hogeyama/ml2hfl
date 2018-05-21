open Term

(** ML expressions *)

(** {6 Auxiliary constructors} *)

val mk_var : Idnt.t -> t
val mk_app : t -> t list -> t
val mk_const : Const. t -> t

val mk_fix_pat : (Pattern.t * Type.t) -> t -> t
val mk_fix : TypEnv.elem -> t -> t

val mk_fun_pat : (Pattern.t * Type.t) list -> t -> t
val mk_fun : TypEnv.t -> t -> t

val mk_if : Type.t -> t -> t -> t -> t
val mk_cases : Type.t -> (t * t) list -> t -> t

val mk_let_pat : Type.t -> (Pattern.t * Type.t) -> t -> t -> t
val mk_let : Type.t -> TypEnv.elem -> t -> t -> t
val seq : t -> t -> t

val mk_letrec_pat :
  ((Pattern.t * Type.t) * (Pattern.t * Type.t) list * t) list -> t -> t
val mk_letrec : (TypEnv.elem * TypEnv.t * t) list -> t -> t
(** no mutual recursion *)
val mk_letrec_simple : (TypEnv.elem * TypEnv.t * t) list -> t -> t

val mk_recfun : Type.t -> TypEnv.elem -> TypEnv.t -> t -> t -> t
val mk_recfun_f : Type.t -> TypEnv.elem -> t -> t -> t
val mk_closure : t list -> TypEnv.elem -> t -> t

(** @todo is this necessary? *)
val mk_undef : t
val mk_top : t
val mk_bot : t

(** @todo ?*)
val mk_rand : Type.t -> TypEnv.elem -> t -> t
val mk_rand_bool : t
val mk_rand_int : t
val mk_rand_real : t
(* val mk_read_int : TypEnv.elem -> t -> t *)
val mk_read_bool : t
val mk_read_int : t
val mk_read_real : t
val mk_event : string -> t -> t
val mk_fail : t
val mk_end : t

val mk_assert : t -> t

val of_pattern : TypEnv.t -> Pattern.t -> Type.t -> t

val name_of_proj : (int * int) list -> Idnt.t
val name_of_id : Idnt.t

val mk_church_tuple : t list -> t
val mk_church_proj : (int * int) list -> t
val mk_church_proj_def : (int * int) list -> TypEnv.elem * TypEnv.t * t

val mk_church_fst : t -> t
val mk_church_snd : t -> t
val mk_id : t

(** {6 Auxiliary destructors} *)

val is_var : t -> bool
val is_const : t -> bool

val is_closure : t -> bool
val is_value : (Idnt.t -> int) -> t -> bool
val is_app : t -> bool

val let_var : t -> (Idnt.t -> 'a) -> 'a
val let_const : t -> (Const.t -> 'a) -> 'a

val let_fix : t -> (TypEnv.elem -> t -> 'a) -> 'a
val let_funs : t -> (TypEnv.t -> t -> 'a) -> 'a
val let_n_funs : int -> t -> (TypEnv.t -> t -> 'a) -> 'a
val let_let_pat : t -> (Type.t -> Pattern.t * Type.t -> t -> t -> t list -> 'a) -> 'a
val let_let : t -> (Type.t -> TypEnv.elem -> t -> t -> t list -> 'a) -> 'a
val let_letrec :
  t ->
  ((TypEnv.elem * TypEnv.t * t) list -> t -> t list -> 'a) ->
  'a
val let_closure : t -> (t list -> TypEnv.elem -> t -> t list -> 'a) -> 'a
val let_top : t -> (unit -> 'a) -> 'a
val let_bot : t -> (unit -> 'a) -> 'a
val let_event : t -> (string -> t -> t list -> 'a) -> 'a

(** {6 Morphisms} *)

val para :
  < fvar : Idnt.t -> 'a;
    fcon : Const.t -> 'a;
    fif : Type.t -> t -> 'a -> t -> 'a -> t -> 'a -> 'a;
    flet : Type.t -> (Pattern.t * Type.t) -> t -> 'a -> t -> 'a -> 'a;
    fletrec : (TypEnv.elem * TypEnv.t * t * 'a) list -> t -> 'a -> 'a;
    fevent : string -> t -> 'a -> 'a;
    fapp : t -> 'a -> t list -> 'a list -> 'a;
    ffun : TypEnv.elem -> t -> 'a -> 'a;
    ftuple : Type.t list -> t list -> 'a list -> 'a;
    fkon : TypEnv.elem -> t list -> 'a list -> 'a;
    ffix : TypEnv.elem -> t -> 'a -> 'a;
    fcls : t list -> TypEnv.elem -> t -> 'a -> 'a; 
    farray : t list -> 'a list ->  'a;
    faget : t -> 'a -> t -> 'a -> 'a;
    faset : t -> 'a -> t -> 'a -> t -> 'a -> t -> 'a -> 'a; .. > ->
  t -> 'a

val fold :
  < fvar : Idnt.t -> 'a;
    fcon : Const.t -> 'a;
    fif : Type.t -> 'a -> 'a -> 'a -> 'a;
    flet : Type.t -> (Pattern.t * Type.t) -> 'a -> 'a -> 'a;
    fletrec : (TypEnv.elem * TypEnv.t * 'a) list -> 'a -> 'a;
    fevent : string -> 'a -> 'a;
    fapp : 'a -> 'a list -> 'a;
    ffun : TypEnv.elem -> 'a -> 'a;
    ftuple : Type.t list -> 'a list -> 'a;
    fkon : TypEnv.elem -> 'a list -> 'a;
    ffix : TypEnv.elem -> 'a -> 'a;
    fcls : t list -> TypEnv.elem -> 'a -> 'a;  
    farray : 'a list ->  'a;
    faget : 'a -> 'a -> 'a;
    faset : 'a -> 'a -> 'a -> 'a -> 'a; .. > ->
  t -> 'a

val visit :
  < fvar : Idnt.t -> 'a;
    fcon : Const.t -> 'a;
    fif : Type.t -> t -> t -> t -> 'a;
    flet : Type.t -> (Pattern.t * Type.t) -> t -> t -> 'a;
    fletrec : (TypEnv.elem * TypEnv.t * t) list -> t -> 'a;
    fevent : string -> t -> 'a;
    fapp : t -> t list -> 'a;
    ffun : TypEnv.elem -> t -> 'a;
    ftuple : Type.t list -> t list -> 'a;
    fkon : TypEnv.elem -> t list -> 'a;
    ffix : TypEnv.elem -> t -> 'a;
    fcls : t list -> TypEnv.elem -> t -> 'a;
    farray : t list ->  'a;
    faget : t -> t -> 'a;
    faset : t -> t -> t -> t -> 'a; .. > ->
  t -> 'a

val map_const : (Const.t -> Const.t) -> t -> t

(** {6 Printers} *)

val pr : Format.formatter -> t -> unit
val pr_list : Format.formatter -> t list -> unit

(** {6 Operators} *)

val subst : (Idnt.t * t) list -> t -> t
val elim_fun : t -> t
val elim_def_with_mult_args : t -> t
(** @require [e] does not contain lambdas
    @require for any definition [let rec f xs = ...], the length of
    [xs] is at most 1 *)
val cps : (t -> t) -> t -> t
(** @require [exp] does not contain lambdas *)
val elim_def_with_free_vars : Idnt.t list -> (Idnt.t * TypEnv.t) list -> t -> t
(** @require [exp] does not contain lambdas *)
val lift : t -> t
(** @require [exp] does not contain lambdas *)
val inline : (Idnt.t * t option) list -> t -> t
val simplify : t -> t
val anf : t -> t
val determinize : (Idnt.t * bool) list -> t -> t

(** {6 Inspectors} *)

val boolean_angelic_nondet_ids : t -> Idnt.t list
val has_read_bool : t -> bool

(** {6 Sample expressions} *)

val fib : t
val fib_cps : t
