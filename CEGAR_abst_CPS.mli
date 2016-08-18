
val abst_arg : CEGAR_syntax.var -> CEGAR_syntax.typ -> CEGAR_syntax.var list
val make_pts :
  CEGAR_syntax.var ->
  CEGAR_syntax.typ -> (CEGAR_syntax.t * CEGAR_syntax.t) list
val beta_reduce_term : CEGAR_syntax.t -> CEGAR_syntax.t
val beta_reduce_def :
  'a * 'b * CEGAR_syntax.t * 'c * CEGAR_syntax.t ->
  'a * 'b * CEGAR_syntax.t * 'c * CEGAR_syntax.t
val expand_non_rec : CEGAR_syntax.prog -> CEGAR_syntax.prog
val trans_eager_bool : CEGAR_syntax.var -> CEGAR_syntax.t -> CEGAR_syntax.t
val is_bool : CEGAR_syntax.env -> CEGAR_syntax.t -> bool
val trans_eager_term :
  CEGAR_syntax.env ->
  (CEGAR_syntax.t -> CEGAR_syntax.t) -> CEGAR_syntax.t -> CEGAR_syntax.t
val trans_eager_def :
  (CEGAR_syntax.var * CEGAR_syntax.typ) list ->
  CEGAR_syntax.var * CEGAR_syntax.var list * CEGAR_syntax.t * 'a *
  CEGAR_syntax.t ->
  CEGAR_syntax.var * CEGAR_syntax.var list * CEGAR_syntax.t * 'a *
  CEGAR_syntax.t
val trans_eager : CEGAR_syntax.prog -> CEGAR_syntax.prog
val eta_expand_term_aux :
  (CEGAR_syntax.var * CEGAR_syntax.typ) list ->
  CEGAR_syntax.t -> CEGAR_syntax.typ -> CEGAR_syntax.t
val eta_expand_term :
  CEGAR_syntax.env -> CEGAR_syntax.t -> CEGAR_syntax.typ -> CEGAR_syntax.t
val eta_expand_def :
  (CEGAR_syntax.var * CEGAR_syntax.typ) list ->
  CEGAR_syntax.var * CEGAR_syntax.var list * 'a * 'b * CEGAR_syntax.t ->
  CEGAR_syntax.var * CEGAR_syntax.var list * 'a * 'b * CEGAR_syntax.t
val eta_expand : CEGAR_syntax.prog -> CEGAR_syntax.prog
val eta_reduce_term : CEGAR_syntax.t -> CEGAR_syntax.t
val print_env :
  Format.formatter -> (CEGAR_syntax.var * CEGAR_syntax.typ) list -> unit
val propagate_fun_arg_typ :
  CEGAR_syntax.t CEGAR_type.t -> CEGAR_syntax.t -> CEGAR_syntax.t
val abstract_rand_int :
  int ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list option ->
  CEGAR_syntax.env ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list ->
  CEGAR_syntax.t list -> CEGAR_syntax.t -> CEGAR_syntax.t
val abstract_term :
  (CEGAR_syntax.t * CEGAR_syntax.t) list option ->
  CEGAR_syntax.env ->
  CEGAR_syntax.t list ->
  (CEGAR_syntax.t * CEGAR_syntax.t) list ->
  CEGAR_syntax.t -> CEGAR_syntax.typ -> CEGAR_syntax.t list
val abstract_typ : CEGAR_syntax.t CEGAR_type.t -> 'a CEGAR_type.t
val abstract_def :
  (CEGAR_syntax.var * CEGAR_syntax.typ) list ->
  CEGAR_syntax.var * CEGAR_syntax.var list * CEGAR_syntax.t * 'a list *
  CEGAR_syntax.t ->
  (CEGAR_syntax.var * Util.StringSet.elt list * CEGAR_syntax.t * 'a list *
   CEGAR_syntax.t)
  list
val make_br' : CEGAR_syntax.t -> CEGAR_syntax.t -> CEGAR_syntax.t
type typ_cps = X | TFun1 of typ_cps | TFun2 of typ_cps * typ_cps
val trans_typ : CEGAR_syntax.typ -> typ_cps
val make_arg : CEGAR_syntax.var list -> typ_cps -> CEGAR_syntax.t list
val add_ks :
  CEGAR_syntax.var ->
  typ_cps -> CEGAR_syntax.var list -> CEGAR_syntax.var list
val make_ext_fun_cps : CEGAR_syntax.var list -> typ_cps -> CEGAR_syntax.t
val add_ext_funs_cps : CEGAR_syntax.prog -> CEGAR_syntax.prog
val abstract_prog : CEGAR_syntax.prog -> CEGAR_syntax.prog
val pr : string -> CEGAR_syntax.prog -> unit
val abstract :
  'a ->
  'b -> CEGAR_syntax.prog -> 'c -> CEGAR_syntax.var list * CEGAR_syntax.prog
