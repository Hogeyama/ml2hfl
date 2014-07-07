
(** Syntax of intermediate language *)

type label = Read | Write | Close
type binop = Eq | Lt | Gt | Leq | Geq | And | Or | Add | Sub | Mult
type typ = typed_term Type.t
and id = typ Id.t
and typed_term = {desc:term; typ:typ}
and const = (* only base type constants *)
  | Unit
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
and term =
  | Const of const
  | RandInt of bool (** true denotes CPS-term *)
  | RandValue of typ * bool (** true denotes CPS-term *)
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
  | Field of int * string * mutable_flag * typed_term
  | SetField of int option * int * string * mutable_flag * typed_term * typed_term
  | Nil
  | Cons of typed_term * typed_term
  | Constr of string * typed_term list
  | Match of typed_term * (typed_pattern * typed_term * typed_term) list
  | Raise of typed_term
  | TryWith of typed_term * typed_term
  | Tuple of typed_term list
  | Proj of int * typed_term
  | Bottom
  | Label of info * typed_term
  | Ref of typed_term
  | Deref of typed_term
  | SetRef of typed_term * typed_term
  | TNone
  | TSome of typed_term

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
  | PTuple of typed_pattern list
  | PRecord of (int * (string * mutable_flag * typed_pattern)) list
  | PNone
  | PSome of typed_pattern
  | POr of typed_pattern * typed_pattern
type node = BrNode | LabNode of bool | FailNode | EventNode of string | PatNode of int



type trans =
  {mutable tr_term:      typed_term    -> typed_term;
   mutable tr_term_rec:  typed_term    -> typed_term;
   mutable tr_desc:      term          -> term;
   mutable tr_desc_rec:  term          -> term;
   mutable tr_typ:       typ           -> typ;
   mutable tr_typ_rec:   typ           -> typ;
   mutable tr_var:       id            -> id;
   mutable tr_var_rec:   id            -> id;
   mutable tr_pat:       typed_pattern -> typed_pattern;
   mutable tr_pat_rec:   typed_pattern -> typed_pattern;
   mutable tr_info:      info          -> info;
   mutable tr_info_rec:  info          -> info;
   mutable tr_const:     const         -> const;
   mutable tr_const_rec: const         -> const}

type 'a trans2 =
  {mutable tr2_term:      'a -> typed_term    -> typed_term;
   mutable tr2_term_rec:  'a -> typed_term    -> typed_term;
   mutable tr2_desc:      'a -> term          -> term;
   mutable tr2_desc_rec:  'a -> term          -> term;
   mutable tr2_typ:       'a -> typ           -> typ;
   mutable tr2_typ_rec:   'a -> typ           -> typ;
   mutable tr2_var:       'a -> id            -> id;
   mutable tr2_var_rec:   'a -> id            -> id;
   mutable tr2_pat:       'a -> typed_pattern -> typed_pattern;
   mutable tr2_pat_rec:   'a -> typed_pattern -> typed_pattern;
   mutable tr2_info:      'a -> info          -> info;
   mutable tr2_info_rec:  'a -> info          -> info;
   mutable tr2_const:     'a -> const         -> const;
   mutable tr2_const_rec: 'a -> const         -> const}

type 'a col =
  {mutable col_term:      typed_term    -> 'a;
   mutable col_term_rec:  typed_term    -> 'a;
   mutable col_desc:      term          -> 'a;
   mutable col_desc_rec:  term          -> 'a;
   mutable col_typ:       typ           -> 'a;
   mutable col_typ_rec:   typ           -> 'a;
   mutable col_var:       id            -> 'a;
   mutable col_var_rec:   id            -> 'a;
   mutable col_pat:       typed_pattern -> 'a;
   mutable col_pat_rec:   typed_pattern -> 'a;
   mutable col_info:      info          -> 'a;
   mutable col_info_rec:  info          -> 'a;
   mutable col_const:     const         -> 'a;
   mutable col_const_rec: const         -> 'a;
   mutable col_app:       'a -> 'a -> 'a;
   mutable col_empty:     'a}

type ('a,'b) col2 =
  {mutable col2_term:      'b -> typed_term    -> 'a;
   mutable col2_term_rec:  'b -> typed_term    -> 'a;
   mutable col2_desc:      'b -> term          -> 'a;
   mutable col2_desc_rec:  'b -> term          -> 'a;
   mutable col2_typ:       'b -> typ           -> 'a;
   mutable col2_typ_rec:   'b -> typ           -> 'a;
   mutable col2_var:       'b -> id            -> 'a;
   mutable col2_var_rec:   'b -> id            -> 'a;
   mutable col2_pat:       'b -> typed_pattern -> 'a;
   mutable col2_pat_rec:   'b -> typed_pattern -> 'a;
   mutable col2_info:      'b -> info          -> 'a;
   mutable col2_info_rec:  'b -> info          -> 'a;
   mutable col2_const:     'b -> const         -> 'a;
   mutable col2_const_rec: 'b -> const         -> 'a;
   mutable col2_app:       'a -> 'a -> 'a;
   mutable col2_empty:     'a}

type ('a,'b) tr_col2 =
  {mutable tr_col2_term:      'b -> typed_term    -> 'a * typed_term;
   mutable tr_col2_term_rec:  'b -> typed_term    -> 'a * typed_term;
   mutable tr_col2_desc:      'b -> term          -> 'a * term;
   mutable tr_col2_desc_rec:  'b -> term          -> 'a * term;
   mutable tr_col2_typ:       'b -> typ           -> 'a * typ;
   mutable tr_col2_typ_rec:   'b -> typ           -> 'a * typ;
   mutable tr_col2_var:       'b -> id            -> 'a * id;
   mutable tr_col2_var_rec:   'b -> id            -> 'a * id;
   mutable tr_col2_pat:       'b -> typed_pattern -> 'a * typed_pattern;
   mutable tr_col2_pat_rec:   'b -> typed_pattern -> 'a * typed_pattern;
   mutable tr_col2_info:      'b -> info          -> 'a * info;
   mutable tr_col2_info_rec:  'b -> info          -> 'a * info;
   mutable tr_col2_const:     'b -> const         -> 'a * const;
   mutable tr_col2_const_rec: 'b -> const         -> 'a * const;
   mutable tr_col2_app:       'a -> 'a -> 'a;
   mutable tr_col2_empty:     'a}

type 'a fold_tr =
  {mutable fold_tr_term:      'a -> typed_term    -> 'a * typed_term;
   mutable fold_tr_term_rec:  'a -> typed_term    -> 'a * typed_term;
   mutable fold_tr_desc:      'a -> term          -> 'a * term;
   mutable fold_tr_desc_rec:  'a -> term          -> 'a * term;
   mutable fold_tr_typ:       'a -> typ           -> 'a * typ;
   mutable fold_tr_typ_rec:   'a -> typ           -> 'a * typ;
   mutable fold_tr_var:       'a -> id            -> 'a * id;
   mutable fold_tr_var_rec:   'a -> id            -> 'a * id;
   mutable fold_tr_pat:       'a -> typed_pattern -> 'a * typed_pattern;
   mutable fold_tr_pat_rec:   'a -> typed_pattern -> 'a * typed_pattern;
   mutable fold_tr_info:      'a -> info          -> 'a * info;
   mutable fold_tr_info_rec:  'a -> info          -> 'a * info;
   mutable fold_tr_const:     'a -> const         -> 'a * const;
   mutable fold_tr_const_rec: 'a -> const         -> 'a * const}


val make_trans : unit -> trans
val make_trans2 : unit -> 'a trans2
val make_col : 'a -> ('a -> 'a -> 'a) -> 'a col
val make_col2 : 'a -> ('a -> 'a -> 'a) -> ('a,'b) col2
val make_tr_col2 : 'a -> ('a -> 'a -> 'a) -> ('a,'b) tr_col2
val make_fold_tr : unit -> 'a fold_tr

val occur : id -> typ -> bool
val get_vars_pat : typed_pattern -> id list
val get_fv : ?cmp:(id -> id -> bool) -> typed_term -> id list

(** {6 Printing} *)

val print_typ : Format.formatter -> typ -> unit
val print_id : Format.formatter -> id -> unit
val print_id_typ : Format.formatter -> id -> unit
val string_of_const : const -> string
val string_of_binop : binop -> string
val string_of_typ : typ -> string
val string_of_node : node -> string
val print_pattern : Format.formatter -> typed_pattern -> unit
val print_const : Format.formatter -> const -> unit
val print_desc : Format.formatter -> term -> unit
val print_term : Format.formatter -> typed_term -> unit
val print_term' : Format.formatter -> typed_term -> unit
val print_term_typ : Format.formatter -> typed_term -> unit
val print_defs : Format.formatter -> (id * (id list * typed_term)) list -> unit
val print_constr : Format.formatter -> typed_term -> unit
