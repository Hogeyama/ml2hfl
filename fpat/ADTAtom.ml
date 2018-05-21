open Util
open Combinator

(** Atoms on ADTs *)

(** {6 Auxiliary constructors} *)

let mk_recognizer ty x t = Atom.make (Const.Recognizer(ty, x)) [t]

let eval_accessor = Atom.term_of >> ADTTerm.eval_accessor >> Atom.of_term
