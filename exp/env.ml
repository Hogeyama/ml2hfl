(* DIRECTORIES *)
let wiki_dir = "wiki/"
let fpat_dir = "../fpat/"
let csisat_dir = "../csisat/"

(* FILENAMES *)
let program_list = "program.list"
let option_list = "option.list"
let exp_list = "exp.list"
let dummy_image = "dummy.png"

(* OPTIONS *)
let debug = ref false
let limit = ref 120
let default_option () = Format.sprintf " -limit %d" !limit
let mochi () = "./mochi.opt -exp" ^ default_option ()
let run_force = ref false
let ignore_remote = ref false

let items =
  ["filename", Markdown.Left;
   "result", Markdown.Left;
   "cycles", Markdown.Right;
   "total", Markdown.Right;
   "abst", Markdown.Right;
   "mc", Markdown.Right;
   "refine", Markdown.Right]

(* FLAGS *)
let updated : int list ref = ref []
