open Util

type url = string

and text =
    Text of string
  | Link of string * url
  | Emphasis of string
  | Strong of string
  | InlineCode of string
  | Image of string * url

type element = text list

type pos = Left | Center | Right

type paragraph =
    Normal of element
  | Header of int * element
  | Quote of paragraph
  | UnorderedList of element list
  | OrderdList of element list
  | Code of string option * string
  | HorizontalLine
  | Table of element list * pos list * element list list

let print_text ppf = function
    Text s -> Format.fprintf ppf "%s@?" s
  | Link(s,url) -> Format.fprintf ppf "[%s](%s)@?" s url
  | Emphasis s -> Format.fprintf ppf "_%s_@?" s
  | Strong s -> Format.fprintf ppf "__%s__@?" s
  | InlineCode s -> Format.fprintf ppf "`%s`@?" s
  | Image(s,url) -> Format.fprintf ppf "![%s](%s)@?" s url

let print_element ppf ts = List.iter (print_text ppf) ts

let rec print_paragraph ppf = function
    Normal e -> print_element ppf e
  | Header(n,e) ->
      let rec pr_sharp n =
        if n > 0
        then (Format.fprintf ppf "#"; pr_sharp (n-1))
        else Format.fprintf ppf " "
      in
      pr_sharp n;
      Format.fprintf ppf "%a@?" print_element e
  | Quote p ->
      print_paragraph Format.str_formatter p;
      let s = Format.flush_str_formatter () in
      Format.fprintf ppf "%s@?" ("> " ^ Str.global_replace (Str.regexp_string "\n") "\n> " s)
  | UnorderedList es -> List.iter (Format.fprintf ppf "* %a@." print_element) es
  | OrderdList es -> List.iter (Format.fprintf ppf "1. %a@." print_element) es
  | Code(lang,s) ->
      let header =
        match lang with
          None -> ""
        | Some name -> Format.sprintf "#!%s\n" name
      in
      Format.fprintf ppf "```@.%s%s@.```" header s
  | HorizontalLine -> Format.fprintf ppf "***"
  | Table(head,pos,body) ->
      let n = List.length head in
      let string_of_pos = function
          Left -> [Text "-"]
        | Center -> [Text ":-:"]
        | Right -> [Text "-:"]
      in
      let print_line xs =
        Format.fprintf ppf "%a@." (print_list print_element "|") xs
      in
      List.iter (fun xs -> assert (List.length xs = n)) body;
      print_line head;
      print_line @@ List.map string_of_pos pos;
      List.iter print_line body

let print_paragraphs ppf ps = List.iter (Format.fprintf ppf "%a@.@." print_paragraph) ps

let string_of_paragraph p =
  print_paragraph Format.str_formatter p;
  Format.flush_str_formatter ()
let string_of_paragraphs ps =
  print_paragraphs Format.str_formatter ps;
  Format.flush_str_formatter ()
