
exception Found;;
exception Not_Found;;

let is_character_in_string str ch =
    try
      for i = 0 to String.length str - 1 do
        if str.[i] = ch then raise Found
      done;
      false
    with Found -> true

;;

let rec member elem = function
  | [] -> false
  | x :: rest -> x = elem || member elem rest
;;

(* Note: for pedagogical purpose this exception is different from the
   standard exception Not_found *)
exception Not_Found;;

let rec associate_of x = function
  | [] -> raise Not_Found
  | (key, value) :: l ->
      if x = key then value else associate_of x l;;

let rec associate_in_list key = function
  | [] -> raise Not_Found
  | (list_of_keys, value) :: rest ->
      if member key list_of_keys then value
      else associate_in_list key rest
;;

let rec associate_of_an_element_of list_of_keys association_list =
  match list_of_keys with
  | [] -> raise Not_Found
  | key :: rest ->
     try
       associate_in_list key association_list
     with Not_Found ->
       associate_of_an_element_of rest association_list
;;

let lowercase_of ch =
  if int_of_char ch >= 65 && int_of_char ch <= 90
  then char_of_int (int_of_char ch + 32)
  else ch
;;

let lowercase_all str =
  let string_in_lowercase =
    String.create (String.length str) in
  for i = 0 to String.length str - 1 do
    string_in_lowercase.[i] <- lowercase_of str.[i]
  done;
  string_in_lowercase
;;


let substring s start finish =
  String.sub s start (finish - start + 1)
;;

let simplifications =
  [("à","a");];;

let simplify_word word =
  let new_word = String.create (String.length word) in
  let i = ref 0 and j = ref 0 in
  let rec search_translation = function
  | [] -> raise Not_Found
  | (original, translation) :: rest ->
      let length = String.length original in
      if !i + length <= String.length word
       && String.sub word !i length = original
      then (length, translation)
      else search_translation rest in
  while !i < String.length word do
    try
      let (length, translation) =
        search_translation simplifications in
      String.blit translation 0 new_word !j
                  (String.length translation);
      i := !i + length;
      j := !j + String.length translation
    with Not_Found ->
      new_word.[!j] <- word.[!i];
      i := !i + 1;
      j := !j + 1
  done;
  String.sub new_word 0 !j
;;

let divide_in_words str =
  let words = ref [] in
  let j = ref (String.length str - 1) in
  let add_word i j =
      if i <= j then
      words := simplify_word (substring str i j) :: !words in
  for i =  String.length str - 1 downto 0 do
    match str.[i] with
    | (' ' | '\n' | '.' | ',' | ';' | '-' | '!' | '?') ->
       add_word (i + 1) !j; j := i - 1
    | _ -> ()
 done;
 add_word 0 !j;   (* extraction of last word *)
 !words
;;

let farewells =
["It will be long and difficult, come back and see me often ...";
 "Your case is not simple, and even rather worrying ... Until later?";
 "Simple diagnosis: wihtout a doubt you are paranoid.";
 "With a probability of 92.37234%: polymorphous perversion.";
 "You are suffering from rapidly evolving schizophrenia, DANGER";
 "According to my calculations, your mental health has been compromised.";
 "My final advice: you must not stay that way, take care!"]
;;

let restarters =
[ "Tell me a little about yourself";]

let answers_to_simple_phrases =
[([],
  ["Do you want to change the subject?";]);
]
;;

let answers_to_small_words =
[(["isnt"],
  ["Not at all?";
   "Really not?";
   "Why not?"]);
]
;;

let answers_to_interesting_words =
[(["fear"; "fears"; "feared";
   "fright"; "frightful"; "frightening"; "frightened";
   "scare"; "scares"; "scared"; "afraid"],
  ["Talk to me about your fears";
    "Are you often afraid?";
    "Do you have unexplained fears, nightmares?"]);
]
;;

let choice_from (v:string list) = List.nth v (Random.int (List.length v));;

let message s = print_string s; print_newline ();;

let price_to_pay = ref 0;;

let hello () =
  price_to_pay := 30;
  message "\nHello, my name is Camelia.";
  message "\nI'm here to help you resolve your psychological problems.";
  message "Finish by telling me: Goodbye.";
  message "\nLet's get to it.  Tell me about yourself.\n";;

let say_goodbye () =
  message "\nThe result of my observations:\n";
  message (choice_from farewells);
  message "\nGoodbye ...\n";
  print_string "You owe me "; print_int !price_to_pay;
  message " dollars. Make out a check to the order of Camelia. Thank you."
;;

let patient_response = ref "";;

let listen_to_patient () =
  price_to_pay := !price_to_pay + 2;
  print_string ">> ";
  patient_response := read_line ()
;;

let rec synonym_of_phrase = function
  | ["how"] -> ["what"]
  | phrase -> phrase
;;


let is_goodbye ph = (ph = ["so"; "long"]) || (ph = ["bye"]) ||
  (ph = ["goodbye"]) || (ph = ["see you later"])
;;

exception Finished;;

let answer_patient () =
  let r = lowercase_all !patient_response in
  let phrase = divide_in_words r in
  if is_goodbye phrase then raise Finished
  else
  let possible_answers =(*
      try associate_of (synonym_of_phrase phrase)
                     answers_to_simple_phrases
      with Not_Found ->
      try associate_of_an_element_of phrase
          answers_to_interesting_words
      with Not_Found ->
      if is_character_in_string r '?'
      then answers_to_questions
      else *)try associate_of_an_element_of phrase
               answers_to_small_words
           with Not_Found -> restarters in
  message (choice_from (possible_answers));
  print_newline ()
;;

let camelia () =
  hello ();
  try
   while true do
     listen_to_patient ();
     answer_patient ()
   done
  with Finished -> say_goodbye ()
     | End_of_file | Sys.Break ->
         message "\n\n\nYou could be polite \
                  and say goodbye to me ...\n\n\n";
         say_goodbye ()
;;
