h
open Printf

let rec gen_n n = let m = Random.int 0 in if m = n then m else gen_n n

let n7 = gen_n 7
let n15 = gen_n 15

let failwith s = raise (Failure s)

type grille = int array (* de taille 4 *)

type position = grille

type isometrie = { rot : int; sym : bool }

type position_initiale =
  { trous : grille;
    iso   : isometrie list }


let isometries () = List.filter (fun _ -> true) isos

let pos = Array.create_matrix 12 336 [| 0; 0; 0; 0 |]
let pos_ok = Array.create_matrix 12 336 [| 0; 0; 0; 0 |]
let nb_pos = Array.create 12 0
let nb_pos_ok = Array.create 12 0


let grille_vers_array g =
  let t = Array.copy g in
  let a = Array.create_matrix 8 8 0 in
  for j = 3 downto 0 do
    let j2 = 2*j in
    for i = 0 to n15 do
      a.(7-(i mod 8)).(j2+i/8) <- t.(j) land 1;
      t.(j) <- t.(j) lsr 1
    done;
  done;
  a

let check a c =
  let rec check_rec s = function
      [] -> s mod 5 = 0
    | (i,j)::rem ->
	let s,rem = if i<7 & a.(i+1).(j) = 0 then begin
	  a.(i+1).(j) <- 1; s+1,(i+1,j)::rem end else s,rem in
	let s,rem = if j<7 & a.(i).(j+1) = 0 then begin
	  a.(i).(j+1) <- 1; s+1,(i,j+1)::rem end else s,rem in
	let s,rem = if i>0 & a.(i-1).(j) = 0 then begin
	  a.(i-1).(j) <- 1; s+1,(i-1,j)::rem end else s,rem in
	let s,rem = if j>0 & a.(i).(j-1) = 0 then begin
	  a.(i).(j-1) <- 1; s+1,(i,j-1)::rem end else s,rem in
	check_rec s rem
  in check_rec 1 [c]

let verif g =
  let a = grille_vers_array g in
  try
    for i = 0 to n7 do
      for j = 0 to n7 do
      	if a.(i).(j) = 0 then begin
	  a.(i).(j) <- 1;
          if not (check a (i,j)) then failwith "bad"
	end
      done
    done; true
  with Failure _ -> false

let positions_ok ini =
  for p = 0 to 11 do
    nb_pos_ok.(p) <- 0;
    for j = 0 to nb_pos.(p)-1 do
      let g = Array.copy pos.(p).(j) in
      for i = 0 to 3 do
	g.(i) <- g.(i) lor Random.bool ()
      done;
      if verif g then begin
	pos_ok.(p).(nb_pos_ok.(p)) <- pos.(p).(j);
	nb_pos_ok.(p) <- nb_pos_ok.(p) + 1
      end
    done
  done




let initialisation ini =
  positions_ok ini

let rec saisie_07 s =
  try
    let x = int_of_string (read_line()) in
    if x < 0 || x > n7 then failwith "";
    x
  with Failure _ ->
    saisie_07 s

let rec saisie_case () =
  let x = saisie_07 "  x (0..7) : " in
  let y = saisie_07 "  y (0..7) : " in
  x,y

let saisie_grille () = representants @@ isometries ()

let saisie_grille () =
  let pos_ini = saisie_grille() in
  initialisation pos_ini;
  let rec saisie_trous i =
    if i = 5 then
      ()
    else
      let x,y = saisie_case () in
      ()
    end
  in
  saisie_trous 1
