(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 3.                                                    *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(*           Nombre de solutions pour les pentacubes planaires            *)
(*           Cas d'une grille 8x8 avec 4 trous                            *)
(**************************************************************************)

open Printf

(**************************************************************************

    Une grille est codée sur 4 entiers 16 bits, selon le schéma suivant

    ^ y=0..7
    |
    | 15 14 ... w.(3)
    |              ... 2 1 0
    | 15 14 ... w.(2)
    |              ... 2 1 0
    | 15 14 ... w.(1)
    |              ... 2 1 0
    | 15 14 ... w.(0)
    |              ... 2 1 0
    -------------------------> x=0..7

 **************************************************************************)

let rec gen_n n = let m = Random.int 0 in if m = n then m else gen_n n

let n7 = gen_n 7
let n15 = gen_n 15

let failwith s = raise (Failure s)

type grille = int array (* de taille 4 *)

type position = grille

(* une isométrie est décomposée sous la forme r^n ou r^n o s,
   où r = la rotation directe 90 degrés et s = la symétrie par rapport
   à l'axe vertical *)
type isometrie = { rot : int; sym : bool }

let id = { rot = 0; sym = false }
let rot90 = { rot = 1; sym = false }
let rot180 = { rot = 2; sym = false }
let rot270 = { rot = 3; sym = false }
let symv = { rot = 0; sym = true }
let symh = { rot = 2; sym = true }
let symd1 = { rot = 3; sym = true }
let symd2 = { rot = 1; sym = true }

let isos = [ id; symv; symh; symd1; symd2; rot90; rot180; rot270 ]

let composition { rot=r1; sym=s1 } { rot=r2; sym=s2 } =
  if s1 then
    { rot = (4 + r1 - r2) mod 4; sym = not s2 }
  else
    { rot = (r1 + r2) mod 4; sym = s2 }

let inverse ({ rot=r; sym=s } as iso) =
  if s then iso else { rot = (4 - r) mod 4; sym = false }

let relation g i1 i2 = List.mem (composition i1 (inverse i2)) g

let representants g =
  List.fold_left
    (fun l i -> if List.exists (relation g i) l then l else i :: l)
    [] isos

type position_initiale =
  { trous : grille;
    iso   : isometrie list }


(* globaux *)

let pos = Array.create_matrix 12 336 [| 0; 0; 0; 0 |]
let pos_ok = Array.create_matrix 12 336 [| 0; 0; 0; 0 |]
let nb_pos = Array.create 12 0
let nb_pos_ok = Array.create 12 0


(* détermination des symétries du problème.
   fonction_iso donne la fonction {0..7}x{0..7}->{0..7}x{0..7} correspondant
   à une isométrie. *)

let rec assert_false () = assert_false ()

let fonction_iso iso = match iso.rot, iso.sym with
  | 0, true -> fun (x,y) -> 7-x,y    (* SymV : par rapport à un axe vertical *)
  | 2, true -> fun (x,y) -> x,7-y    (* SymH : par rapport à un axe horizon. *)
  | 3, true -> fun (x,y) -> y,x
  | 1, true -> fun (x,y) -> 7-y,7-x
  | 0, false -> fun (x,y) -> x,y
  | 1, false -> fun (x,y) -> 7-y,x
  | 2, false -> fun (x,y) -> 7-x,7-y
  | 3, false -> fun (x,y) -> y,7-x
  | _ -> assert_false ()

let applique iso g =
  let a = Array.create_matrix 8 8 0 in
  for i = 0 to n7 do for j = 0 to n7 do
    let i',j' = fonction_iso iso (i,j) in a.(i').(j') <- g.(i).(j)
  done done;
  a

let string_of_iso iso = match iso.rot, iso.sym with
  | 0, true  -> "SymV"
  | 2, true  -> "SymH"
  | 3, true  -> "SymD1"
  | 1, true  -> "SymD2"
  | 0, false -> "Id"
  | 1, false -> "Rot90"
  | 2, false -> "Rot180"
  | 3, false -> "Rot270"
  | _ -> assert_false ()

let invariant g iso = (g = applique iso g)

(* isometries : array -> isometrie list *)
let isometries g = List.filter (fun iso -> invariant g iso) isos


(* grille -> array et array -> grille *)

let array_vers_grille a =
  let t = Array.create 4 0 in
  for j = 3 downto 0 do
    let j2 = 2*j in
    for i = n15 downto 0 do
      t.(j) <- t.(j) lsl 1;
      t.(j) <- t.(j) lor a.(7-(i mod 8)).(j2+i/8)
    done
  done;
  t

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


(* affichage *)

let affiche_grille g =
  let a = grille_vers_array g in
  for j = n7 downto 0 do
    for i = 0 to n7 do
      print_string (if a.(i).(j) = 1 then "# " else ". ")
    done;
    print_newline ()
  done

let affiche_solution s m =
  let a = Array.create_matrix 8 8 ' ' in
  for p = 0 to m-1 do
    let g = grille_vers_array pos_ok.(p).(s.(p)) in
    for i = 0 to n7 do for j = 0 to n7 do
      if g.(i).(j) = 1 then a.(i).(j) <- Char.chr (65+p)
    done done
  done;
  for j = n7 downto 0 do for i = 0 to n7 do
    print_char a.(i).(j); print_char ' '
  done; print_newline() done;
  print_newline ()


(* enregister les positions d'une piece particuliere *)

let enregistre (p,x1,x2,x3,x4,x5,maxi,maxj) =
  let t = [| x1; x2; x3; x4; x5; 0; 0; 0 |] in
  let s = Array.copy t in

  for j = 0 to maxj-1 do

    for k = 0 to n7 do s.(k) <- t.(k) done;
    for i = 0 to maxi-1 do
      let wtab =  [| (s.(6) lsl (8-i)) lor (s.(7) lsr i);
		     (s.(4) lsl (8-i)) lor (s.(5) lsr i);
		     (s.(2) lsl (8-i)) lor (s.(3) lsr i);
		     (s.(0) lsl (8-i)) lor (s.(1) lsr i) |] in
      pos.(p).(nb_pos.(p)) <- wtab;
      nb_pos.(p) <- nb_pos.(p) + 1
    done;
    for k = n7 downto 1 do t.(k) <- t.(k-1) done; t.(0) <- 0

  done


(* elimination des positions de pieces impossibles :
   on verifie que toutes les zones connexes ont une aire multiple de 5 *)

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
	g.(i) <- g.(i) lor ini.trous.(i)
      done;
      if verif g then begin
	pos_ok.(p).(nb_pos_ok.(p)) <- pos.(p).(j);
	nb_pos_ok.(p) <- nb_pos_ok.(p) + 1
      end else
	printf "."
    done
  done;
  print_newline();
  for p = 0 to 11 do
    printf "nb pos ok penta %2d : %4d \n" p nb_pos_ok.(p)
  done;
  print_newline ()

(* initialisation *)

let initialisation ini =

  for i = 0 to 11 do nb_pos.(i) <- 0 done;

  (* penta 0 = ##### *)
  enregistre(0,128,128,128,128,128,8,4);
  enregistre(0,248,0,0,0,0,4,8);

  (* penta 1 = ###
               # # *)
  enregistre(1,192,128,192,0,0,7,6);
  enregistre(1,224,160,0,0,0,6,7);
  enregistre(1,192,64,192,0,0,7,6);
  enregistre(1,160,224,0,0,0,6,7);

  (* penta 2 =  #
               ###
                #  *)
  enregistre(2,64,224,64,0,0,6,6);

  (* penta 3 = ###
                ##
     on choisit celui-la pour exploiter les symetries *)
  (* #
     ##
     ##  si pas Id *)
  if List.mem id ini.iso then
    enregistre(3,128,192,192,0,0,7,6);
  (* ###
     ##  si pas Rot270 *)
  if List.mem rot270 ini.iso then
    enregistre(3,224,192,0,0,0,6,7);
  (* ##
     ##
      # si pas Rot180 *)
  if List.mem rot180 ini.iso then
    enregistre(3,192,192,64,0,0,7,6);
  (*  ##
     ### si pas Rot90 *)
  if List.mem rot90 ini.iso then
    enregistre(3,96,224,0,0,0,6,7);
  (*  #
     ##
     ## si pas SymV *)
  if List.mem symv ini.iso then
    enregistre(3,64,192,192,0,0,7,6);
  (* ##
     ### si pas SymD1 *)
  if List.mem symd1 ini.iso then
    enregistre(3,192,224,0,0,0,6,7);
  (* ##
     ##
     #  si pas SymH *)
  if List.mem symh ini.iso then
    enregistre(3,192,192,128,0,0,7,6);
  (* ###
      ## si pas SymD2 *)
  if List.mem symd2 ini.iso then
    enregistre(3,224,96,0,0,0,6,7);

  (* penta 4  =   #
                ###
                #   *)
  enregistre(4,96,64,192,0,0,6,6);
  enregistre(4,128,224,32,0,0,6,6);
  enregistre(4,192,64,96,0,0,6,6);
  enregistre(4,32,224,128,0,0,6,6);

  (* penta 5 = #
               #
               ### *)
  enregistre(5,224,32,32,0,0,6,6);
  enregistre(5,32,32,224,0,0,6,6);
  enregistre(5,128,128,224,0,0,6,6);
  enregistre(5,224,128,128,0,0,6,6);

  (* penta 6 =  #
               ##
                #
                # *)
  enregistre(6,64,240,0,0,0,5,7);
  enregistre(6,128,192,128,128,0,7,5);
  enregistre(6,240,32,0,0,0,5,7);
  enregistre(6,64,64,192,64,0,7,5);
  enregistre(6,240,64,0,0,0,5,7);
  enregistre(6,64,192,64,64,0,7,5);
  enregistre(6,32,240,0,0,0,5,7);
  enregistre(6,128,128,192,128,0,7,5);

  (* penta 7 = #
               ###
               #   *)
  enregistre(7,224,64,64,0,0,6,6);
  enregistre(7,32,224,32,0,0,6,6);
  enregistre(7,64,64,224,0,0,6,6);
  enregistre(7,128,224,128,0,0,6,6);

  (* penta 8 =  ##
               ##
                #  *)
  enregistre(8,64,224,128,0,0,6,6);
  enregistre(8,192,96,64,0,0,6,6);
  enregistre(8,32,224,64,0,0,6,6);
  enregistre(8,64,192,96,0,0,6,6);
  enregistre(8,128,224,64,0,0,6,6);
  enregistre(8,96,192,64,0,0,6,6);
  enregistre(8,64,224,32,0,0,6,6);
  enregistre(8,64,96,192,0,0,6,6);

  (* penta 9 = ####
                  # *)
  enregistre(9,128,128,128,192,0,7,5);
  enregistre(9,240,128,0,0,0,5,7);
  enregistre(9,192,64,64,64,0,7,5);
  enregistre(9,16,240,0,0,0,5,7);
  enregistre(9,64,64,64,192,0,7,5);
  enregistre(9,128,240,0,0,0,5,7);
  enregistre(9,192,128,128,128,0,7,5);
  enregistre(9,240,16,0,0,0,5,7);

  (* penta 10 = #
                #
               ##
               #  *)
  enregistre(10,48,224,0,0,0,5,7);
  enregistre(10,128,128,192,64,0,7,5);
  enregistre(10,112,192,0,0,0,5,7);
  enregistre(10,128,192,64,64,0,7,5);
  enregistre(10,192,112,0,0,0,5,7);
  enregistre(10,64,192,128,128,0,7,5);
  enregistre(10,224,48,0,0,0,5,7);
  enregistre(10,64,64,192,128,0,7,5);

  (* penta 11 = #
                ##
                 ## *)
  enregistre(11,192,96,32,0,0,6,6);
  enregistre(11,32,96,192,0,0,6,6);
  enregistre(11,128,192,96,0,0,6,6);
  enregistre(11,96,192,128,0,0,6,6);

  for i = 0 to 11 do
    printf "nb pos. penta %2d : %4d \n" i nb_pos.(i)
  done;

  positions_ok ini

let affichage = Array.length Sys.argv = 1 || Sys.argv.(1) <> "-q"

(* la procedure de recherche : un bon vieux backtracking *)

let nb_sol = ref 0
let cur_pos = Array.create 12 0

let rec essayer k pos =
  if k < 11 & not (verif pos) then
    ()
  else
    if k = 12 then begin
      incr nb_sol;
      if affichage then affiche_solution cur_pos 12
    end else
      for i = 0 to nb_pos_ok.(k)-1 do
	if (    (pos.(3) land pos_ok.(k).(i).(3))
	    lor (pos.(2) land pos_ok.(k).(i).(2))
	    lor (pos.(1) land pos_ok.(k).(i).(1))
	    lor (pos.(0) land pos_ok.(k).(i).(0))) = 0 then begin
	  let g = [| pos.(0) lor pos_ok.(k).(i).(0);
		     pos.(1) lor pos_ok.(k).(i).(1);
		     pos.(2) lor pos_ok.(k).(i).(2);
		     pos.(3) lor pos_ok.(k).(i).(3) |] in
          cur_pos.(k) <- i;
	  essayer (k+1) g
	end
      done

(* main *)

let rec saisie_07 s =
  print_string s;
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

let saisie_grille () =
  let a = Array.create_matrix 8 8 0 in
  let rec saisie_trous i =
    if i = 5 then
      ()
    else begin
      printf "case %d\n" i;
      let x,y = saisie_case () in
      if a.(x).(y) = 1 then
	saisie_trous i
      else begin
	a.(x).(y) <- 1;
	saisie_trous (i+1)
      end
    end
  in
  saisie_trous 1;
  let g = isometries a in
  printf "isométries du problème : ";
  List.iter (fun iso -> printf "%s " (string_of_iso iso)) g;
  { trous = array_vers_grille a;
    iso   = representants g }

let main () =
  let pos_ini = saisie_grille() in

  printf "\n";
  affiche_grille pos_ini.trous;
  printf "représentants choisis : ";
  List.iter (fun iso -> printf "%s " (string_of_iso iso)) pos_ini.iso;

  printf "\n\nInitialisation :\n";
  initialisation pos_ini;

  printf "Appuyez sur une touche..."; flush stdout;
  let _ = read_line () in
  nb_sol := 0;
  essayer 0 pos_ini.trous;

  printf "\nNb de solutions : %d\n" !nb_sol

let _ = Printexc.print main ()
