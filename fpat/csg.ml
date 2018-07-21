open Util
open Combinator

(** CSGs *)

let rec g = function [] -> 0 | p::ps -> if p.RTPoint3.flag then 0 else 1 + (g ps)

module Union =
  struct
    let rec rt robjs =
      object
        method raytrace ray =
          let rec f n ps =
            match ps with
              [] -> []
            | p::ps' ->
                if p.RTPoint3.flag then
                  if n = 0 then
                    p::(f 1 ps')
                  else if n = 1 then
                    f 2 ps'
                  else if n = 2 then
                    f 2 ps'
                  else
                    failwith ""
                else
                  if n = 0 then
                    f 0 ps'
                  else if n = 1 then
                    p::(f 0 ps')
                  else if n = 2 then
                    f 1 ps'
                  else
                    failwith "" in
          match List.map (fun robj -> robj#raytrace ray) robjs with
            [] -> []
          | [ps] -> ps
          | ps::pss ->
              List.fold_left
                (fun ps1 ps2 ->
                  let ps' = List.merge RTPoint3.compare ps1 ps2 in
                  f (g ps') ps')
                ps
                pss
      end
  end

module Intersection =
  struct
    let rec rt robjs =
      object
        method raytrace ray =
          let rec f n ps =
            match ps with
              [] -> []
            | p::ps' ->
                if p.RTPoint3.flag then
                  if n = 0 then
                    f 1 ps'
                  else if n = 1 then
                    p::(f 2 ps')
                  else if n = 2 then
                    f 2 ps'
                  else
                    failwith ""
                else
                  if n = 0 then
                    f 0 ps'
                  else if n = 1 then
                    f 0 ps'
                  else if n = 2 then
                    p::(f 1 ps')
                  else
                    failwith "" in
          match List.map (fun robj -> robj#raytrace ray) robjs with
            [] -> []
          | [ps] -> ps
          | ps::pss ->
              List.fold_left
                (fun ps1 ps2 ->
                  let ps' = List.merge RTPoint3.compare ps1 ps2 in
                  f (g ps') ps')
                ps
                pss
      end
  end

let reverse p =
  { p with RTPoint3.normal = Vector3.neg p.RTPoint3.normal;
           RTPoint3.flag = not p.RTPoint3.flag }

module Complement =
  struct
    let rec rt robj =
      object
        method raytrace ray =
          let rec f ps =
            match ps with
              [] -> []
            | p::ps' -> (reverse p)::(f ps') in
          let ps = robj#raytrace ray in
          f ps
      end
  end

module Difference =
  struct
    let rec rt robj1 robj2 =
      object
        method raytrace ray =
          let rec f n ps1 ps2 =
            match ps1, ps2 with
              [], ps2 -> []
            | ps1, [] -> ps1
            | p1::ps1', p2::ps2' ->
                if p1.RTPoint3.distance < p2.RTPoint3.distance then
                  if p1.RTPoint3.flag then
                    if n = -1 then
                      f 0 ps1' ps2
                    else if n = 0 then
                      p1::(f 1 ps1' ps2)
                    else if n = 1 then
                      f 1 ps1' ps2 (*?*)
                    else
                      failwith ""
                  else
                    if n = -1 then
                      f (-1) ps1' ps2 (*?*)
                    else if n = 0 then
                      f (-1) ps1' ps2
                    else if n = 1 then
                      p1::(f 0 ps1' ps2)
                    else
                      failwith ""
                else
                  if p2.RTPoint3.flag then
                    if n = -1 then
                      f (-1) ps1 ps2' (*?*)
                    else if n = 0 then
                      f (-1) ps1 ps2'
                    else if n = 1 then
                      (reverse p2)::(f 0 ps1 ps2')
                    else
                      failwith ""
                  else
                    if n = -1 then
                      f 0 ps1 ps2'
                    else if n = 0 then
                      (reverse p2)::(f 1 ps1 ps2')
                    else if n = 1 then
                      f 1 ps1 ps2' (*?*)
                    else
                      failwith "" in
          let ps1 = robj1#raytrace ray in
          let ps2 = robj2#raytrace ray in
          f ((g ps1) - (g ps2)) ps1 ps2
      end

      (*
       * complementで生成される可能性のある(-\infty,\infty)と
       * \emptysetの区別がつかないから次のコードは間違い
       *)
      (*Intersection.rt [robj1; Complement.rt robj2]*)
  end
