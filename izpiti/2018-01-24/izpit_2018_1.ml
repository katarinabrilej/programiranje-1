(* ==========================================================================
   NALOGA 1.1

   Definirajte funkcijo, ki sprejme seznam celih števil in zaporedoma izpiše
   vse elemente seznama in vrne unit.

   Primer: print_all [1; 2; 3; 4] = () in izpiše 1234
   ========================================================================== *)
  let izpisi_vsa_stevila list = 
  match list with 
  | [] -> ()
  | _ -> List.iter print_int list

(* ==========================================================================
   NALOGA 1.2

   Definirajte funkcijo, ki sprejme dva seznama in funkcijo dveh argumentov.
   Vrne naj seznam rezultatov, če funkcijo f zaporedoma uporabimo na elementih
   seznamov. Želimo, da funkcija deluje zgolj kadar sta oba vhodna seznama
   enake dolžine zato uporabite tip option.

   Za maksimalno število točk naj bo funkcija repno rekurzivna.

   Primer: map2_opt [1; 2; 3] [7; 5; 3] (+) = Some [8; 7; 6]
           map2_opt [1; 2; 3] [3; 2] (+) = None
   ========================================================================== *)
  
   let map2_opt f list1 list2 = 
    let rec aux acc f list1 list2 = 
      match list1,list2 with
      | [],[] -> Some(acc)
      | _,[] | [],_ -> None
      | x :: xs, y :: ys -> aux (f x y :: acc) f xs ys
    in aux [] f list1 list2