(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let rec reverse  = function
  | [] -> []
  | x :: xs -> reverse xs @ [x]

let reverse list = 
  let rec reverse' acc = function
  | [] -> acc
  | x :: xs -> reverse' (x :: acc) xs
  in 
  reverse' [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n =
  if n <= 0 then
    []
  else
    x :: repeat x (n-1)

let rec repeat x n = 
  let rec repeat' x n acc =
    if n <= 0 then
      acc
    else
      let new_acc = x :: acc in
      repeat' x (n-1) new_acc
  in
  repeat' x n []
(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let rec range n = 
  if n < 0 then
    []
  else
    (range (n-1)) @ [n]

    
let rec range n = 
  let rec range' n acc = 
    if n < 0 then
      acc (*vrnemo vse kar smo do sedaj naredili, torej akumulator*)
    else 
      range' (n-1) (n :: acc)
  in
  range' n []

let rec test_bad n acc =
  if n < 0 then
    []
  else
    test_bad (n-1) (acc @ [0])

let rec test_good n acc =
  if n < 0 then
    acc
  else
    test_good (n-1) (0 :: acc)

(*boljše je dodajati stvari na začetek in potem na koncu obrnemo seznam*)

(*uradna rešitev*)
let range_not_tailrec n =
  let rec range_from m =
    if m > n
    then []
    else m :: (range_from (m + 1))
  in range_from 0

let range n =
   let rec range_aux n acc =
     if n < 0 then acc else range_aux (n - 1) (n :: acc)
   in
   range_aux n []


(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f(x0); f(x1); f(x2); ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+)2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
  (*vzamemo seznam in mu spremenimo vrednosti, to naredimo tako da najprej spremenimo prvega in tako dalje*)
  | [] -> []
  | x :: xs -> f x :: map f xs
(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x+2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec f list =
  let rec map_tlrec' acc f list = 
    match list with
    | [] -> acc
    | x :: xs -> map_tlrec' (f x :: acc) f xs
  in
  reverse(map_tlrec' [] f list)


(*----------------------------------------------------------------------------*]
 Funkcija [mapi] sprejme seznam in funkcijo dveh argumentov ter vrne seznam
 preslikanih vrednosti seznama, kjer kot drugi argument funkcije podamo indeks
 elementa v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let rec mapi f list  =
  let rec mapi' acc stevec f list  =
    match list with
    | [] -> acc
    | x :: xs -> mapi'(f x stevec :: acc) (stevec + 1) f xs 
  in
  reverse(mapi' [] 0 f list )

  
(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let rec zip  list1 list2 = 
  let rec zip' acc list1 list2 = 
    match list1, list2 with
    | [], [] -> acc 
    | x :: xs, y :: ys -> zip'((x, y) :: acc) xs ys
    | [], list2 -> failwith "napačna dolžina"
    | list1, [] -> failwith "napačna dolžina"
  in 
  reverse(zip' [] list1 list2)

(*uradna rešitev*)
  let rec zip list1 list2 =
    match list1, list2 with
    | [], [] -> []
    | _, [] | [], _ -> failwith "Different lengths of input lists."
    | x :: xs, y :: ys -> (x, y) :: (zip xs ys)

(*----------------------------------------------------------------------------*]
 Funkcija [zip_enum_tlrec] sprejme seznama [x_0; x_1; ...] in [y_0; y_1; ...]
 ter vrne seznam [(0, x_0, y_0); (1, x_1, y_1); ...]. Funkcija je repno
 rekurzivna. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip_enum_tlrec ["a"; "b"; "c"] [7; 3; 4];;
 - : (int * string * int) list = [(0, "a", 7); (1, "b", 3); (2, "c", 4)]
[*----------------------------------------------------------------------------*)

let rec zip_enum_tlrec list1 list2 = 
  let rec zip_enum_tlrec' acc stevec list1 list2 =
    match list1, list2 with
    | [], [] -> acc 
    | x :: xs, y :: ys -> zip_enum_tlrec'((stevec, x, y) :: acc) (stevec + 1) xs ys
    | [], list2 -> failwith "napačna dolžina"
    | list1, [] -> failwith "napačna dolžina"
  in 
  reverse(zip_enum_tlrec' [] 0 list1 list2)

(*uradna rešitev*)
  let zip_enum_tlrec list1 list2 =
    let rec zipe_aux list1 list2 i acc =
      match list1, list2 with
      | [], [] -> reverse acc
      | _, [] | [], _ -> failwith "Different lengths of input lists."
      | x :: xs, y :: ys ->
        let element = (i, x, y) in
        zipe_aux xs ys (i + 1) (element :: acc)
     in
     zipe_aux list1 list2 0 []

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
| [] -> ([],[])
| (x1, x2) :: xs -> let (list1, list2) = unzip xs in (x1 :: list1, x2 :: list2)

(*uradna rešitev*)
let rec unzip = function
  | [] -> ([], [])
  | (x, y) :: tl -> let (list1, list2) = unzip tl in (x :: list1, y :: list2)

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec list = 
  let rec unzip_tlrec' acc1 acc2 list = 
  match list with
  | [] -> (reverse(acc1), reverse(acc2))
  | x :: xs -> unzip_tlrec'(fst x :: acc1) (snd x :: acc2) xs
  in
  unzip_tlrec' [] [] list

(*uradna rešitev*)
let unzip_tlrec list =
  let rec unzip_aux list acc1 acc2 =
    match list with
    | [] -> (reverse acc1, reverse acc2)
    | (x, y) :: tl -> unzip_aux tl (x :: acc1) (y :: acc2)
  in
  unzip_aux list [] []

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f list = 
  match list with
  | [] -> failwith "premalo elementov"
  | x :: [] -> failwith "premalo elementov"
  | x :: y :: [] -> f x y
  | x :: y :: xs -> fold_left_no_acc f ((f x y) :: xs)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x n = 
  let rec apply' acc f x n = 
    if n < 0 then
      reverse(acc)
    else
      apply' (x :: acc) f (f x) (n-1)
  in
  apply' [] f x n

 (*uradna rešitev*)
let apply_sequence f x n =
  let rec apply_aux f x n acc =
    if n < 0 then
      reverse acc
    else
      apply_aux f (f x) (n - 1) (x :: acc)
  in
  apply_aux f x n []


(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let rec filter f list = 
  let rec filter' acc f list =
    match list with
    | [] -> acc
    | x :: xs -> if f x then  filter' (x :: acc) f xs else filter' acc f xs

  in 
  reverse(filter' [] f list)


(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<)3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<)8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f = function
  | [] -> false
  | x :: xs -> if f x then true else exists f xs 

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<)3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<)8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default
  | x :: xs -> if f x then x else first f default xs 

