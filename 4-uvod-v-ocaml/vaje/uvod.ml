
(* ========== Vaja 1: Uvod v OCaml  ========== *)

(*----------------------------------------------------------------------------*]
 Funkcija [penultimate_element] vrne predzadnji element danega seznama. V
 primeru prekratkega seznama vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # penultimate_element [1; 2; 3; 4];;
 - : int = 3
[*----------------------------------------------------------------------------*)

let rec ultimate_element list = 
  match list with
  | [] -> failwith "List too short" (*ce je seznam prazen*)
  | y :: [] -> y
  | x :: ys -> ultimate_element (ys)

let rec penultimate_element list = 
  match list with
  | [] -> failwith "List too short" (*ce je seznam prazen*)
  | x :: [] -> failwith "List too short" (*ce ima seznam natanko eno vrednost*)
  | x :: y :: [] -> x (*ce ima seznam natanko dva elementa*)
  | x :: y :: ys -> penultimate_element (y :: ys)

let rec penultimate_element = function
  | [] | _ :: [] -> failwith "List too short"  (*ker imamo enak ukaz za dva vzorca, lahko damo kar skupaj in ločimo z | *)
  | x :: _ :: [] -> x (*y nadomestimo z _ ker nam je vseeno kaj je za x, samo da je en element*)
  | _ :: y :: ys -> penultimate_element (y :: ys)


  (*uradna rešitev*)

  let rec penultimate_element = function
  | x :: _ :: [] -> x
  | _ :: xs -> penultimate_element xs
  | [] -> failwith "List is too short."

(*----------------------------------------------------------------------------*]
 Funkcija [get k list] poišče [k]-ti element v seznamu [list]. Številčenje
 elementov seznama (kot ponavadi) pričnemo z 0. Če je k negativen, funkcija
 vrne ničti element. V primeru prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # get 2 [0; 0; 1; 0; 0; 0];;
 - : int = 1
[*----------------------------------------------------------------------------*)
(*function uporabimo, ko imamo smao zadnji element ki ga bomo takoj matchali in ga sicer ne bomo rabili*)
let rec get k list =
  match k, list with 
  | _, [] -> failwith "List too short"
  | k, x :: xs when k <= 0 -> x (*vrnemo nicti element, ce je k <= 0, sicer gremo rekurzivno naprej*)
  | k, x :: xs -> get (k - 1) xs

let rec get k  = function (*v resnici bomo matchali samo list, sploh ne rabimo k, zato napišemo function in zbrišemo list kot argument, 
ocaml ve da bo dobil list za match iz vzorcev*)
  | [] -> failwith "List too short"
  | x :: xs when k <= 0 -> x 
  | x :: xs -> get (k - 1) xs

 (*uradna rešitev*)
 let rec get k = function
 | [] -> failwith "List is too short."
 | x :: xs -> if k <= 0 then x else get (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [double] podvoji pojavitve elementov v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # double [1; 2; 3];;
 - : int list = [1; 1; 2; 2; 3; 3]
[*----------------------------------------------------------------------------*)

let rec double = function
| [] -> []
| x :: [] -> x :: x :: []
| x :: xs -> x :: x :: double xs

(*uradna rešitev*)
let rec double = function
  | x :: xs -> x :: x :: double xs
  | [] -> []

(*----------------------------------------------------------------------------*]
 Funkcija [divide k list] seznam razdeli na dva seznama. Prvi vsebuje prvih [k]
 elementov, drugi pa vse ostale. Funkcija vrne par teh seznamov. V primeru, ko
 je [k] izven mej seznama, je primeren od seznamov prazen.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # divide 2 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2], [3; 4; 5])
 # divide 7 [1; 2; 3; 4; 5];;
 - : int list * int list = ([1; 2; 3; 4; 5], [])
[*----------------------------------------------------------------------------*)

let rec divide k list = 
  match k, list with
  | k, list when (k <= 0) -> ([], list)
  | k, [] -> ([],[])
  | k, x :: xs ->
    let (left_list, right_list) = divide (k-1) xs in 
    (*vemo, da rezultat dobimo v obliki para = tuple*, zato lahko razdelimo na dva dela*)
    (x :: left_list, right_list)


(*----------------------------------------------------------------------------*]
 Funkcija [delete k list] iz seznama izbriše [k]-ti element. V primeru
 prekratkega seznama funkcija vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # delete 3 [0; 0; 0; 1; 0; 0];;
 - : int list = [0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec delete k list = 
  match k, list with
  | k, [] -> failwith "List too short"
  | k, list when (k < 0) -> list
  | 0, x :: xs -> xs
  | k, x :: xs -> x :: delete (k-1) xs

(*----------------------------------------------------------------------------*]
 Funkcija [slice i k list] sestavi nov seznam, ki vsebuje elemente seznama
 [list] od vključno [i]-tega do izključno [k]-tega. Predpostavimo, da sta [i] in
 [k] primerna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # slice 3 6 [0; 0; 0; 1; 2; 3; 0; 0];;
 - : int list = [1; 2; 3]
[*----------------------------------------------------------------------------*)

let rec slice i k list = 
  match i, k, list with
  | i ,k, [] -> []
  | i, k , list ->
    let (levi, desni) = divide (i) list in
    let (levi2, desni2) = divide (k-i) desni in
    levi2

(*uradna rešitev*)
let slice i k list =
  let (_, slice1) = divide i list in
  let (slice2, _) = divide (k - i) slice1 in
  slice2


(*----------------------------------------------------------------------------*]
 Funkcija [insert x k list] na [k]-to mesto seznama [list] vrine element [x].
 Če je [k] izven mej seznama, ga funkcija doda na začetek oziroma na konec.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 1 3 [0; 0; 0; 0; 0];;
 - : int list = [0; 0; 0; 1; 0; 0]
 # insert 1 (-2) [0; 0; 0; 0; 0];;
 - : int list = [1; 0; 0; 0; 0; 0]
[*----------------------------------------------------------------------------*)

let rec insert x k list = 
  match x, k, list with
  | x, k, [] -> x :: []
  | x, k, list when k <= 0 -> x :: list
  | x, k, head :: tail -> head :: insert x (k-1) tail


(*uradna rešitev*)
let rec insert x k = function
  | [] -> [x]
  | y :: ys -> if k <= 0 then x :: y :: ys else y :: insert x k ys

(*----------------------------------------------------------------------------*]
 Funkcija [rotate n list] seznam zavrti za [n] mest v levo. Predpostavimo, da
 je [n] v mejah seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # rotate 2 [1; 2; 3; 4; 5];;
 - : int list = [3; 4; 5; 1; 2]
[*----------------------------------------------------------------------------*)

let rec rotate n list = 
  let (levi, desni) = divide (n) list in 
  desni @ levi


(*----------------------------------------------------------------------------*]
 Funkcija [remove x list] iz seznama izbriše vse pojavitve elementa [x].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # remove 1 [1; 1; 2; 3; 1; 2; 3; 1; 1];;
 - : int list = [2; 3; 2; 3]
[*----------------------------------------------------------------------------*)

let rec remove x = function 
  | [] -> []
  | head :: tail -> if head = x then remove x tail else head :: remove x tail
  

(*----------------------------------------------------------------------------*]
 Funkcija [is_palindrome] za dani seznam ugotovi ali predstavlja palindrom.
 Namig: Pomagaj si s pomožno funkcijo, ki obrne vrstni red elementov seznama.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_palindrome [1; 2; 3; 2; 1];;
 - : bool = true
 # is_palindrome [0; 0; 1; 0];;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec reverse = function
| [] -> []
| x :: xs -> reverse xs @ [x]

let is_palindrome list = 
  let reversed = reverse list in 
  list = reversed 

(*uradna rešitev*)
let is_palindrome list =
  let rec reverse = function
    | x :: xs -> reverse xs @ [x]
	  | [] -> []
  in
  list = reverse list



(*----------------------------------------------------------------------------*]
 Funkcija [max_on_components] sprejme dva seznama in vrne nov seznam, katerega
 elementi so večji od istoležnih elementov na danih seznamih. Skupni seznam ima
 dolžino krajšega od danih seznamov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_on_components [5; 4; 3; 2; 1] [0; 1; 2; 3; 4; 5; 6];;
 - : int list = [5; 4; 3; 3; 4]
[*----------------------------------------------------------------------------*)

let rec max_on_components list1 list2 = 
  match list1, list2 with
  | [], _ -> []
  | _, [] -> []
  | x :: xs, y :: ys -> if x > y then x :: max_on_components xs ys else y :: max_on_components xs ys

(*uradna rešitev*)
let rec max_on_components list1 list2 =
  match (list1, list2) with
  | (x :: xs, y :: ys) -> max x y :: max_on_components xs ys
  | _ -> []

(*----------------------------------------------------------------------------*]
 Funkcija [second_largest] vrne drugo največjo vrednost v seznamu. Pri tem se
 ponovitve elementa štejejo kot ena vrednost. Predpostavimo, da ima seznam vsaj
 dve različni vrednosti.
 Namig: Pomagaj si s pomožno funkcijo, ki poišče največjo vrednost v seznamu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # second_largest [1; 10; 11; 11; 5; 4; 10];;
 - : int = 10
[*----------------------------------------------------------------------------*)
let rec largest list = 
  match list with
  | [] -> failwith "List is too short."
  | x :: [] -> x
  | x :: xs -> max x (largest xs) 

let rec second_largest list = 
  let krajsi = remove (largest list) list in
  largest krajsi

(*uradna rešitev*)
let second_largest list =
  let rec largest = function
    | [] -> failwith "List is too short."
	  | x :: [] -> x
	  | x :: xs -> max x (largest xs)
  in
  largest (delete (largest list) list)