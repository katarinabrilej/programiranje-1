(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree = 
    | Empty
    | Node of 'a tree * 'a * 'a tree

(*
 type 'a tree = 
    | Empty
    | Leaf 'a
    | Node of 'a tree * 'a * tree

Leaf x = Node (Empty, x, Empty)
*)

let leaf x = Node(Empty, x, Empty)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let test_tree = 
    let left_t = Node(leaf 0, 2, Empty) in
    let right_t = Node(leaf 6, 7, leaf 11) in 
    Node(left_t, 5, right_t)
(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)
let rec mirror tree = 
    match tree with
    | Empty -> Empty (*prazno podrevo ne rabimo zrcalit, ker je itak prazno*)
    | Node(lt, x, rt) -> Node(mirror rt, x, mirror lt)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)
let rec size = function
    | Empty -> 0
    | Node(lt, x, rt) -> 1 + size lt + size rt

(*še repno rekurzivno*)


(*pazi kolokvij*)
let tl_rec_size tree  = 
    let rec size' acc queue = 
    (*zdaj rabimo še dodaten akumulator za vsa drevesa, ki jih rabimo še oddelat -> queue*)
    (*Pogledamo, kateri je naslednji elemtn v vrsti za obravnavo*)
        match queue with
        | [] -> acc
        | t :: ts -> (
            (*Obravnavamo drevo*)
            match t with
            | Empty -> size' acc ts (*Prazno drevo samo odstarnimo iz vrste*)
            | Node(lt, x, rt) -> 
                let new_acc = acc + 1 in (*Obravnavamo vozlišče*)
                let new_queue  = lt :: rt :: ts in (*Dodamo poddrevesa v vrsto*)
                size' new_acc new_queue
        )
    in 
    (*Zaženemo pomožno funkcijo*)
    size' 0 [tree]


let rec height = function
    | Empty -> 0
    | Node(lt, x, rt) -> 1 + max (height lt) (height rt)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)
let rec map_tree f = function
    | Empty -> Empty
    | Node(lt, x, rt) -> Node (map_tree f lt, f x , map_tree f rt)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
        | Empty -> []
        | Node(lt, x, rt) -> list_of_tree lt @ [x] @ list_of_tree rt

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec narascajoce = function
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> if y >= x then narascajoce (y :: xs) else false
  
let is_bst tree = 
    let seznam = list_of_tree tree in
    narascajoce seznam

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)
let rec member n tree =
    let rec member' n = function
        | [] -> false
        | x :: xs -> if n = x then true else member' n xs
    in
    member' n (list_of_tree tree)

let rec member n = function
    | Empty -> false
    | Node(lt,x,rt) when n == x -> true 
    | Node(lt,x,rt) when n < x -> member n lt
    | Node(lt,x,rt) when n > x -> member n rt

let rec insert n = function
    | Empty -> Node(Empty, n, Empty)
    | Node(lt, x, rt) ->  
        if n < x then
            let new_lt = insert n lt in 
            Node(new_lt, x, rt)
        else
            let new_rt = insert n rt in 
            Node(lt, x, new_rt)

(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)
let rec member2 n tree =
    let rec member' n = function
        | [] -> false
        | x :: xs -> if n = x then true else member' n xs
    in
    member' n (list_of_tree tree)

let rec member2 x = function
    | Empty -> false
    | Node(l, y, r) -> x = y || (member2 x l) || (member2 x r)

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
[*----------------------------------------------------------------------------*)

let rec minimum tree = 
    let rec minimum' = function
    | [] -> failwith "prazno drevo"
    | x :: [] -> x 
    | x :: xs -> min x (minimum' xs)
    in
    minimum'(list_of_tree tree)

let rec lepi_min = function
    | Empty -> None
    | Node(Empty,x,rt) -> Some x 
    | Node(lt,x,rt) -> lepi_min lt


let rec succ = function
    | Empty -> None
    | Node(lt, x, Empty) -> None
    | Node(lt, x, rt) -> lepi_min rt

let rec maximum = function
    | Empty -> failwith "prazno drevo"
    | Node(lt,x,Empty) -> x 
    | Node(lt,x,rt) -> maximum rt

let rec pred = function
    | Empty -> failwith "Prazno drevo"
    | Node(Empty, x, rt) -> failwith "ni manjšega elementa" 
    | Node(lt, x, rt) -> maximum lt



(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)
let rec delete x tree = 
    match tree with
    | Empty -> (*Empty case - prazno drevo*) Empty
    | Node(Empty, y, Empty) when x = y -> (*leaf case - imamo list in dobim oprazno drevo ko ga izbrišemo*) Empty
    | Node(Empty, y, rt) when x = y -> (*One sided*) rt
    | Node(lt, y, Empty) when x = y -> (*One sided*) lt   
    | Node(lt, y, rt) when x <> y -> (*Recurse deeper*)
        if x > y then
            Node(lt, y, delete x rt)
        else
            Node(delete x lt, y, rt)
    | Node(lt, y, rt) -> (*SUPER FUN CASE :D*)
        match succ tree with
        | None -> failwith "HOW IS THIS POSSIBLE?!" (*This cannot happen :D*)
        | Some z -> Node(lt, z, delete z rt) (*v desno drevo gremo poiskat succ in ga prenesemo*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)
type ('key, 'value) dict = ('key * 'value) tree

let test_dict = 
    let lt = Node(Empty,("a",0),Empty) in 
    let rt = Node(leaf ("c",-2), ("d",2), Empty) in 
    Node(lt, ("b", 1), rt)

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)
let rec dict_get key = function
    | Empty -> None
    | Node(lt, (k, v), rt) when k = key -> Some v
    | Node(lt, (k, v), rt) -> if key < k then dict_get key lt else dict_get key rt
      
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 0
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 0
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

