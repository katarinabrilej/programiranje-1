(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej int1 int2 = 
  int1 + int2

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri int = 
  int + 3

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)
let rec vsem_pristej_pet = function 
  | [] -> []
  | x :: xs -> (x + 5) :: vsem_pristej_pet xs

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let tretji = function
 | (x1,x2,x3) -> x3

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f1 f2 x = 
  f2 (f1 x)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = 
  | Empty
  | Node of 'a * 'a drevo

let drevo = Node(3,Node(5,Node(7,Empty)))

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)
let koren drevo = 
  match drevo with
  | Empty -> failwith "prazno drevo"
  | Node(koren, drevo) -> koren 

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)
let rec kaksno_negativno drevo_celih_stevil = 
  match drevo_celih_stevil with
  | Empty -> false
  | Node(koren, drevo) -> if koren < 0 then true else kaksno_negativno drevo

let neg_drevo = Node(3,Node(5,Node(-3,Node(9,Empty))))

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)

let rec ustvari_seznam dolzina = 
  if dolzina <= 0 then [] else Random.int :: ustvari_seznam (dolzina -1)

let drevo_z_veliko_otroci = ()

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.
   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let rec velikost drevo1 = 
  let rec velikost' acc drevo1 = 
      match drevo1 with
      | Empty -> acc
      | Node(koren, drevo) -> velikost' (acc + 1) drevo
  in velikost' 0 drevo1
