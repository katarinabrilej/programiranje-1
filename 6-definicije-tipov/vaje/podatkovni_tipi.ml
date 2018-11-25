(* ========== Vaja 3: Definicije Tipov  ========== *)

let reverse list = 
  let rec reverse' acc = function
  | [] -> acc
  | x :: xs -> reverse' (x :: acc) xs
  in 
  reverse' [] list

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute.
 Oglejmo si dva pristopa k izboljšavi varnosti pri uporabi valut.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte tipa [euro] in [dollar], kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število.
 Nato napišite funkciji [euro_to_dollar] in [dollar_to_euro], ki primerno
 pretvarjata valuti (točne vrednosti pridobite na internetu ali pa si jih
 izmislite).

 Namig: Občudujte informativnost tipov funkcij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dollar_to_euro;;
 - : dollar -> euro = <fun>
 # dollar_to_euro (Dollar 0.5);;
 - : euro = Euro 0.4305
[*----------------------------------------------------------------------------*)

type euro = Euro of float
type dollar = Dollar of float

let euro_to_dollar(Euro e) = Dollar(e /. 0.88)
let dollar_to_euro(Dollar d) = Euro(0.88 *. d)

(*slaba verzija*)
let dollar_to_euro_bad dollar = 0.2 *. dollar

let dollar_to_euro_good dollar = 
  match dollar with
  | Dollar  v -> Euro (0.2 *. v)

  (*
    let Dollar v = dollar in ....

    let d_to_e (Dollar v) = ....
  *)

(*----------------------------------------------------------------------------*]
 Definirajte tip [currency] kot en vsotni tip z konstruktorji za jen, funt
 in švedsko krono. Nato napišite funkcijo [to_pound], ki primerno pretvori
 valuto tipa [currency] v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
        Ocaml sam opozori, da je potrebno popraviti funkcijo [to_pound].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # to_pound (Yen 100.);;
 - : currency = Pound 0.007
[*----------------------------------------------------------------------------*)

type currency = 
  | Yen of float
  | Pound of float
  | Krona of float

  (*zdaj imamo tri konstruktorje zato ne moremo vec razstaviti z let, moramo z match*)
let to_pound c = 
  match c with
  | Yen v -> Pound (1. *. v)
  | Pound v -> Pound v (*se ne spremeni*)
  | Krona v -> Pound 0.
(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip [list] predstavimo s konstruktorjem za prazen seznam
 [Nil] (oz. [] v Ocamlu) in pa konstruktorjem za člen [Cons(x, xs)] (oz.
 x :: xs v Ocamlu).
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)
type 'a list =
  | Empty
  | Cons of 'a * 'a list
(*----------------------------------------------------------------------------*]
 Definirajte tip [intbool_list] z konstruktorji za:
  1.) prazen seznam,
  2.) člen z celoštevilsko vrednostjo,
  3.) člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal "[5; true; false; 7]".
[*----------------------------------------------------------------------------*)

type intbool_list = 
  | Empty
  | Int_val of int * intbool_list
  | Bool_val of bool * intbool_list
  (*Int_val, Bool_val*)
  (*vsi ti konstruktorji sprejmejo natanko en argument*)

  let primer = Int_val(5, Bool_val(true, Bool_val(false, Int_val(7,Empty)))) 

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_map f_int f_bool ib_list] preslika vrednosti [ib_list] v nov
 [intbool_list] seznam, kjer na elementih uporabi primerno od funkcij [f_int]
 oz. [f_bool].
[*----------------------------------------------------------------------------*)
(*kako bi napisali navaden map*)
let rec map f = function
  | [] -> []
  | x :: xs -> (f x) :: map f xs
(*
let rec map ugly f = function
  | Empty -> Empty
  | Cons(x, xs) -> Cons( f x, map_ugly f xs)
*)


let rec intbool_map (f_int : int -> int) (f_bool: bool -> bool) = function
  | Empty -> Empty
  | Int_val (i,tail) -> Int_val(f_int i, intbool_map f_int f_bool tail)
  | Bool_val (b,tail) -> Bool_val(f_bool b, intbool_map f_int f_bool tail)



(*
let rec intbool_map (f_int : int -> int) (f_bool: bool -> bool) = function
  | Empty -> Empty
  | Integer (i,tail) -> 
      let new_i = f_int i in
      let new_tail = intbool_map f_int f_bool tail in
      Integer (new_i, new_tail)
*)  

(*----------------------------------------------------------------------------*]
 Funkcija [intbool_reverse] obrne vrstni red elementov [intbool_list] seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse ib_list =
  let rec reverse' (acc: intbool_list) = function (*povemu mo da je akumulator ipa intbool_list*)
    | Empty -> acc
    | Int_val(i, tail) -> 
        let new_acc = Int_val(i,acc) in 
        reverse' new_acc tail
    | Bool_val (b, tail) ->
        let new_acc = Bool_val(b,acc) in 
        reverse' new_acc tail
  in
  reverse' Empty ib_list


(*----------------------------------------------------------------------------*]
 Funkcija [intbool_separate ib_list] loči vrednosti [ib_list] v par [list]
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate ib_list = 
  let rec separate' acc1 acc2  = function
  | Empty -> (reverse acc1, reverse acc2)
  | Int_val(i, tail) -> separate' (i :: acc1) acc2 tail
  | Bool_val(b, tail) -> separate' acc1 (b :: acc2)  tail

  in
  separate' [] [] ib_list


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 [magic], ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire,
 frost in arcane.

 Ko se čarodej zaposli na akademiji, se usmeri v zgodovino, poučevanje ali
 raziskovanje oz. historian, teacher in researcher. Definirajte tip
 [specialisation], ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = 
  | Fire
  | Frost
  | Arcane

type specialisation = 
  | Historian
  | Teacher
  | Researcher

(*----------------------------------------------------------------------------*]
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent,
 na koncu pa SE lahko tudi zaposli.
 Definirajte tip [status], ki določa ali je čarodej:
  a.) začetnik [Newbie],
  b.) študent [Student] (in kateri vrsti magije pripada in koliko časa študira),
  c.) zaposlen [Employed] (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip [wizard] z poljem za ime in poljem za trenuten
 status.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # professor;;
 - : wizard = {name = "Matija"; status = Employed (Fire, Teacher)}
[*----------------------------------------------------------------------------*)
type status = 
  | Newbie 
  | Student of magic * int
  | Employed of magic * specialisation 

type wizard = {name : string; status : status}

let professor = {name = "Matija"; status = Employed (Fire, Teacher)}


(*----------------------------------------------------------------------------*]
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip [magic_counter], ki v posameznem polju hrani število
 uporabnikov magije.
 Nato definirajte funkcijo [update counter magic], ki vrne nov števec s
 posodobljenim poljem glede na vrednost [magic].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # update {fire = 1; frost = 1; arcane = 1} Arcane;;
 - : magic_counter = {fire = 1; frost = 1; arcane = 2}
[*----------------------------------------------------------------------------*)
type magic_counter = {fire : int; frost : int; arcane : int}

let update counter magic  =
  match magic with
  | Fire -> {counter with fire = counter.fire + 1}
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}

(*----------------------------------------------------------------------------*]
 Funkcija [count_magic] sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count_magic [professor; professor; professor];;
 - : magic_counter = {fire = 3; frost = 0; arcane = 0}
[*----------------------------------------------------------------------------*)

let rec count_magic wizard_list = 
  let rec count counter = function
    | [] -> counter
    | {name; status} :: wizards -> (
      match status with
      | Newbie -> count counter wizards
      | Student (magic, _) -> count (update counter magic) wizards
      | Employed (magic, _) -> count (update counter magic) wizards)
  in count {fire = 0; frost = 0; arcane = 0} wizard_list

(*----------------------------------------------------------------------------*]
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija.
 Funkcija [find_candidate magic specialisation wizard_list] poišče prvega
 primernega kandidata na seznamu čarodejev in vrne njegovo ime, čim ustreza
 zahtevam za [specialisation] in študira vrsto [magic]. V primeru, da ni
 primernega kandidata, funkcija vrne [None].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let jaina = {name = "Jaina"; status = Student (Frost, 4)};;
 # find_candidate Frost Researcher [professor; jaina];;
 - : string option = Some "Jaina"
[*----------------------------------------------------------------------------*)

let rec find_candidate magic specialisation wizard_list = 
  let year = 
    match specialisation with
    | Historian -> 3
    | Researcher -> 4 
    | Teacher -> 5
  in
  let rec search wizard_list = 
    match wizard_list with
    | [] -> None
    | {name; status} :: wizards ->
      match status with
      | Newbie -> search wizards 
      | Student (m, y) -> if  m = magic && y >= year then Some name else search wizards 
      | Employed (_, _) -> search wizards 

  in search wizard_list
