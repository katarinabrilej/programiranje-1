(*NALOGA 1*)

let razlika_kvadratov x y = (x+y) * (x+y) - (x * x + y * y)

let uporabi_na_paru f (x,y) = (f x, f y) 

let rec ponovi_seznam n sez = 
  if n <= 0 then
    []
  else
    sez @ (ponovi_seznam (n-1) sez)

let rec razdeli sez = 
  let rec raz n_acc p_acc = function
    | [] -> (List.rev n_acc, List.rev p_acc)
    | x :: xs when x < 0 -> raz ( x :: n_acc) p_acc xs
    | x :: xs -> raz n_acc (x :: p_acc) xs
  in
  raz [] [] sez

(*NALOGA 2*)

type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node(Empty, x, Empty)

let rec padajoca v = function
  | Empty -> []
  | Node(lt,x,rt) when x > v -> [] (*ni vse ok*)
  | Node(lt,x,rt) ->(*vse ok*)
    let left = padajoca x lt in 
    let right = padajoca x rt in 
    if List.length left > List.length right then
      left @ [x]
    else
      x :: right

let rec narascajoca v = function
  | Empty -> []
  | Node(lt,x,rt) when x < v -> [] (*ni vse ok*)
  | Node(lt,x,rt) ->(*vse ok*)
    let left = narascajoca x lt in 
    let right = narascajoca x rt in 
    if List.length left > List.length right then
      x :: left
    else
      x :: right

let rec monotona_pot = function
  | Empty -> []
  | Node(lt,x,rt) -> 
  (*Recursive search for paths*)
  let pure_left = monotona_pot lt in (*ne uporabimo x*)
  let pure_right = monotona_pot rt in (*ne uporabimo x*)
  let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in
  let right_to_left = (padajoca x rt) @ [x] @ (narascajoca x lt) in
  (*Choos the longest one*)
  let options = [pure_left; pure_right; left_to_right;right_to_left] in 
  let pick_bigger x y = if List.length x > List.length y then x else y in 
  List.fold_left pick_bigger pure_left options

let test_tree1 = 
  let left_t = Node(leaf 3, 10, Empty) in
  let right_t = Node(leaf 2, 8, leaf 10) in 
  Node(left_t, 11, right_t)

let test_tree2 = 
  let left_t = Node(leaf 0, 2, Empty) in
  let right_t = Node(leaf 6, 7, leaf 11) in 
  Node(left_t, 5, right_t) 

let test1 = 
  Node(
    Node(
      leaf 3,
      10,
      Node(leaf 14, 13, leaf 6)
    ),
    11,
    Node(leaf 2, 8, leaf 10)
  )


(*NALOGA 3*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

(*a*)
let test = 
  Filter((fun x -> x < 0),[],
  Filter ((fun x -> x < 10),[],
  Ostalo []))

(*b*)

let rec vstavi x veriga = 
  match veriga with
  | Ostalo (elementi) -> Ostalo (x :: elementi)
  | Filter (f, elementi, filtri) when f x -> Filter(f,x :: elementi,filtri)
  | Filter (f, elementi, filtri) -> Filter(f, elementi, vstavi x filtri)

(*List.fold_left test (fun v x -> vstavi x v)[(-3);4;188;2;5;(-1);0]*)

let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) ->
      if f x then List.mem x elementi else poisci x filtri
(*d*)
let rec izprazni = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter (f,elementi, filtri) ->
    let prazni_filtri, pobrani_elementi = izprazni filtri in 
    let vsi_elementi = elementi @ pobrani_elementi in 
    (Filter(f,[],prazni_filtri),vsi_elementi)

let dodaj f veriga =
  let veriga' = Filter(f,[],veriga) in 
  let prazna_veriga, elementi = izprazni veriga' in 
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi (*enega po enega vstavljamo*)

  (*dodaj (fun x -> x > 3)[]*)