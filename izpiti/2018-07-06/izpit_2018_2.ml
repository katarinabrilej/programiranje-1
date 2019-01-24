(* NALOGA 1*)

let uporabi f x = f x 

let ibaropu x f = f x 

let zacetnih n list = 
  let rec aux acc n list = 
    if n <= 0 then
      Some (List.rev acc)
    else
      match list with 
      | [] -> None
      | x :: xs -> aux ( x :: acc) (n-1) xs
  in aux [] n list

(* NALOGA 2*)
  type 'a neprazen_sez = 
    | Konec of 'a 
    | Sestavljen of 'a * 'a neprazen_sez

let prvi = function
  | Konec(a) -> a 
  | Sestavljen(a,list) -> a 

let rec zadnji = function
  | Konec(a) -> a 
  | Sestavljen(a,list) -> zadnji list

let rec dolzina = function
  | Konec(a) -> 1
  | Sestavljen(a,list) -> 1 + dolzina list

let rec pretvori_v_seznam = function
  | Konec(a) -> a :: []
  | Sestavljen(a,list) -> a :: pretvori_v_seznam list

let rec zlozi f s = function
  | Konec(a) -> f s a
  | Sestavljen(a,list) -> zlozi f (f s a ) list