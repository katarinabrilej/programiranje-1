(*  NALOGA 1*) 

let razlika_kvadratov x y = (x + y) * (x + y) - (x * x + y * y)

let uporabi_na_paru f (x,y) = (f x , f y)

let rec ponovi_seznam n list = 
  if n <= 0 then
    []
  else
    list @ ponovi_seznam (n-1) list

let razdeli list = 
  let rec aux neg poz = function
  | [] -> (neg,poz)
  | x :: xs -> if x < 0 then aux (x :: neg) poz xs else aux neg ( x :: poz) xs
  in 
  aux [] [] list 


  










