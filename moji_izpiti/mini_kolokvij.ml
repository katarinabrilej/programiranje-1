(* -------- 1 -------- *)
let rec vsota list = 
  let rec vsota' acc = function
    | [] -> acc
    | x :: xs -> vsota' (x + acc) xs
  in 
  vsota' 0 list

(* -------- 2 -------- *)
let rec narascajoce = function
  | [] -> true
  | x :: [] -> true
  | x :: y :: xs -> if y >= x then narascajoce (y :: xs) else false

(* -------- 3 -------- *)
let rec vstavi n = function
  | [] -> n :: []
  | x :: [] -> if n >= x then x :: n :: [] else n :: x :: []
  | x :: xs -> if n < x then n :: x :: xs else x :: vstavi n xs

let seznam = [0;1;1;42]

let rec urejena list = 
  let rec urejena' acc = function
    | [] -> acc
    | x :: xs -> urejena' (vstavi x acc) xs
  in 
  urejena' [] list
(* -------- 4 -------- *)

(*let rec uredi cmp list = 
  let rec uredi' acc cmp = function
    | [] -> acc
    | x :: y :: xs -> *)



(* -------- 5 -------- *)
type priority = 
  | Top
  | Group of int 

type status = 
  | Staff 
  | Passenger of priority

type flyer = { status : status ; name : string }

let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]

(* -------- 6 -------- *)
let rec uredi_potnike list = 
  let rec uredi_potnike' acc1 acc2 acc3 = function
    | [] -> acc1,acc2,acc3
    | {status; name} :: potniki -> (
      match status with
      | Staff -> uredi_potnike' ({status; name}:: acc1) acc2 acc3 potniki
      | Passenger p -> if p = Top then uredi_potnike' acc1 ({status; name}:: acc2) acc3 potniki else uredi_potnike' acc1 acc2 ({status; name}:: acc3)potniki)
  in uredi_potnike' [] [] [] list
  acc1@acc2@acc3

(* -------- 7 -------- *)

(*let rec blok = function
  | [] -> []
  | {statu; name} :: potniki -> *)