(* Problem 02 - Find the last and the last but one elements of a list *)
let rec last_but_one xs = match xs with 
        []  -> raise (Invalid_argument "Can't take the last two elements of an empty list")
    | (x::[]) -> raise (Invalid_argument "Can't take the last two elements of a list with only one element")
    | (x1::x2::[]) -> (x1, x2)
    | (x::xs') -> last_but_one xs'
