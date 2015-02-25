(* Returns the last element of a given list *)
let rec last xs = match xs with
    |  [] -> raise (Invalid_argument "Can't take the last element of an empty list")
    | (x::[]) -> x
    | (x::xs') -> last xs'
