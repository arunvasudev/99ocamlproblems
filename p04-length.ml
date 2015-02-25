(* Problem 04 - find the length of a given list *)
let rec length xs = match xs with
     [] -> 0
   | (x::xs') -> 1 + (length xs')
