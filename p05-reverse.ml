(* Problem 05 - reverse a list *)
let rec reverse' xs_rev xs = match xs with 
   [] -> xs_rev
 | (x::xs') -> reverse' (x::xs_rev) xs'

let reverse xs = reverse' [] xs

