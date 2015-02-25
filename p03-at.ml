(* Problem 03 - take the nth element of a given list *)
let rec at n xs = match n, xs with
    (_, []) -> raise (Invalid_argument "Can't take the nth element of an empty list")
  | (1, x::xs') -> x
  | (n, x::xs') -> at (n - 1) xs'
