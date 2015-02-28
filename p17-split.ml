(* Problem 17 - split a list into two parts, the first of whose length is n *)
let split xs n = (List.take xs n, List.drop xs n)
