(* Problem 21 - insert an element at a given position into a list *)
let insert_at xs x n = List.concat [List.take xs n; [x]; List.drop xs n]
