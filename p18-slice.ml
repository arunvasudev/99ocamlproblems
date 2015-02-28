(* p18 - extract a slice (i,k) from a list, both indices included *)
let slice xs i k = List.drop (List.take xs k) (i - 1)
