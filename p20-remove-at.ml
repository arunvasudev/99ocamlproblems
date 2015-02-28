(* p20 - remove the kth element of a list *)
let remove_at xs k = List.append (List.take (List.take xs (k+1)) (k)) (List.drop xs (k+1))
