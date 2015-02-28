(* p20 - remove the kth element of a list *)
let remove_at xs k = List.append (List.take (List.take xs k) (k-1)) (List.drop xs k)
