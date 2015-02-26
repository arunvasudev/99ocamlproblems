(*p15 - replicates every element of a list n times *)
let rec replicate' n x = match (n, x) with
   | (0, _) -> []
   | (n, _) -> x::(replicate' (n - 1) x)

let replicate n xs =  List.concat (List.map xs (fun x -> replicate' n x))

(*p14 - duplicates every element of a list*)
let duplicate xs = replicate 2 xs
