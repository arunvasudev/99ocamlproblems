(* p09 - packs consecutive elements of a list into sublists *)
let rec pack' ys xs = match ys, xs with 
 | (_,[]) -> [ys]
 | ([], (x::xs')) -> pack' [x] xs'
 | (y::ys', x::xs') -> 
         if (x = y) 
         then (pack' (x::ys) xs') 
         else ys::(pack' [x] xs')

let pack xs = pack' [] xs 

(* p10 - returns the runlength encoding of a list *)
let rec runlength_encode xs = 
    let xs' = pack xs in
    List.map xs' (fun (y::ys')  -> (y, 1 + List.length ys')) 
