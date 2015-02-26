(* p09 - packs consecutive elements of a list into sublists *)
let rec pack' ys xs = match ys, xs with 
 | (_,[]) -> [ys]
 | ([], (x::xs')) -> pack' [x] xs'
 | ((y::ys'), (x::xs')) -> 
         if (x = y) 
         then (pack' (x::ys) xs') 
         else ys::(pack' [x] xs')

let pack xs = pack' [] xs 
