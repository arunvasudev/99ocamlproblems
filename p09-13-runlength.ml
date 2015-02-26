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
let runlength_encode xs = 
    let xs' = pack xs in
    List.map xs' (fun (y::ys')  -> (1 + List.length ys', y)) 

type 'a rle = 
    | One of 'a
    | Many of int * 'a

(* p11 - runlength version 2. Returns items of type rle. *)
let runlength_encode2 xs =
    let xs' = pack xs in
    List.map xs' (fun ys -> match ys with
                    | y::[] -> One y
                    | y::ys' -> Many (1 + List.length ys', y))
