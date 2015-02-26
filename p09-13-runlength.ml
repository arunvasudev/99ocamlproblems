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
    List.map xs' (fun ys  -> match ys with 
                    | [] -> raise (Invalid_argument "Not expecting a null list")
                    | y::ys' -> (List.length ys, y)) 

type 'a rle = 
    | One of 'a
    | Many of int * 'a

(* p11 - runlength version 2. Returns items of type rle. *)
let runlength_encode2 xs =
    let xs' = pack xs in
    List.map xs' (fun ys -> match ys with
                    | [] -> raise (Invalid_argument "Not expecting a null list")
                    | y::[] -> One y
                    | y::ys' -> Many (1 + List.length ys', y))

(* p12 - decodes a runlength encoding as produced in p11 *)
let rec replicate n x = match (n,x) with
    | (0, _) -> []
    | _ -> x::(replicate (n - 1) x)

let unpack_rle rle = match rle with
    | One x -> [x]
    | Many (n, x) -> replicate n x

let runlength_decode rles = List.concat (List.map rles unpack_rle)
