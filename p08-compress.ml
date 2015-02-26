(*p08 - eliminate consecutive duplicates from a list*)
let rec compress' first rest = match rest with
    | [] -> [first]
    | (x::xs) -> if (first = x) then (compress' first xs) else first::(compress' x xs)

let compress xs = match xs with
    | [] -> []
    | (x::xs') -> compress' x xs'
