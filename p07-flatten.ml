(*p07 - flatten a nested tree structure*)
type 'a node =
    | One of 'a
    | Many of 'a node list

let rec flatten xs = match xs with 
    | [] -> []
    | One x::xs' -> x :: (flatten xs')
    | Many xs'::xs'' -> List.append (flatten xs') (flatten xs'')

