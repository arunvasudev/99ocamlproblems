(* p22 - generate a list consisting of all integers in a range *)
let rec range a b =
    if (a > b) then []
    else a::(range (a+1) b)
