(* p19 - rotate a list n items to the left *)
let rotate xs n = 
    let len = List.length xs in
    let n' = abs(n) % len in
    if (n < 0) then (* to the right *)
        List.append (List.drop xs (len - n')) (List.take xs (len - n')) 
    else
        List.append (List.drop xs n') (List.take xs n')
