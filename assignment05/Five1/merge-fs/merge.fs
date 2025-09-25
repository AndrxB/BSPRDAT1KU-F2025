module mergeSort

let rec merge (xs, ys) = 
    match xs, ys with
    // append the rest of the list.
    | [], ks | ks, [] -> ks
    // compare x, y and call continue.
    | x :: xs', y :: ys' -> if x < y then x :: merge (xs', ys) else y :: merge (xs, ys')

[<EntryPoint>]
let main _ =
    let testAndPrint (a, b) = (a, b) |> merge |> printfn "%O"
    // Tests
    testAndPrint ([3;5;12], [2;3;4;7])
    testAndPrint ([false; false; true], [false; true; true])
    testAndPrint (['a'; 'b'; 'c'; 'q'; 'z'], ['a'; 'b'; 'c'; 'q'; 'z'])
    0