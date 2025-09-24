module mergeSort

(*
    Inspired by <link>https://www.geeksforgeeks.org/dsa/merge-sort/</link>
*)
let merge (a, b) =
    // Recursively divide the array into singletons and merge
    let rec mergeSort (lst : 'a list) =
        // Base-cases : [] | [x]
        if lst.Length <= 1 then lst else
        // Split list in the middle and get singletons
        let left, right = List.splitAt (lst.Length / 2) lst
        let lSplit, rSplit = mergeSort left, mergeSort right
        // sort the lists
        merge lSplit rSplit
    // compare and merge, take elements from left and insert into right 
    and merge (left : 'a list) (right : 'a list) =
        match left, right with
        | [], v  | v, []-> v
        | l :: ls, rs -> insert l rs |> merge ls
    // given a list lst, insert 'a element by F#'s default comparable ordering
    and insert (element : 'a) (lst : 'a list) =
        match lst with
        | [] -> [element]
        | x :: xs when x > element -> element :: x :: xs
        | x :: xs -> x :: insert element xs
    // Hiding implementation
    a @ b |> mergeSort

[<EntryPoint>]
let main args =
    let testAndPrint (a, b) = (a, b) |> merge |> printfn "%O"
    // Tests
    testAndPrint ([3;5;12], [2;3;4;7])
    testAndPrint (["z";"a";"q";"b";"c"], ["z";"a";"q";"b";"c"])
    testAndPrint ([true; false; false], [true; false; true])
    0