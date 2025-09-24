(* File Fun/ParseAndRun.fs *)

module ParseAndRun

let fromString = Parse.fromString;;

let eval = Fun.eval;;

let run e = eval e [];;

// Exercise 4.2 Write more example programs in the functional language, and test
// them in the same way as in Exercise 4.1:


// Compute the sum of the numbers from 1000 down to 1. Do this by defining a
// function sum n that computes the sum n + (n − 1) + · · · + 2 + 1. (Use straight-
// forward summation, no clever tricks.)
let FourTwoOneTxt = "
let sum n =
    if n = 0 then
        0
    else
        n + sum (n - 1)
in
    sum 1000
end
" 
let FourTwoOne = FourTwoOneTxt |> fromString |> run


// Compute the number 3^8 , that is, 3 raised to the power 8. Again, use a recursive
// function
let FourTwoTwoTxt = "
let rec n =
    if n = 0 then
        1
    else
        3 * rec (n - 1)
in
    rec 8
end
" 
let FourTwoTwo = FourTwoTwoTxt |> fromString |> run

// Compute 3^0 + 3^1 + · · · + 3^10 + 3^11 , using a recursive function (or two, if you
// prefer).
let FourTwoThreeTxt = "
let bar a =
    let foo n =
        if n = 0 then
            1
        else
            3 * foo (n - 1)
    in
        if a = 0 then
            1
        else
            foo a + bar (a - 1)
    end
in
    bar 11
end
"

let FourTwoThree = FourTwoThreeTxt |> fromString |> run

// Compute 1^8 + 2^8 + · · · + 10^8 , again using a recursive function (or two).
let FourTwoFourTxt = "
let sumPowers n =
    if n = 0 then
        0
    else
        let power8 x =
            x * x * x * x * x * x * x * x
        in
            power8 n + sumPowers (n - 1)
        end
in
    sumPowers 8
end
"

let FourTwoFour = FourTwoFourTxt |> fromString |> run