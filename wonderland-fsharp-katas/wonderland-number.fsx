// See the file wonderland-number.md for detailed information.
let haveSameDigits (n1:int,n2:int) =
    (string n1 |> Set.ofSeq) = (string n2 |> Set.ofSeq)

// Curried version of haveSameDigits for partial application
let haveSameDigits' n1 n2 = haveSameDigits(n1,n2)

// For some given int i, first create array of i multiplied with mults,
// then filter out the ones that don't have the same digits as i. If
// all entries are still there, we're good.
let checkMultipliedWith (mults:int list) (i:int) = 
    List.map ((*) i) mults |>
    List.filter (haveSameDigits' i) |> 
    List.length = mults.Length

// Given a sequence of possible ints, try and find the first one to 
// pass wonderland conditions.
// We fall back to 0 in case there isn't one. Opts to default to 0 instead
// of returning an Option and needing to modify the code in tests().
let wonderlandNumber () = seq { 100000 .. (int (1000000/6)) } |>  
                          Seq.tryFind (checkMultipliedWith [2 .. 6]) |>
                          Option.defaultWith (fun _ -> 0)

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    let wonderNum = wonderlandNumber()

    test <@ (string wonderNum).Length = 6 @>

    test <@ haveSameDigits (wonderNum, 2 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 3 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 4 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 5 * wonderNum) @>
    test <@ haveSameDigits (wonderNum, 6 * wonderNum) @>

// run the tests
tests ()
