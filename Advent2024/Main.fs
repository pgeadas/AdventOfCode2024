// ReSharper disable all

module Advent2024.Main

let runDay1 () =
    printfn "\nDay 1:"
    printfn "Part 1: %A" (Day1.part1 ())
    printfn "Part 2: %A" (Day1.part2 ())

let runDay2 () =
    printfn "\nDay 2:"
    printfn "Part 1: %A" (Day2.part1 ())
    printfn "Part 2: %A" (Day2.part2 ())

let runDay3 () =
    printfn "\nDay 3:"
    printfn "Part 1: %A" (Day3.part1 ())
    printfn "Part 2: %A" (Day3.part2 ())

let runDay4 () =
    printfn "\nDay 4:"
    printfn "Part 1: %A" (Day4.part1 ())
    printfn "Part 2: %A" (Day4.part2 ())

// Day5 was not a good day. I took too long trying solving it using graphs.
// For part2, tried using topological sort when it was not needed, and it didn't work.
// Ended up solving it in Kotlin instead using simple sorting.
let runDay5 () =
    printfn "\nDay 5:"
    printfn "Part 1: %A" (Day5.part1 ())

let runDay6 () =
    printfn "\nDay 6:"
    printfn "Part 1: %A" (Day6.part1 ())

let runDay7 () =
    printfn "\nDay 7:"
    printfn "Part 1: %A" (Day7.part1 ())
    printfn "Part 2: %A" (Day7.part2 ())

let runDay9 () =
    printfn "\nDay 9:"
    printfn "Part 1: %A" (Day9.part1 ())
    printfn "Part 2: %A" (Day9.part2 ())

let runDay10 () =
    printfn "\nDay 10:"
    printfn "Part 1: %A" (Day10.part1 ())
    printfn "Part 2: %A" (Day10.part2 ())

let runDay13 () =
    printfn "\nDay 13:"
    printfn "Part 1: %A" (Day13.part1 ())

let runDay14 () =
    printfn "\nDay 14:"
    printfn "Part 1: %A" (Day14.part1 ())
    printfn "Part 2: %A" (Day14.part2 ())

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> printfn "No day specified. Usage: program.exe [day]"
    | [| day |] ->
        match int day with
        | 1 -> runDay1 ()
        | 2 -> runDay2 ()
        | 3 -> runDay3 ()
        | 4 -> runDay4 ()
        | 5 -> runDay5 ()
        | 6 -> runDay6 ()
        | 7 -> runDay7 ()
        | 9 -> runDay9 ()
        | 10 -> runDay10 ()
        | 13 -> runDay13 ()
        | 14 -> runDay14 ()
        | n -> printfn "Day %d not implemented" n
    | _ -> printfn "Usage: program.exe [day]"

    0
