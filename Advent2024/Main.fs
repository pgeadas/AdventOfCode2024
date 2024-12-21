// ReSharper disable all

module Advent2024.Main

let runDay1 () =
    printfn "Day 1:"
    printfn "Part 1: \n%A" (Day1.part1 ())
    printfn "Part 2: \n%A" (Day1.part2 ())

let runDay2 () =
    printfn "Day 2:"
    printfn "Part 1: \n%A" (Day2.part1 ())
    printfn "Part 2: \n%A" (Day2.part2 ())

let runDay3 () =
    printfn "Day 3:"
    printfn "Part 1: \n%A" (Day3.part1 ())
    printfn "Part 2: \n%A" (Day3.part2 ())

let runDay4 () =
    printfn "Day 4:"
    printfn "Part 1: \n%A" (Day4.part1 ())
    printfn "Part 2: \n%A" (Day4.part2 ())

// Day5 was not a good day. I took too long trying solving it using graphs.
// For part2, tried using topological sort when it was not needed, and it didn't work.
// Ended up solving it in Kotlin instead using simple sorting.
let runDay5 () =
    printfn "Day 5:"
    printfn "Part 1: \n%A" (Day5.part1 ())

let runDay6 () =
    printfn "Day 6:"
    printfn "Part 1: \n%A" (Day6.part1 ())

let runDay7 () =
    printfn "Day 7:"
    printfn "Part 1: \n%A" (Day7.part1 ())
    printfn "Part 2: \n%A" (Day7.part2 ())

let runDay8 () =
    printfn "Day 8:"
    printfn "Part 1: \n%A" (Day8.part1 ())
    printfn "Part 2: \n%A" (Day8.part2 ())

let runDay9 () =
    printfn "Day 9:"
    printfn "Part 1: \n%A" (Day9.part1 ())
    printfn "Part 2: \n%A" (Day9.part2 ())

let runDay10 () =
    printfn "Day 10:"
    printfn "Part 1: \n%A" (Day10.part1 ())
    printfn "Part 2: \n%A" (Day10.part2 ())

let runDay13 () =
    printfn "Day 13:"
    printfn "Part 1: \n%A" (Day13.part1 ())

let runDay14 () =
    printfn "Day 14:"
    printfn "Part 1: \n%A" (Day14.part1 ())
    printfn "Part 2: \n%A" (Day14.part2 ())

let runDay15 () =
    printfn "Day 15:"
    printfn "Part 1: \n%A" (Day15.part1 ())
//printfn "Part 2: \n%A" (Day15.part2 ())

let runDay16 () =
    printfn "Day 16:"
    printfn "Part 1: \n%A" (Day16.part1 ())
    printfn "Part 2: \n%A" (Day16.part2 ())

let runDay19 () =
    printfn "Day 19:"
    printfn "Part 1: \n%A" (Day19.part1 ())
    printfn "Part 2: \n%A" (Day19.part2 ())


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
        | 8 -> runDay8 ()
        | 9 -> runDay9 ()
        | 10 -> runDay10 ()
        | 13 -> runDay13 ()
        | 14 -> runDay14 ()
        | 15 -> runDay15 ()
        | 16 -> runDay16 ()
        | 19 -> runDay19 ()
        | n -> printfn "Day %d not implemented" n
    | _ -> printfn "Usage: program.exe [day]"

    0
