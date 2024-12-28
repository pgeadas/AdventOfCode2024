// ReSharper disable all

module Advent2024.Main

open System.Diagnostics

let executeTimed (f: unit -> 'a) =
    let stopwatch = Stopwatch.StartNew()
    let result = f ()
    stopwatch.Stop()
    printfn "%A" result
    printfn "Time elapsed: %A" stopwatch.Elapsed

let runDay1 () =
    printfn "Part 1: "
    executeTimed Day1.part1
    printfn "Part 2: "
    executeTimed Day1.part2

let runDay2 () =
    printfn "Part 1: "
    executeTimed Day2.part1
    printfn "Part 2: "
    executeTimed Day2.part2

let runDay3 () =
    printfn "Part 1:"
    executeTimed Day3.part1
    printfn "Part 2: "
    executeTimed Day3.part2

let runDay4 () =
    printfn "Part 1: "
    executeTimed Day4.part1
    printfn "Part 2: "
    executeTimed Day4.part2

// Day5 was not a good day. I took too long trying solving it using graphs.
// For part2, tried using topological sort when it was not needed, and it didn't work.
// Ended up solving it in Kotlin instead using simple sorting.
let runDay5 () =
    printfn "Part 1: "
    executeTimed Day5.part1

let runDay6 () =
    printfn "Part 1: "
    executeTimed Day6.part1
    printfn "Part 2: "
    executeTimed Day6.part2

let runDay7 () =
    printfn "Part 1: "
    executeTimed Day7.part1
    printfn "Part 2: "
    executeTimed Day7.part2

let runDay8 () =
    printfn "Part 1: "
    executeTimed Day8.part1
    printfn "Part 2: "
    executeTimed Day8.part2

let runDay9 () =
    printfn "Part 1: "
    executeTimed Day9.part1
    printfn "Part 2: "
    executeTimed Day9.part2

let runDay10 () =
    printfn "Part 1: "
    executeTimed Day10.part1
    printfn "Part 2: "
    executeTimed Day10.part2

let runDay11 () =
    printfn "Part 1: "
    executeTimed Day11.part1
    printfn "Part 2: "
    executeTimed Day11.part2

let runDay13 () =
    printfn "Part 1: "
    executeTimed Day13.part1

let runDay14 () =
    printfn "Part 1: "
    executeTimed Day14.part1
    printfn "Part 2: "
    executeTimed Day14.part2

let runDay15 () =
    printfn "Part 1: "
    executeTimed Day15.part1

let runDay16 () =
    printfn "Part 1: "
    executeTimed Day16.part1
    printfn "Part 2: "
    executeTimed Day16.part2

let runDay18 () =
    printfn "Part 1: "
    executeTimed Day18.part1
    printfn "Part 2: "
    executeTimed Day18.part2

let runDay19 () =
    printfn "Part 1: "
    executeTimed Day19.part1
    printfn "Part 2: "
    executeTimed Day19.part2

let runDay21 () =
    printf "Part 1: "
    executeTimed Day21.part1

let runDay23 () =
    printf "Part 1: "
    executeTimed Day23.part1
    printf "Part 2: "
    executeTimed Day23.part2

let runDay25 () =
    printf "Part 1: "
    executeTimed Day25.part1

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> printfn "No day specified. Usage: program.exe [day]"
    | [| day |] ->
        printfn "* Running day %s *" day

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
        | 11 -> runDay11 ()
        | 13 -> runDay13 ()
        | 14 -> runDay14 ()
        | 15 -> runDay15 ()
        | 16 -> runDay16 ()
        | 18 -> runDay18 ()
        | 19 -> runDay19 ()
        | 21 -> runDay21 ()
        | 23 -> runDay23 ()
        | 25 -> runDay25 ()
        | n -> printfn "Day %d not implemented" n
    | _ -> printfn "Usage: program.exe [day]"

    0
