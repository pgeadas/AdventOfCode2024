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

let runDay13 () =
    printfn "\nDay 13:"
    printfn "Part 1: \n%A" (Day13.part1 ())
    printfn "Part 2: %A" (Day13.part2 ())

let runDay14 () =
    printfn "\nDay 14:"
    printfn "Part 1: \n%A" (Day14.part1 ())
    printfn "Part 2: %A" (Day14.part2 ())

let runDay7 () =
    printfn "\nDay 7:"
    printfn "Part 1: %A" (Day7.part1 ())
    printfn "Part 2: %A" (Day7.part2 ())

[<EntryPoint>]
let main argv =
    match argv with
    | [||] -> printfn "No day specified. Usage: program.exe [day]"
    | [| day |] ->
        // Run specific day
        match int day with
        | 1 -> runDay1 ()
        | 2 -> runDay2 ()
        | 7 -> runDay7 ()
        | 13 -> runDay13 ()
        | 14 -> runDay14 ()
        | n -> printfn "Day %d not implemented" n
    | _ -> printfn "Usage: program.exe [day]"

    0
