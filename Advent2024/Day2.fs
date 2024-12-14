module Advent2024.Day2

open System

let parseLineToNumbers (line: string) =
    line.Split(' ') |> Array.map int |> List.ofArray

let readAllLinesFromConsole =
    let rec readLines matrix =
        let input = Console.ReadLine()

        if String.IsNullOrWhiteSpace(input) then
            List.rev matrix
        else
            let values = parseLineToNumbers input
            readLines (values :: matrix)

    readLines []

let isStrictlyOrdered comparison lst =
    List.pairwise lst
    |> List.forall (fun (a, b) ->
        let diff = a - b
        comparison diff)

let isAscending = isStrictlyOrdered (fun diff -> diff >= -3 && diff < 0)
let isDescending = isStrictlyOrdered (fun diff -> diff <= 3 && diff > 0)

let countSelectedRows matrix =
    matrix
    |> List.filter (fun row -> isAscending row || isDescending row)
    |> List.length

let removeAt index lst =
    lst |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

let canBeMadeSafe lst =
    let isSafe lst = isAscending lst || isDescending lst

    if isSafe lst then
        true
    else
        List.indexed lst |> List.exists (fun (i, _) -> isSafe (removeAt i lst))

let part1 () =
    let matrix = readAllLinesFromConsole
    let result = countSelectedRows matrix
    printfn $"%d{result}"

let part2 () =
    let matrix = readAllLinesFromConsole
    let result = matrix |> List.filter canBeMadeSafe |> List.length
    printfn $"%d{result}"
