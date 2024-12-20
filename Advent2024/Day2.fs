module Advent2024.Day2

open System
open System.IO

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day2.txt"

let parseLineToNumbers (line: string) =
    line.Split(' ') |> Array.map int |> List.ofArray

let readMatrix filePath =

    let processLine matrix line =
        if String.IsNullOrWhiteSpace(line) then
            List.rev matrix
        else
            let values = parseLineToNumbers line
            (values :: matrix)

    File.ReadLines(filePath) |> Seq.fold processLine [] |> List.rev

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
    let matrix = readMatrix filePath
    countSelectedRows matrix

let part2 () =
    let matrix = readMatrix filePath
    matrix |> List.filter canBeMadeSafe |> List.length
