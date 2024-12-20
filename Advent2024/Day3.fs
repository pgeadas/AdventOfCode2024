module Advent2024.Day3

open System.IO
open System.Text.RegularExpressions

let pattern = Regex("mul\((\d{1,3}),(\d{1,3})\)")
let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day3.txt"

let readAllLinesFromFile filePath = File.ReadAllText(filePath)

let getAllMatches (regex: Regex) input =
    let matches = regex.Matches(input)

    [ for m in matches ->
          let group1 = m.Groups.[1].Value
          let group2 = m.Groups.[2].Value
          (group1 |> int, group2 |> int) ]

let multAndSum list =
    list |> List.map (fun (a, b) -> a * b) |> List.sum

let executePart1 (input: string) =
    let matches = getAllMatches pattern input
    multAndSum matches

let part1 () =
    let input = readAllLinesFromFile filePath
    executePart1 input

let executePart2 (input: string) =
    let mutable parsingEnabled = true // Initially enabled to capture initial `mul`
    let mutable results = []
    let mutable index = 0
    let inputLength = input.Length

    while index < inputLength do
        if index <= inputLength - 4 && input[index .. index + 3] = "do()" then
            parsingEnabled <- true
            index <- index + 4
        elif index <= inputLength - 7 && input[index .. index + 6] = "don't()" then
            parsingEnabled <- false
            index <- index + 7
        elif parsingEnabled then
            let matchFound = pattern.Match(input, index)

            if matchFound.Success && matchFound.Index = index then
                let a = matchFound.Groups.[1].Value |> int
                let b = matchFound.Groups.[2].Value |> int
                results <- (a, b) :: results
                index <- index + matchFound.Length // Move past the matched part completely
            else
                index <- index + 1 // Move to the next character if no match at current index
        else
            index <- index + 1

    results |> multAndSum

let part2 () =
    let input = readAllLinesFromFile filePath
    executePart2 input
