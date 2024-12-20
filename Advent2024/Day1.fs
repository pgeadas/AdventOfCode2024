module Advent2024.Day1

open System
open System.Collections.Generic
open System.IO
open Microsoft.FSharp.Core

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day1.txt"

let readAllPairs filePath =
    File.ReadLines(filePath)
    |> Seq.fold
        (fun (leftAcc, rightAcc) line ->
            if not (String.IsNullOrWhiteSpace(line)) then
                let parts = line.Split("  ", StringSplitOptions.RemoveEmptyEntries)

                match parts with
                | [| value1; value2 |] -> (int value1 :: leftAcc, int value2 :: rightAcc)
                | _ -> (leftAcc, rightAcc)
            else
                (leftAcc, rightAcc))
        ([], [])

let difference leftVal rightVal = abs (leftVal - rightVal)

let countAppearances num list =
    list |> List.filter ((=) num) |> List.length

let getOrMultiply key (map: Dictionary<int, int>) list =
    if map.ContainsKey(key) then
        map[key]
    else
        let value = (countAppearances key list) * key
        map.Add(key, value)
        value

let calculate leftValues rightValues =
    let map = Dictionary<int, int>()

    leftValues
    |> List.map (fun value -> getOrMultiply value map rightValues)
    |> List.sum

let part1 () =
    let leftValues, rightValues = readAllPairs filePath

    let sortedLeft = leftValues |> List.sort
    let sortedRight = rightValues |> List.sort

    let sumDifferences =
        List.zip sortedLeft sortedRight
        |> List.map (fun (left, right) -> difference left right)
        |> List.sum

    sumDifferences

let part2 () =
    let leftValues, rightValues = readAllPairs filePath
    calculate leftValues rightValues
