module Advent2024.Day1

open System
open System.Collections.Generic
open Microsoft.FSharp.Core

let toInt (s: string) = s.Trim() |> int

let readAllLines () =
    let rec readLines leftAcc rightAcc =
        let input = Console.ReadLine()

        if String.IsNullOrWhiteSpace(input) then
            leftAcc, rightAcc
        else
            match input.Split("  ") with
            | [| value1; value2 |] -> readLines (toInt value1 :: leftAcc) (toInt value2 :: rightAcc)
            | _ -> readLines leftAcc rightAcc

    readLines [] []

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
    let leftValues, rightValues = readAllLines ()

    let sortedLeft = leftValues |> List.sort
    let sortedRight = rightValues |> List.sort

    let sumDifferences =
        List.zip sortedLeft sortedRight
        |> List.map (fun (left, right) -> difference left right)
        |> List.sum

    printfn $"%d{sumDifferences}"

let part2 () =
    let leftValues, rightValues = readAllLines ()
    let result = calculate leftValues rightValues
    printfn $"%d{result}"
