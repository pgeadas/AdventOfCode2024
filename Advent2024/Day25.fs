module Advent2024.Day25

open System
open System.IO

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day25.txt"

let readAndTransform filePath =
    let rec readUntilEmptyLine remaining (acc: string list) =
        match remaining with
        | [] -> (List.rev (List.tail acc), [])
        | line :: rest ->
            if String.IsNullOrWhiteSpace(line) then
                //drop first line (before reversing, which means last line) since we don't need it
                (List.rev (List.tail acc), rest)
            else
                readUntilEmptyLine rest (line :: acc)

    let rec readNextKeyOrLock remaining (keys: string list list) (locks: string list list) =
        let isLock (firstLine: string) = firstLine.StartsWith('#')

        match remaining with
        | [] -> (keys, locks)
        // drop first line since we don't need it, passing only the rest
        | line :: rest ->
            if isLock line then
                let newLock, rest = readUntilEmptyLine rest []
                readNextKeyOrLock rest keys (newLock :: locks)
            else
                let newKey, rest = readUntilEmptyLine rest []
                readNextKeyOrLock rest (newKey :: keys) locks

    let lines = File.ReadLines filePath |> Seq.toList

    readNextKeyOrLock lines [] []

let sumColumns (matrix: int array list) =
    Array.transpose matrix |> Array.map Array.sum

let createOverlapMatrix (keys: int array list) (locks: int array list) =
    let targetSum = keys[0].Length

    keys
    |> List.map (fun key ->
        locks
        |> List.map (fun lock ->
            // Compare each index and stop as soon as one fails
            let rec checkIndex i =
                if i >= targetSum then
                    1
                else
                    let elemA = Array.item i key
                    let elemB = Array.item i lock
                    if elemA + elemB <= targetSum then checkIndex (i + 1) else 0

            checkIndex 0))

let part1 () =
    let keys, locks = readAndTransform filePath

    let toInt (line: string) =
        line |> Array.ofSeq |> Array.map (fun c -> if c = '#' then 1 else 0)

    let keys = keys |> List.map (List.map toInt) |> List.map sumColumns
    let locks = locks |> List.map (List.map toInt) |> List.map sumColumns

    let overlapMatrix = createOverlapMatrix keys locks

    overlapMatrix |> List.collect id |> List.sum
