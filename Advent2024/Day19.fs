module Advent2024.Day19

open System
open System.Collections.Generic
open System.IO

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day19.txt"

let readDay19Input (filePath: string) =
    let lines = File.ReadLines(filePath)
    let configLine = Seq.head lines
    let configParts = configLine.Split(',', StringSplitOptions.TrimEntries)

    let sequences =
        Seq.skip 1 lines |> Seq.filter (fun line -> line <> "") |> Seq.toList

    (configParts, sequences)

let part1 () =
    let patterns, designs = readDay19Input filePath

    let rec checkIfPossible currentIndex (patterns: string array) (design: string) =
        if currentIndex >= design.Length then
            true
        else
            patterns
            |> Array.exists (fun pattern ->
                if design.Substring(currentIndex).StartsWith(pattern) then
                    let currentIndex = currentIndex + pattern.Length
                    checkIfPossible currentIndex patterns design
                else
                    false)

    designs
    |> List.map (checkIfPossible 0 patterns)
    |> List.filter (fun b -> b = true)
    |> List.length

// Using lists to track the solutions takes too long, even when using memoization
let rec findAllWays target (patterns: string array) currentSolution solutions =
    if target = "" then
        currentSolution :: solutions
    else
        patterns
        |> Array.fold
            (fun acc pattern ->
                if target.StartsWith(pattern) then
                    findAllWays (target.Substring(pattern.Length)) patterns (pattern :: currentSolution) acc
                else
                    acc)
            solutions

// storing only the count is way faster than storing the solutions
let countAllWays (targets: string list) (patterns: string array) =
    let memo = Dictionary<string, uint64>()

    let rec countWithMemoization target =
        if target = "" then
            // If the target is exhausted, there's exactly one way to achieve it
            1UL
        elif memo.ContainsKey(target) then
            // If the target has been solved before, return the stored count
            memo[target]
        else
            // Calculate the count of ways by trying each pattern
            let count =
                patterns
                |> Array.fold
                    (fun acc pattern ->
                        if target.StartsWith(pattern) then
                            // Recurse for the remaining part of the target
                            acc + countWithMemoization (target.Substring(pattern.Length))
                        else
                            acc)
                    0UL
            // Store the result in memoization dictionary
            memo[target] <- count
            count

    targets |> List.map countWithMemoization

let part2 () =
    let patterns, designs = readDay19Input filePath

    countAllWays designs patterns |> List.sum
