module Advent2024.Day11

open System.Collections.Generic
open System.IO

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day11.txt"

let readInput filePath =
    let line = File.ReadAllLines(filePath)[0]

    line.Split(' ') |> Array.map uint64 |> Array.toList

let numberOfDigits n =
    let rec countDigits n count =
        if n = 0UL then
            count
        else
            countDigits (n / 10UL) (count + 1)

    countDigits n 0

let splitNumber (number: uint64) =
    let digits = numberOfDigits number
    let halfLength = digits / 2

    let divisor = pown 10UL halfLength
    let firstHalf = number / divisor
    let secondHalf = number % divisor

    [ firstHalf; secondHalf ]

let transformStone (cache: Dictionary<uint64, uint64 list>) (stone: uint64) =
    match cache.TryGetValue stone with
    | true, stones -> stones
    | false, _ ->
        let res =
            match stone with
            | 0UL -> [ 1UL ]
            | _ when (numberOfDigits stone) % 2 = 0 -> splitNumber stone
            | _ -> [ stone * 2024UL ]

        cache.Add(stone, res)
        res

let calculateStoneCounts blinks =
    let stones = readInput filePath
    let cache = Dictionary<uint64, uint64 list>()

    let stoneCounts = Dictionary<uint64, uint64>()

    for stone in stones do
        stoneCounts[stone] <- 1UL

    let transformStoneCounts (currentCounts: Dictionary<uint64, uint64>) =
        let newCounts = Dictionary<uint64, uint64>()

        for KeyValue(stone, count) in currentCounts do
            for newStone in transformStone cache stone do
                match newCounts.TryGetValue newStone with
                | true, existingCount -> newCounts[newStone] <- existingCount + count
                | false, _ -> newCounts[newStone] <- count

        newCounts

    let rec blink n (counts: Dictionary<uint64, uint64>) =
        if n = 0 then
            counts
        else
            let newCounts = transformStoneCounts counts
            blink (n - 1) newCounts

    let finalCounts = blink blinks stoneCounts

    finalCounts.Values |> Seq.sum


let part1 () = calculateStoneCounts 25

let part2 () = calculateStoneCounts 75
