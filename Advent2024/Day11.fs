module Advent2024.Day11

open System.Collections.Generic
open System.IO

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day11.txt"

let readInput filePath =
    let line = File.ReadAllLines(filePath)[0]

    line.Split(' ') |> Array.map uint64 |> Array.toList


let transformStone (cache: Dictionary<uint64, uint64 list>) (stone: uint64) =
    match cache.TryGetValue stone with
    | true, stones -> stones
    | false, _ ->
        let res =
            match stone with
            | 0UL -> [ 1UL ]
            | stone when (string stone).Length % 2 = 0 ->
                let stringStone = string stone
                let halfLength = stringStone.Length / 2

                [ stringStone.Substring(0, halfLength) |> uint64
                  stringStone.Substring(halfLength) |> uint64 ]
            | _ -> [ stone * 2024UL ]

        cache.Add(stone, res)
        res

let part1 () =
    let cache = Dictionary<uint64, uint64 list>()
    let stones = readInput filePath

    let transformStones stones =
        stones |> List.collect (transformStone cache)

    let rec blink n currentStones =
        if n = 0 then
            currentStones
        else
            let newStones = transformStones currentStones
            blink (n - 1) newStones

    (blink 25 stones).Length
