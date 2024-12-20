module Advent2024.Day8

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

// WIP
let group (matrix: char array list) =
    let dict = Dictionary<char, Coordinate list>()

    matrix
    |> List.iteri (fun rowIndex row ->
        row
        |> Array.iteri (fun colIndex char ->
            match char with
            | '.' -> ()
            | _ ->
                match dict.TryGetValue(char) with
                | true, list -> dict[char] <- (Coordinate.Create(colIndex, rowIndex) :: list)
                | false, _ -> dict[char] <- [ Coordinate.Create(colIndex, rowIndex) ]))

    dict

let antinodes (antena1: Coordinate) (antena2: Coordinate) =
    let deltaX = abs (antena1.X - antena2.X)
    let deltaY = abs (antena1.Y - antena2.Y)

    let antinode1 = antena1 - Coordinate.Create(deltaX, deltaY)
    let antinode2 = antena2 + Coordinate.Create(deltaX, deltaY)
    [ antinode1; antinode2 ]

let part1 () =
    let matrix =
        readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day8.txt"

    let antenas = group matrix

    let res =
        [ for antenas in antenas.Values do
              for i in 0 .. antenas.Length - 2 do
                  yield antinodes antenas.[i] antenas.[i + 1] ]

    let res2 = res |> List.collect id

    res2
