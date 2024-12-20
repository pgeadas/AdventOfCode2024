module Advent2024.Day8

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

let groupSimilarAntenas (matrix: char array list) =
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
    let calculateCoordinate a1 a2 delta =
        if a1 < a2 then
            a1 - delta, a2 + delta
        else
            a1 + delta, a2 - delta

    let deltaX = abs (antena1.X - antena2.X)
    let deltaY = abs (antena1.Y - antena2.Y)

    let antinode1X, antinode2X = calculateCoordinate antena1.X antena2.X deltaX
    let antinode1Y, antinode2Y = calculateCoordinate antena1.Y antena2.Y deltaY

    [ Coordinate.Create(antinode1X, antinode1Y)
      Coordinate.Create(antinode2X, antinode2Y) ]

let part1 () =
    let matrix =
        readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day8.txt"

    let antenasMap = groupSimilarAntenas matrix

    let generatePairings (antenas: Coordinate list) =
        [ for i in 0 .. antenas.Length - 1 do
              for j in i + 1 .. antenas.Length - 1 do
                  yield (antenas[i], antenas[j]) ]

    let rows, cols = matrixSize matrix

    let antenasPositions = antenasMap.Values |> Seq.collect id

    let res =
        [ for antenas in antenasMap.Values do
              yield! generatePairings antenas ]
        |> List.collect (fun (a1, a2) -> antinodes a1 a2)
        |> List.filter (isValidCoordinate cols rows)
        |> List.filter (fun coord -> not (Seq.contains coord antenasPositions))

    res.Length
