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

let calculateCoordinate a1 a2 delta =
    if a1 < a2 then
        a1 - delta, a2 + delta
    else
        a1 + delta, a2 - delta

let antinodePair position1 position2 deltaX deltaY =
    let antinode1X, antinode2X = calculateCoordinate position1.X position2.X deltaX
    let antinode1Y, antinode2Y = calculateCoordinate position1.Y position2.Y deltaY

    [ Coordinate.Create(antinode1X, antinode1Y)
      Coordinate.Create(antinode2X, antinode2Y) ]

// generates only the first pair of antinodes
let antinodesPart1 (antena1: Coordinate) (antena2: Coordinate) rows cols =
    let deltaX = abs (antena1.X - antena2.X)
    let deltaY = abs (antena1.Y - antena2.Y)

    antinodePair antena1 antena2 deltaX deltaY
    |> List.filter (isValidCoordinate cols rows)

let generatePairings (antenas: Coordinate list) =
    [ for i in 0 .. antenas.Length - 1 do
          for j in i + 1 .. antenas.Length - 1 do
              yield (antenas[i], antenas[j]) ]

let antinodesCoordinates (antenasMap: Dictionary<char, Coordinate list>) matrix =
    let antenasPositions = antenasMap.Values |> Seq.collect id
    let rows, cols = matrixSize matrix

    [ for antenas in antenasMap.Values do
          yield! generatePairings antenas ]
    |> List.collect (fun (a1, a2) -> antinodesPart1 a1 a2 cols rows)
    |> List.filter (fun coord -> not (Seq.contains coord antenasPositions))

let part1 () =
    let matrix =
        readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day8.txt"

    let antenasMap = groupSimilarAntenas matrix

    antinodesCoordinates antenasMap matrix |> List.length

// generates antinodes until both are invalid
let rec generateAllAntinodes (antinodes: Coordinate list) rows cols deltaX deltaY =
    match antinodes with
    | [ first; second ] when isValidCoordinate cols rows first || isValidCoordinate cols rows second ->

        let newAntinodes = antinodePair first second deltaX deltaY

        generateAllAntinodes newAntinodes rows cols deltaX deltaY @ [ first; second ]
    | _ -> antinodes

let allAntinodes (antena1: Coordinate) (antena2: Coordinate) rows cols =
    let deltaX = abs (antena1.X - antena2.X)
    let deltaY = abs (antena1.Y - antena2.Y)

    let initialAntinodes = antinodePair antena1 antena2 deltaX deltaY

    (generateAllAntinodes initialAntinodes rows cols deltaX deltaY
     @ [ antena1; antena2 ]) // these are going to be antinodes as well now
    |> List.filter (isValidCoordinate cols rows)

let antinodesCoordinates2 (antenasMap: Dictionary<char, Coordinate list>) matrix =
    let rows, cols = matrixSize matrix

    [ for antenas in antenasMap.Values do
          yield! generatePairings antenas ]
    |> List.collect (fun (a1, a2) -> allAntinodes a1 a2 rows cols)
    |> Set.ofList // remove duplicates (when we add the original antena coordinates, some are duplicated)

let part2 () =
    let matrix =
        readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day8.txt"

    let antenasMap = groupSimilarAntenas matrix

    antinodesCoordinates2 antenasMap matrix |> Set.count
