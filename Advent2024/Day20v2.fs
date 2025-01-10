module Advent2024.Day20v2

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix
open Advent2024.ShortestPath

// Day20 but solved using a better approach.
// Since we know the track is always the same, we don't actually need to recalculate the shortest path
// every time. We only need to calculate the cost up to the nearest track point (relative to the start)
// and subtract the cost of the closest track point (relative to the end) from the first, where both points
// are adjacent to the hackable wall.

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day20.txt"

let findShortestPathsWithRemainingCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let moveCost: Cost = 1

    match findShortestPathsWithRemainingCost matrix startPos endPos moveCost with
    | Some [ head ] -> head
    | Some _ -> failwith "Unexpected multiple paths found on racetrack"
    | None -> failwith "No path found"

let findCheatsSavedCost (matrix: char array list) (path: IDictionary<Coordinate, int>) =
    let rows, cols = matrixSize matrix

    let calculateSavedCost (x: int) (y: int) (deltaX: int) (deltaY: int) (path: IDictionary<Coordinate, int>) =
        let pointA = path.TryGetValue(Coordinate.Create(x + deltaX, y + deltaY))
        let pointB = path.TryGetValue(Coordinate.Create(x - deltaX, y - deltaY))

        let closestToStart, closestToEnd =
            match pointA, pointB with
            | (true, a), (true, b) -> max a b, min a b
            | _ -> failwith "No path found"

        closestToStart - closestToEnd

    seq {
        // ignore borders of the matrix
        for x in 1 .. rows - 2 do
            for y in 1 .. cols - 2 do
                if matrix[x][y] = '#' then
                    if matrix[x + 1][y] = '.' && matrix[x - 1][y] = '.' then
                        calculateSavedCost x y 1 0 path
                    elif matrix[x][y + 1] = '.' && matrix[x][y - 1] = '.' then
                        calculateSavedCost x y 0 1 path
    }

let calculateTotalSavedCost (matrix: char array list) (path: IDictionary<Coordinate, int>) =
    findCheatsSavedCost matrix path
    |> Seq.filter (fun saved -> saved > 100)
    |> Seq.groupBy id
    |> Seq.map (fun (cost, costs) -> cost, Seq.length costs)
    |> Seq.sumBy snd

let part1 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    matrix[startPos.X][startPos.Y] <- '.'
    matrix[endPos.X][endPos.Y] <- '.'

    findShortestPathsWithRemainingCost matrix startPos endPos |> dict |> calculateTotalSavedCost matrix
