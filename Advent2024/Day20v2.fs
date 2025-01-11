module Advent2024.Day20v2

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix
open Advent2024.ShortestPath

// Day20 but solved using a better approach. Actually, ended up using two different approaches here, since we didn't
// really need to know how many of each cheat was saved, just the total saved cost.
//
// Since we know the track is always the same, we don't actually need to recalculate the shortest path
// every time. We only need to calculate the cost up to the nearest track point (relative to the start)
// and subtract the cost of the closest track point (relative to the end) from the first, where both points
// are adjacent to the hackable wall.
//
// For part2, we can use the same approach, but we need to do it for all points inside the max cheat length range,
// which we can calculate using the manhattan distance between the origin and the points.

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day20.txt"

let minPicoseconds = 100

let findShortestPathWithRemainingCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let moveCost: Cost = 1

    match findShortestPathsWithRemainingCost matrix startPos endPos moveCost with
    | Some [ head ] -> head
    | Some _ -> failwith "Unexpected multiple paths found on racetrack"
    | None -> failwith "No path found"

let findCheatsSavedCostWithSequence (matrix: char array list) (path: IDictionary<Coordinate, int>) =
    let rows, cols = matrixSize matrix

    let calculateSavedCost (x: int) (y: int) (deltaX: int) (deltaY: int) (path: IDictionary<Coordinate, int>) =
        let pointA = path.TryGetValue(Coordinate.Create(x + deltaX, y + deltaY))
        let pointB = path.TryGetValue(Coordinate.Create(x - deltaX, y - deltaY))

        let closestToStart, closestToEnd =
            match pointA, pointB with
            | (true, a), (true, b) -> max a b, min a b
            | _ -> failwith "No cost found"

        closestToStart - closestToEnd - 2 // don't count the start and 1 less saved for the wall hacked

    seq {
        for x in 1 .. rows - 2 do
            for y in 1 .. cols - 2 do
                if matrix[x][y] = '#' then
                    if matrix[x + 1][y] = '.' && matrix[x - 1][y] = '.' then
                        yield calculateSavedCost x y 1 0 path
                    elif matrix[x][y + 1] = '.' && matrix[x][y - 1] = '.' then
                        yield calculateSavedCost x y 0 1 path
    }

let calculateTotalSavedCost savedCosts =
    savedCosts
    |> Seq.filter (fun saved -> saved >= minPicoseconds)
    |> Seq.groupBy id
    |> Seq.map (fun (cost, costs) -> cost, Seq.length costs)
    |> Seq.sortBy fst
    |> Seq.sumBy snd

// Part 1 with Sequence (slower), so we can debug and see if we are capturing the correct cheats
let part1WithSequence () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    matrix[startPos.X][startPos.Y] <- '.'
    matrix[endPos.X][endPos.Y] <- '.'

    let path = findShortestPathWithRemainingCost matrix startPos endPos |> dict
    let savedCosts = findCheatsSavedCostWithSequence matrix path
    calculateTotalSavedCost savedCosts

let findCheatsSavedCost (matrix: char array list) (path: IDictionary<Coordinate, int>) =
    let rows, cols = matrixSize matrix

    let calculateSavedCost (x: int) (y: int) (deltaX: int) (deltaY: int) (path: IDictionary<Coordinate, int>) =
        let pointA = path.TryGetValue(Coordinate.Create(x + deltaX, y + deltaY))
        let pointB = path.TryGetValue(Coordinate.Create(x - deltaX, y - deltaY))

        let closestToStart, closestToEnd =
            match pointA, pointB with
            | (true, a), (true, b) -> max a b, min a b
            | _ -> failwith "No cost found"

        closestToStart - closestToEnd - 2 // don't count the start and 1 less saved for the wall hacked

    let mutable savedCost = 0

    for x in 1 .. rows - 2 do
        for y in 1 .. cols - 2 do
            if matrix[x][y] = '#' then
                if matrix[x + 1][y] = '.' && matrix[x - 1][y] = '.' then
                    if calculateSavedCost x y 1 0 path >= minPicoseconds then
                        savedCost <- savedCost + 1
                elif matrix[x][y + 1] = '.' && matrix[x][y - 1] = '.' then
                    if calculateSavedCost x y 0 1 path >= minPicoseconds then
                        savedCost <- savedCost + 1

    savedCost

let part1 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    matrix[startPos.X][startPos.Y] <- '.'
    matrix[endPos.X][endPos.Y] <- '.'

    let path = findShortestPathWithRemainingCost matrix startPos endPos |> dict
    findCheatsSavedCost matrix path

let findCheatsSavedCostPart2 (path: IDictionary<Coordinate, int>) maxCheatLength =
    let getCostFromPoint (x: int) (y: int) (path: IDictionary<Coordinate, int>) =
        match path.TryGetValue(Coordinate.Create(x, y)) with
        | true, a -> a
        | _ -> failwith "No cost found"

    let pathList = path.Keys |> Seq.toList |> List.rev

    // Group points by X coordinate for faster lookup
    let pointsByX = pathList |> List.groupBy _.X |> dict

    let mutable totalSavedCost = 0

    for i in 1 .. pathList.Length do
        let origin = pathList[i - 1]

        // Only look at X coordinates within possible manhattan distance
        let minX = max 0 (origin.X - maxCheatLength)
        let maxX = origin.X + maxCheatLength

        for j in minX..maxX do
            match pointsByX.TryGetValue(j) with
            | true, points ->
                // For points at this X, we can calculate remaining allowed Y distance
                let remainingDist = maxCheatLength - abs (origin.X - j)
                let minY = origin.Y - remainingDist
                let maxY = origin.Y + remainingDist

                for dest in points do
                    if dest.Y >= minY && dest.Y <= maxY then
                        let manhattanDistance = abs (origin.X - dest.X) + abs (origin.Y - dest.Y)
                        let hackedCost = manhattanDistance + getCostFromPoint dest.X dest.Y path
                        let costFromOrigin = getCostFromPoint origin.X origin.Y path
                        let savedCost = costFromOrigin - hackedCost

                        if hackedCost < costFromOrigin && savedCost >= minPicoseconds then
                            totalSavedCost <- totalSavedCost + 1
            | false, _ -> ()

    totalSavedCost

// We could use part2 method to solve part1 by using a cheat length of 2
let part2 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    let path = findShortestPathWithRemainingCost matrix startPos endPos |> dict
    findCheatsSavedCostPart2 path 20
