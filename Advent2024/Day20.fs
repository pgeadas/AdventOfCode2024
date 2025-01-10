module Advent2024.Day20

open System.Collections.Generic
open Advent2024.Common
open Advent2024.ShortestPath
open Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day20.txt"

let findShortestPathCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let moveCost: Cost = 1
    findShortestPathCost matrix startPos endPos moveCost

let findCheatCosts (matrix: char array list) startPos endPos =
    let rows, cols = matrixSize matrix

    matrix[startPos.X][startPos.Y] <- '.'
    matrix[endPos.X][endPos.Y] <- '.'

    seq {
        // ignore borders of the matrix
        for x in 1 .. rows - 2 do
            for y in 1 .. cols - 2 do
                if matrix[x][y] = '#' then
                    if // check for potential hackable walls
                        matrix[x + 1][y] = '.' && matrix[x - 1][y] = '.'
                        || matrix[x][y + 1] = '.' && matrix[x][y - 1] = '.'
                    then
                        matrix[x][y] <- '.'
                        let res = findShortestPathCost matrix startPos endPos
                        matrix[x][y] <- '#'
                        yield res
    }

let calculateCheatSavedCosts normalCost minSavedCost costs =
    costs
    |> Seq.filter (fun cost -> cost < normalCost) // only consider costs that are lower than the normal cost
    |> Seq.map (fun cost -> normalCost - cost) // get the saved picoseconds
    |> Seq.filter (fun cost -> cost >= minSavedCost) // at least 100 picoseconds saved
    |> Seq.groupBy id
    |> Seq.map (fun (cost, costs) -> cost, Seq.length costs)
    |> Seq.sumBy snd

let countBestCheats (matrix: char array list) startPos endPos =
    let normalCost = findShortestPathCost matrix startPos endPos
    findCheatCosts matrix startPos endPos |> calculateCheatSavedCosts normalCost 100

let part1 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    countBestCheats matrix startPos endPos

type Length = int

// This gives way too many cheats. Since we use backtrack, we pass near the End position if there is a wall and continue,
// only stopping when finding a dot. So it is giving us too many wrong cheats.
let findHackableWalls (matrix: char array list) startPos =
    let rows, cols = matrixSize matrix

    let hackableWalls = Dictionary<(int * int) * (int * int), ResizeArray<int * int>>()

    let isValidPosition rows cols (x, y) = x >= 1 && x < rows - 2 && y >= 1 && y < cols - 2

    let rec checkForHackablePath currentPos (currentPath: ResizeArray<int * int>) =
        let x, y = currentPos
        let current = matrix[x][y]

        match current, Seq.length currentPath with
        // Return early if path is too long or current cell is not a path option
        | _, length when length > 20 -> ()
        | '.', length when length > 0 ->
            let key = (startPos, currentPath[currentPath.Count - 1])

            match hackableWalls.TryGetValue(key) with
            // if the cheat starts and ends at the same position, keep the shortest path only
            | true, path when path.Count > currentPath.Count -> hackableWalls[key] <- ResizeArray(currentPath)
            | false, _ -> hackableWalls.Add(key, ResizeArray(currentPath))
            | _ -> ()

        | _ ->
            currentPath.Add(currentPos) // Add current position to path

            for direction in StandardDirection.Values do
                let nextPosX, nextPosY = nextPositionStandard currentPos direction

                if isValidPosition rows cols (nextPosX, nextPosY) && not (currentPath.Contains(nextPosX, nextPosY)) then
                    checkForHackablePath (nextPosX, nextPosY) currentPath

            currentPath.Remove(currentPos) |> ignore // Backtrack

    // Starting the recursive search, ignoring the borders of the matrix
    for x in 1 .. rows - 2 do
        for y in 1 .. cols - 2 do
            if matrix[x].[y] = '#' then
                let startingPos = (x, y)
                checkForHackablePath startingPos (ResizeArray())

    hackableWalls

// For part1 using Dijkstra was already taking 40 seconds, so I was pretty confident that this would not work, due to the
// massive amount of possible cheats.
let countBestCheats2 (matrix: char array list) startPos endPos =
    let normalCost = findShortestPathCost matrix startPos endPos

    let hackableWalls = findHackableWalls matrix (startPos.X, startPos.Y)

    hackableWalls
    |> Seq.map (fun kvp ->
        let path = kvp.Value
        path |> Seq.iter (fun (x, y) -> matrix[x][y] <- '.')
        let res = findShortestPathCost matrix startPos endPos
        path |> Seq.iter (fun (x, y) -> matrix[x][y] <- '#') // backtrack
        res)
    |> calculateCheatSavedCosts normalCost 100

let part2() =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    matrix[startPos.X][startPos.Y] <- '.'
    matrix[endPos.X][endPos.Y] <- '.'

    countBestCheats2 matrix startPos endPos
