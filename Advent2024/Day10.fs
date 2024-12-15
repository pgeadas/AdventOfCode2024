module Advent2024.Day10

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day10.txt"

let directions = [| Right; Left; Up; Down |]

let findAllPaths (matrix: int array list) (startPositions: (int * int) list) =
    let rows, cols = matrixSize matrix
    let allPaths = Dictionary<int * int, (int * int) list list>()
    let visited = Array2D.create rows cols false

    let rec searchPath currentPos currentValue startPos currentPath =
        if currentValue = 9 then

            if allPaths.ContainsKey(startPos) then
                allPaths[startPos] <- currentPath :: allPaths[startPos]
            else
                allPaths[startPos] <- [ currentPath ]

            true
        else
            let x, y = currentPos
            visited[x, y] <- true

            let mutable keepSearching = false

            for direction in directions do
                let nextPos = nextPositionStandard currentPos direction
                let nextX, nextY = nextPos

                if isValidPosition rows cols nextPos && not visited[nextX, nextY] then
                    let nextValue = matrix[nextX][nextY]

                    if nextValue = currentValue + 1 then
                        if searchPath nextPos nextValue startPos (nextPos :: currentPath) then
                            keepSearching <- true

            visited[x, y] <- false // backtrack the previous position so we can try to find another path
            keepSearching

    // Try paths from each starting position
    for startPos in startPositions do
        let x, y = startPos

        if matrix[x][y] = 0 then
            searchPath startPos 0 startPos [ startPos ] |> ignore

    allPaths

let part1 () =
    let matrix, startPositions = readAndFindAllInt filePath '0'

    let paths = findAllPaths (List.ofSeq matrix) startPositions
    // Count paths with same start-end positions as only one
    let uniquePathCount =
        paths
        |> Seq.collect _.Value
        |> Seq.map (fun path -> (List.last path, List.head path)) // Get (start, end) pairs
        |> Set.ofSeq // Convert to set to count unique pairs
        |> Set.count // Count unique start-end combinations

    uniquePathCount

let part2 () =
    let matrix, startPositions = readAndFindAllInt filePath '0'

    let paths = findAllPaths (List.ofSeq matrix) startPositions
    // Count how many paths start at the same position
    let ratings = paths |> Seq.map _.Value |> Seq.map Seq.length |> Seq.sum

    ratings
