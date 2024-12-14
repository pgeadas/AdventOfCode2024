module Advent2024.Day10

open System.IO
open System.Collections.Generic
open Advent2024.Common

let readFromFile filePath =
    let matrix, positions =
        File.ReadLines(filePath)
        |> Seq.mapi (fun rowIndex line ->
            let elements = Array.ofSeq line |> Array.map (string >> int)

            let linePositions =
                elements
                |> Array.indexed
                |> Array.fold (fun acc (columnIndex, value) -> if value = 0 then (rowIndex, columnIndex) :: acc else acc) []

            elements, linePositions)
        |> Seq.fold (fun (matrix, allPositions) (elements, positions) -> (elements :: matrix, positions @ allPositions)) ([], [])

    List.rev matrix, positions

let directions = [| Right; Left; Up; Down |]

let getNextPosition (x, y) direction =
    match direction with
    | Right -> (x, y + 1)
    | Left -> (x, y - 1)
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)

let isValidPosition rows cols (x, y) =
    x >= 0 && x < rows && y >= 0 && y < cols

let matrixSize (matrix: int array list) =
    let rows = List.length matrix
    let cols = List.head matrix |> Array.length
    rows, cols

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
                let nextPos = getNextPosition currentPos direction
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

let part1 (matrix: int array list) (startPositions: (int * int) list) =
    let paths = findAllPaths matrix startPositions
    // Count paths with same start-end positions as one
    let uniquePathCount =
        paths
        |> Seq.collect _.Value
        |> Seq.map (fun path -> (List.last path, List.head path)) // Get (start, end) pairs
        |> Set.ofSeq // Convert to set to count unique pairs
        |> Set.count // Count unique start-end combinations

    uniquePathCount

let part2 (matrix: int array list) (startPositions: (int * int) list) =
    let paths = findAllPaths matrix startPositions
    // Count how many paths start at the same position
    let ratings = paths |> Seq.map _.Value |> Seq.map Seq.length |> Seq.sum

    ratings

// [<EntryPoint>]
// let main argv =
//     let input, indexes =
//         readFromFile "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/Day10.txt"
//
//     // printfn "%d" (part1 (List.ofSeq input) indexes)
//     printfn "%d" (part2 (List.ofSeq input) indexes)
//
//     0
