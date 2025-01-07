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

let countBestCheats (matrix: char array list) startPos endPos =
    let normalCost = findShortestPathCost matrix startPos endPos

    findCheatCosts matrix startPos endPos
    |> Seq.filter (fun cost -> cost < normalCost) // only consider costs that are lower than the normal cost
    |> Seq.map (fun cost -> normalCost - cost) // get the saved picoseconds
    |> Seq.filter (fun cost -> cost >= 100) // at least 100 picoseconds saved
    |> Seq.groupBy id
    |> Seq.map (fun (cost, costs) -> cost, Seq.length costs)
    |> Seq.sumBy snd

let part1 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char = positions |> List.find (fun (x, _) -> x = char) |> snd
    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    countBestCheats matrix startPos endPos
