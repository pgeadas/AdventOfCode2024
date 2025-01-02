module Advent2024.Day18

open System
open System.Collections.Generic
open System.IO
open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day18.txt"
let mapSize = 71

let start: Coordinate = { X = 0; Y = 0 }
let finish: Coordinate = { X = mapSize - 1; Y = mapSize - 1 }

let readInput filePath =
    let lines = File.ReadLines(filePath)

    lines
    |> Seq.map _.Split(',')
    |> Seq.map (fun parts -> (int parts[1], int parts[0]) |> Coordinate.Create)
    |> Seq.toList

type Cost = int
let moveCost: Cost = 1

let findShortestPathCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let rows, cols = matrixSize matrix

    let cost = Array.init rows (fun _ -> Array.init cols (fun _ -> Int32.MaxValue))

    let queue = PriorityQueue<Coordinate * Cost, int>()

    cost[startPos.X].[startPos.Y] <- 0
    queue.Enqueue((startPos, 0), 0)

    let move (nextPos: Coordinate) (currentCost: Cost) =
        let totalCost = currentCost + moveCost
        let currentBestCost = cost[nextPos.X].[nextPos.Y]

        if totalCost < currentBestCost then
            cost[nextPos.X].[nextPos.Y] <- totalCost
            queue.Enqueue((nextPos, totalCost), totalCost)

    let rec processQueue () =
        if queue.Count = 0 then
            None
        else
            let currentPos, currentCost = queue.Dequeue()

            if currentPos = endPos then
                Some(currentCost)
            else
                for dir in StandardDirection.Values do
                    let nextPos =
                        nextPositionStandard (currentPos.X, currentPos.Y) dir |> Coordinate.Create

                    if isValidCoordinate rows cols nextPos && matrix[nextPos.X][nextPos.Y] <> '#' then
                        move nextPos currentCost

                processQueue ()

    processQueue ()

let part1 () =
    let obstacles = readInput filePath
    let matrix = List.init mapSize (fun _ -> Array.init mapSize (fun _ -> '.'))

    for i in 0..1023 do
        let obstacle = obstacles[i]
        matrix[obstacle.X][obstacle.Y] <- '#'

    findShortestPathCost matrix finish start |> Option.get

let part2 () =
    let obstacles = readInput filePath
    let matrix = List.init mapSize (fun _ -> Array.init mapSize (fun _ -> '.'))

    for i in 0..1023 do
        let obstacle = obstacles[i]
        matrix[obstacle.X][obstacle.Y] <- '#'

    let mutable res = Some(0)
    let mutable index = 1023

    while (res <> None) do
        index <- index + 1
        let obstacle = obstacles[index]
        matrix[obstacle.X][obstacle.Y] <- '#'
        res <- findShortestPathCost matrix finish start

    $"{obstacles[index].Y},{obstacles[index].X}"
