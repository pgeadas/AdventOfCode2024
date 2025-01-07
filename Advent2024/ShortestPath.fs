module Advent2024.ShortestPath

open System
open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

type Cost = int

let findShortestPathCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) (moveCost: Cost) =
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
            -1
        else
            let currentPos, currentCost = queue.Dequeue()

            if currentPos = endPos then
                currentCost
            else
                for dir in StandardDirection.Values do
                    let nextPos =
                        nextPositionStandard (currentPos.X, currentPos.Y) dir |> Coordinate.Create

                    if isValidCoordinate rows cols nextPos && matrix[nextPos.X][nextPos.Y] <> '#' then
                        move nextPos currentCost

                processQueue ()

    processQueue ()
