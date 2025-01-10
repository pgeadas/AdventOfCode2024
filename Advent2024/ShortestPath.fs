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

let findShortestPaths (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) (moveCost: Cost) =
    let rows, cols = matrixSize matrix

    let cost = Array.init rows (fun _ -> Array.init cols (fun _ -> Int32.MaxValue))

    // Track multiple predecessors for each position
    let predecessors =
        Array.init rows (fun _ -> Array.init cols (fun _ -> ResizeArray<Coordinate>()))

    let queue = PriorityQueue<Coordinate * Cost, int>()

    cost[startPos.X].[startPos.Y] <- 0
    queue.Enqueue((startPos, 0), 0)

    let move (nextPos: Coordinate) (currentCost: Cost) (fromPos: Coordinate) =
        let totalCost = currentCost + moveCost
        let currentBestCost = cost[nextPos.X].[nextPos.Y]

        if totalCost <= currentBestCost then

            if totalCost < currentBestCost then
                cost[nextPos.X].[nextPos.Y] <- totalCost
                // If this is a new best cost, clear previous predecessors
                predecessors[nextPos.X].[nextPos.Y].Clear()
                queue.Enqueue((nextPos, totalCost), totalCost)

            // Add the new path to predecessors, since it is a new best cost
            predecessors[nextPos.X].[nextPos.Y].Add(fromPos)

    let rec processQueue () =
        if queue.Count = 0 then
            None
        else
            let currentPos, currentCost = queue.Dequeue()

            if currentPos = endPos then
                Some(currentCost, predecessors)
            else
                for dir in StandardDirection.Values do
                    let nextPos =
                        nextPositionStandard (currentPos.X, currentPos.Y) dir |> Coordinate.Create

                    if isValidCoordinate rows cols nextPos && matrix[nextPos.X][nextPos.Y] <> '#' then
                        move nextPos currentCost currentPos

                processQueue ()

    processQueue ()

// For each coordinate on the path, also keeps track of the direction that lead to it
let findShortestPathsWithDirection
    (matrix: char array list)
    (startPos: Coordinate)
    (endPos: Coordinate)
    (moveCost: Cost)
    =
    let rec buildPaths (pos, prevDir) (predecessors: ResizeArray<Coordinate> array array) =
        let predList = predecessors[pos.X].[pos.Y]

        if predList.Count = 0 then
            [ [ pos, prevDir ] ]
        else
            [ for prevPos in predList do
                  for path in buildPaths (prevPos, prevDir) predecessors do
                      let previousDir = inferDirection (pos.X, pos.Y) (prevPos.X, prevPos.Y)
                      yield (pos, previousDir.ToChar()) :: path ]

    match findShortestPaths matrix startPos endPos moveCost with
    | Some(_, predecessors) ->
        let paths = buildPaths (endPos, ' ') predecessors
        Some(paths)
    | None -> None

// For each coordinate on the path, also keeps track of the remaining cost to reach the end
let findShortestPathsWithRemainingCost
    (matrix: char array list)
    (startPos: Coordinate)
    (endPos: Coordinate)
    (moveCost: Cost)
    =

    let rec buildPaths (pos, prevCost) (predecessors: ResizeArray<Coordinate> array array) =
        let predList = predecessors[pos.X].[pos.Y]

        if predList.Count = 0 then
            [ [ pos, prevCost ] ]
        else
            [ for prevPos in predList do
                  for path in buildPaths (prevPos, prevCost + 1) predecessors do
                      yield (pos, prevCost) :: path ]

    match findShortestPaths matrix startPos endPos moveCost with
    | Some(_, predecessors) ->
        let paths = buildPaths (endPos, 0) predecessors
        Some(paths)
    | None -> None
