module Advent2024.Day16

open System
open System.Collections.Generic
open Advent2024.Matrix
open Advent2024.Common

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day16.txt"

type Cost = int

let findShortestPath (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let turnCost = 1000
    let moveCost = 1
    let rows, cols = matrixSize matrix

    let directions = [| Up; Down; Left; Right |]

    let cost =
        Array.init rows (fun _ -> Array.init cols (fun _ -> Array.create directions.Length Int32.MaxValue))

    let queue = PriorityQueue<Coordinate * StandardDirection * Cost, int>()

    // Enqueue initial positions for all directions
    for dir in directions do
        // initial cost for reaching the start position from each direction
        cost[startPos.X].[startPos.Y][dir.Index()] <- 0
        // the starting direction is Right, others imply a rotation
        let initialCost = if dir = Right then 0 else turnCost
        queue.Enqueue((startPos, dir, initialCost), initialCost)

    // Update the distances when moving or turning
    let moveOrTurn (moveOrTurnCost: int) (position: Coordinate) (currentCost: int) (direction: StandardDirection) =
        let totalCost = currentCost + moveOrTurnCost

        if totalCost < cost[position.X].[position.Y][direction.Index()] then
            cost[position.X].[position.Y][direction.Index()] <- totalCost
            queue.Enqueue((position, direction, totalCost), totalCost)

    let move = moveOrTurn moveCost
    let turn = moveOrTurn turnCost

    let rec processQueue () =
        if queue.Count = 0 then
            -1 // No path found
        else
            let currentPos, currentDir, currentCost = queue.Dequeue()

            if currentPos = endPos then
                currentCost
            else

                let nextPos =
                    nextPositionStandard (currentPos.X, currentPos.Y) currentDir
                    |> Coordinate.Create

                if
                    isValidPosition rows cols (nextPos.X, nextPos.Y)
                    && matrix[nextPos.X][nextPos.Y] <> '#'
                then
                    // move in the current direction
                    move nextPos currentCost currentDir

                // check if turning left or right gives us a shorter path
                turn currentPos currentCost (currentDir.TurnRight())
                turn currentPos currentCost (currentDir.TurnLeft())

                processQueue ()

    processQueue ()

let part1 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char =
        positions |> List.find (fun (x, _) -> x = char) |> snd

    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    findShortestPath (matrix: char array list) startPos endPos
