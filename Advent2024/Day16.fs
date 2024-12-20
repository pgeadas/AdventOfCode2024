module Advent2024.Day16

open System
open System.Collections.Generic
open Advent2024.Matrix
open Advent2024.Common

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day16.txt"

type Cost = int
let turnCost: Cost = 1000
let moveCost: Cost = 1

let directions = [| Up; Down; Left; Right |]

let findShortestPathCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let rows, cols = matrixSize matrix

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
    let moveOrTurn (moveOrTurnCost: Cost) (position: Coordinate) (currentCost: Cost) (direction: StandardDirection) =
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

    findShortestPathCost (matrix: char array list) startPos endPos

// This solution also solves part1, but since it is more complex I decided to keep it as a separate function
let findShortestPaths (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let rows, cols = matrixSize matrix

    let cost =
        Array.init rows (fun _ -> Array.init cols (fun _ -> Array.create directions.Length Int32.MaxValue))

    // Track multiple predecessors for each position/direction
    let predecessors =
        Array.init rows (fun _ ->
            Array.init cols (fun _ -> Array.init directions.Length (fun _ -> ResizeArray<Coordinate * StandardDirection>())))

    let queue = PriorityQueue<Coordinate * StandardDirection * Cost, int>()

    // Initialize queue
    for dir in directions do
        cost[startPos.X].[startPos.Y][dir.Index()] <- 0
        let initialCost = if dir = Right then 0 else turnCost
        queue.Enqueue((startPos, dir, initialCost), initialCost)

    let moveOrTurn
        (moveOrTurnCost: Cost)
        (position: Coordinate)
        (currentCost: Cost)
        (direction: StandardDirection)
        (fromPos: Coordinate)
        (fromDir: StandardDirection)
        =
        let totalCost = currentCost + moveOrTurnCost
        let currentBestCost = cost[position.X].[position.Y][direction.Index()]

        if totalCost <= currentBestCost then

            if totalCost < currentBestCost then
                cost[position.X].[position.Y][direction.Index()] <- totalCost
                // If this is a new best cost, clear previous predecessors
                predecessors[position.X].[position.Y].[direction.Index()].Clear()
                queue.Enqueue((position, direction, totalCost), totalCost)

            // Add the new path to predecessors, since it is a new best cost
            predecessors[position.X]
                .[position.Y].[direction.Index()].Add((fromPos, fromDir))

    let move = moveOrTurn moveCost
    let turn = moveOrTurn turnCost

    let rec processQueue () =
        if queue.Count = 0 then
            None
        else
            let currentPos, currentDir, currentCost = queue.Dequeue()

            if currentPos = endPos then
                Some(currentCost, currentPos, currentDir)
            else
                let nextPos =
                    nextPositionStandard (currentPos.X, currentPos.Y) currentDir
                    |> Coordinate.Create

                if
                    isValidPosition rows cols (nextPos.X, nextPos.Y)
                    && matrix[nextPos.X][nextPos.Y] <> '#'
                then
                    move nextPos currentCost currentDir currentPos currentDir

                turn currentPos currentCost (currentDir.TurnRight()) currentPos currentDir
                turn currentPos currentCost (currentDir.TurnLeft()) currentPos currentDir

                processQueue ()

    // Reconstruct all paths from predecessors
    let rec buildPaths pos (dir: StandardDirection) =
        let predList = predecessors[pos.X].[pos.Y].[dir.Index()]

        if predList.Count = 0 then
            // Base case: start position
            [ [ (pos, dir) ] ]
        else
            [ for prevPos, prevDir in predList do
                  for path in buildPaths prevPos prevDir do
                      yield (pos, dir) :: path ]

    match processQueue () with
    | Some(cost, endPos, endDir) ->
        let paths = buildPaths endPos endDir
        Some(cost, paths)
    | None -> None

let part2 () =
    let matrix, positions = readAndFindAllChars filePath [ 'S'; 'E' ]

    let findPosition char =
        positions |> List.find (fun (x, _) -> x = char) |> snd

    let startPos = Coordinate.Create(findPosition 'S')
    let endPos = Coordinate.Create(findPosition 'E')

    match findShortestPaths matrix startPos endPos with
    | Some(cost, paths) ->

        // Get the unique positions of the paths
        let uniquePositions = HashSet<Coordinate>()

        paths
        |> List.collect id
        |> List.map fst
        |> List.iter (fun pos -> uniquePositions.Add(pos) |> ignore)

        // part1 and part2
        cost, uniquePositions.Count
    | None ->
        printfn "No path found"
        -1, -1
