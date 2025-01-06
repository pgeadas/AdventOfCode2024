module Advent2024.Day15

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day15.txt"

let groupAndCount (input: char array) =
    input
    |> Array.fold
        (fun acc c ->
            let direction = StandardDirection.FromChar(c)

            match acc with
            | (prevChar, count) :: tail when prevChar = direction -> (prevChar, count + 1) :: tail
            | _ -> (direction, 1) :: acc)
        []
    |> List.rev

let tryFindNextEmptyPosition
    (matrix: ResizeArray<ResizeArray<char>>)
    (direction: StandardDirection)
    (currentPosition: Coordinate)
    =
    let rec searchInDirection pos =
        let nextCoord = nextCoordinate pos direction

        match matrix[nextCoord.X][nextCoord.Y] with
        | '.' -> Some(nextCoord)
        | '#' -> None
        | _ -> searchInDirection nextCoord

    searchInDirection currentPosition

type Steps = int

// remove the head move if no more steps are left
let removeHeadIfZero (moves: ResizeArray<StandardDirection * Steps>) =
    if (snd moves[0] = 0) then
        moves.RemoveAt(0)

let rec move (currentPos: Coordinate) (matrix: ResizeArray<ResizeArray<char>>) (moves: ResizeArray<StandardDirection * Steps>) =
    if (moves.Count = 0) then
        matrix
    else
        let currentDirection, currentSteps = Seq.head moves
        let nextPos = nextCoordinate currentPos currentDirection

        if matrix[nextPos.X][nextPos.Y] = '#' then
            moves.RemoveAt(0)
            move currentPos matrix moves
        else if matrix[nextPos.X][nextPos.Y] = '.' then
            moves[0] <- (currentDirection, currentSteps - 1)
            matrix[nextPos.X][nextPos.Y] <- '@'
            matrix[currentPos.X][currentPos.Y] <- '.'
            removeHeadIfZero moves
            move nextPos matrix moves
        else
            let firstEmptyPos = tryFindNextEmptyPosition matrix currentDirection currentPos

            match firstEmptyPos with
            | Some(emptyPos) ->
                matrix[emptyPos.X][emptyPos.Y] <- 'O'
                matrix[nextPos.X][nextPos.Y] <- '@'
                matrix[currentPos.X][currentPos.Y] <- '.'
                moves[0] <- (currentDirection, currentSteps - 1)
                removeHeadIfZero moves
                move nextPos matrix moves
            | None ->
                moves.RemoveAt(0)
                move currentPos matrix moves

let toResizeArray2d (matrix: char array list) =
    let array2d = ResizeArray<ResizeArray<char>>()

    for i in 0 .. matrix.Length - 1 do
        let row = ResizeArray<char>()

        for j in 0 .. matrix[i].Length - 1 do
            row.Add(matrix[i][j])

        array2d.Add(row)

    array2d

let gpsValue (matrix: ResizeArray<ResizeArray<char>>) char =
    seq {
        for i in 0 .. matrix.Count - 1 do
            for j in 0 .. matrix[i].Count - 1 do
                if matrix[i][j] = char then
                    yield (i, j)
    }
    |> Seq.map (fun (i, j) -> i * 100 + j)
    |> Seq.sum

let part1 () =
    let (matrix, startPosition), moves = readMatrixAndGroups filePath '@'

    // merge moves into a single list and group by character
    let groups = moves |> List.head |> Array.concat |> groupAndCount

    match startPosition with
    | Some startPosition ->
        let startCoord = Coordinate.Create(startPosition)
        let matrix = move startCoord (toResizeArray2d matrix) (ResizeArray(groups))
        gpsValue matrix 'O'
    | None -> failwith "No start position found"

let largeWarehouse (matrix: char array list) =
    let array2d = ResizeArray<ResizeArray<char>>()

    for i in 0 .. matrix.Length - 1 do
        let row = ResizeArray<char>()

        for j in 0 .. matrix[i].Length - 1 do
            if (matrix[i][j] = '#') || matrix[i][j] = '.' then
                row.Add(matrix[i][j])
                row.Add(matrix[i][j])
            elif (matrix[i][j] = '@') then
                row.Add(matrix[i][j])
                row.Add('.')
            else
                row.Add('[')
                row.Add(']')

        array2d.Add(row)

    array2d

type Search = { mutable value: bool }

let tryFindNextEmptyPositionUpOrDown
    (matrix: ResizeArray<ResizeArray<char>>)
    (direction: StandardDirection)
    (currentPosition: Coordinate)
    =

    let rec searchInDirection left right (checkedPositions: HashSet<Option<Coordinate>>) (keepSearching: Search) =
        let nextPosLeft = nextCoordinate left direction
        let nextPosRight = nextCoordinate right direction

        let addToCheckedPositions pos1 pos2 (checkedPositions: HashSet<Option<Coordinate>>) =
            checkedPositions.Add(Some(pos1)) |> ignore
            checkedPositions.Add(Some(pos2)) |> ignore

        match matrix[nextPosLeft.X][nextPosLeft.Y], matrix[nextPosRight.X][nextPosRight.Y] with
        | '#', _
        | _, '#' ->
            keepSearching.value <- false
            checkedPositions.Add(None) |> ignore
            checkedPositions
        | '.', '.' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            checkedPositions
        | '.', '[' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection nextPosRight (nextPosRight.Right()) checkedPositions keepSearching
        | ']', '.' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection (nextPosLeft.Left()) nextPosLeft checkedPositions keepSearching
        | ']', '[' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection (nextPosLeft.Left()) nextPosLeft checkedPositions keepSearching |> ignore
            searchInDirection nextPosRight (nextPosRight.Right()) checkedPositions keepSearching
        | _ ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection nextPosLeft nextPosRight checkedPositions keepSearching

    let findPositions =
        let directionOffset = if direction = StandardDirection.Up then -1 else 1

        let initialCoordinate =
            Coordinate.Create(currentPosition.X + directionOffset, currentPosition.Y)

        let left, right =
            if matrix[initialCoordinate.X][initialCoordinate.Y] = ']' then
                initialCoordinate.Left(), initialCoordinate
            else
                initialCoordinate, initialCoordinate.Right()

        let mutable keepSearching = { value = true }
        let checkedPositions = HashSet<Option<Coordinate>>()
        searchInDirection left right checkedPositions keepSearching

    findPositions |> Seq.toList

let rec moveLarge
    (currentPos: Coordinate)
    (matrix: ResizeArray<ResizeArray<char>>)
    (moves: ResizeArray<StandardDirection * Steps>)
    =

    let processVerticalMovement
        (matrix: ResizeArray<ResizeArray<char>>)
        (currentPos: Coordinate)
        (nextPos: Coordinate)
        (direction: StandardDirection)
        (moves: ResizeArray<StandardDirection * Steps>)
        (sortDirection: Coordinate seq -> Coordinate seq)
        =
        let nextEmptyPositions =
            tryFindNextEmptyPositionUpOrDown matrix direction currentPos

        match nextEmptyPositions |> Seq.tryFind ((=) None) with
        | Some _ ->
            moves.RemoveAt(0)
            moveLarge currentPos matrix moves
        | None ->
            let delta = if direction = StandardDirection.Up then 1 else -1

            nextEmptyPositions
            |> Seq.choose id
            |> sortDirection
            |> Seq.iter (fun pos ->
                let prevX = pos.X + delta
                let prevChar = matrix[prevX][pos.Y]
                matrix[pos.X][pos.Y] <- prevChar
                matrix[prevX][pos.Y] <- '.')

            // Update the remaining moves and positions
            moves[0] <- (direction, snd moves[0] - 1)
            matrix[currentPos.X][currentPos.Y] <- '.'
            matrix[nextPos.X][nextPos.Y] <- '@'
            removeHeadIfZero moves
            moveLarge nextPos matrix moves

    if (moves.Count = 0) then
        matrix
    else
        let currentDirection, currentSteps = Seq.head moves
        let nextPos = nextCoordinate currentPos currentDirection

        if matrix[nextPos.X][nextPos.Y] = '#' then // cant move more in this direction
            moves.RemoveAt(0)
            moveLarge currentPos matrix moves

        else if matrix[nextPos.X][nextPos.Y] = '.' then // can move in this direction
            moves[0] <- (currentDirection, currentSteps - 1)
            matrix[nextPos.X][nextPos.Y] <- '@'
            matrix[currentPos.X][currentPos.Y] <- '.'
            removeHeadIfZero moves
            moveLarge nextPos matrix moves

        else if currentDirection = StandardDirection.Up then
            processVerticalMovement matrix currentPos nextPos currentDirection moves (Seq.sortBy _.X)

        else if currentDirection = StandardDirection.Down then
            processVerticalMovement matrix currentPos nextPos currentDirection moves (Seq.sortByDescending _.X)

        else // move left or right
            let move pos direction =
                let delta = if direction = StandardDirection.Left then 1 else -1

                let rec advanceToEmptyPos finalY =
                    if finalY <> currentPos.Y then
                        matrix[pos.X][finalY] <- matrix[pos.X][finalY + delta]
                        matrix[pos.X][finalY + delta] <- '.'
                        advanceToEmptyPos (finalY + delta)

                advanceToEmptyPos pos.Y
                moves[0] <- (currentDirection, currentSteps - 1)
                removeHeadIfZero moves
                moveLarge nextPos matrix moves

            let nextEmptyPosition = tryFindNextEmptyPosition matrix currentDirection currentPos

            match nextEmptyPosition with
            | Some(emptyPos) -> move emptyPos currentDirection
            | None ->
                moves.RemoveAt(0)
                moveLarge currentPos matrix moves

let part2 () =
    let (matrix, startPosition), moves = readMatrixAndGroups filePath '@'

    let groups = moves |> List.head |> Array.concat |> groupAndCount

    match startPosition with
    | Some startPosition ->
        let startPosition = Coordinate.Create(fst startPosition, snd startPosition * 2)
        let matrix = moveLarge startPosition (largeWarehouse matrix) (ResizeArray(groups))
        gpsValue matrix '['
    | None -> failwith "No start position found"
