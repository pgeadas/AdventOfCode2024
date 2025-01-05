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
    (currentPosition: int * int)
    =
    let rec searchInDirection pos =
        let nextX, nextY = nextPositionStandard pos direction

        match matrix[nextX][nextY] with
        | '.' -> Some(nextX, nextY)
        | '#' -> None
        | _ -> searchInDirection (nextX, nextY)

    searchInDirection currentPosition

let rec move (currentPosition: int * int) (matrix: ResizeArray<ResizeArray<char>>) (moves: ResizeArray<StandardDirection * int>) =
    if (moves.Count = 0) then
        matrix
    else
        //printfn "Current position: %A" (moves.Item(0))
        //printMatrix matrix
        let currentDirection, currentSteps = Seq.head moves
        let x, y = nextPositionStandard currentPosition currentDirection

        if matrix[x][y] = '#' then
            moves.RemoveAt(0)
            move currentPosition matrix moves
        else if matrix[x][y] = '.' then
            moves[0] <- (currentDirection, currentSteps - 1)
            matrix[x][y] <- '@'
            matrix[fst currentPosition][snd currentPosition] <- '.'

            if (snd moves[0] = 0) then
                moves.RemoveAt(0)

            move (x, y) matrix moves
        else
            let nextPosition = tryFindNextEmptyPosition matrix currentDirection currentPosition

            match nextPosition with
            | Some(nextX, nextY) ->
                matrix[nextX][nextY] <- 'O'
                matrix[x][y] <- '@'
                matrix[fst currentPosition][snd currentPosition] <- '.'
                moves[0] <- (currentDirection, currentSteps - 1)

                if (snd moves[0] = 0) then
                    moves.RemoveAt(0)

                move (x, y) matrix moves
            | None ->
                moves.RemoveAt(0)
                move currentPosition matrix moves

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
        let matrix = move startPosition (toResizeArray2d matrix) (ResizeArray(groups))
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
    (currentPosition: int * int)
    =

    let rec searchInDirection left right (checkedPositions: HashSet<Option<int * int>>) (keepSearching: Search) =
        let nextPosLeft = nextPositionStandard left direction
        let nextLeftX, nextLeftY = nextPosLeft
        let nextPosRight = nextPositionStandard right direction
        let nextRightX, nextRightY = nextPosRight

        let addToCheckedPositions pos1 pos2 (checkedPositions: HashSet<Option<int * int>>) =
            checkedPositions.Add(Some(pos1)) |> ignore
            checkedPositions.Add(Some(pos2)) |> ignore

        match matrix[nextLeftX][nextLeftY], matrix[nextRightX][nextRightY] with
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
            searchInDirection nextPosRight (nextRightX, nextRightY + 1) checkedPositions keepSearching
        | ']', '.' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection (nextLeftX, nextLeftY - 1) nextPosLeft checkedPositions keepSearching
        | ']', '[' ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection (nextLeftX, nextLeftY - 1) nextPosLeft checkedPositions keepSearching |> ignore
            searchInDirection nextPosRight (nextRightX, nextRightY + 1) checkedPositions keepSearching
        | _ ->
            addToCheckedPositions nextPosLeft nextPosRight checkedPositions
            searchInDirection nextPosLeft nextPosRight checkedPositions keepSearching

    let mutable keepSearching = { value = true }
    let x, y = currentPosition
    let checkedPositions = HashSet<Option<int * int>>()

    let initialSearch =
        match direction with
        | StandardDirection.Up when matrix[x - 1][y] = ']' ->
            searchInDirection (x - 1, y - 1) (x - 1, y) checkedPositions keepSearching
        | StandardDirection.Up -> searchInDirection (x - 1, y) (x - 1, y + 1) checkedPositions keepSearching
        | StandardDirection.Down when matrix[x + 1][y] = ']' ->
            searchInDirection (x + 1, y - 1) (x + 1, y) checkedPositions keepSearching
        | _ -> searchInDirection (x + 1, y) (x + 1, y + 1) checkedPositions keepSearching

    initialSearch |> Seq.rev |> Seq.toList

let rec moveLarge
    (currentPosition: int * int)
    (matrix: ResizeArray<ResizeArray<char>>)
    (moves: ResizeArray<StandardDirection * int>)
    =

    // remove the head move if no more steps are left
    let removeHeadIfZero (moves: ResizeArray<StandardDirection * int>) =
        if (snd moves[0] = 0) then
            moves.RemoveAt(0)

    let processVerticalMovement
        (matrix: ResizeArray<ResizeArray<char>>)
        (currentPosition: int * int)
        (nextPosition: int * int)
        (direction: StandardDirection)
        (moves: ResizeArray<StandardDirection * int>)
        (sortDirection: (int * int) seq -> (int * int) seq)
        =
        let nextEmptyPositions =
            tryFindNextEmptyPositionUpOrDown matrix direction currentPosition

        match nextEmptyPositions |> Seq.tryFind ((=) None) with
        | Some _ ->
            moves.RemoveAt(0)
            moveLarge currentPosition matrix moves
        | None ->
            let delta = if direction = StandardDirection.Up then 1 else -1

            nextEmptyPositions
            |> Seq.choose id
            |> sortDirection
            |> Seq.iter (fun pos ->
                let x, y = pos
                let prevX = x + delta
                let prevChar = matrix[prevX][y]
                matrix[x][y] <- prevChar
                matrix[prevX][y] <- '.')

            // Update the remaining moves and positions
            moves[0] <- (direction, snd moves[0] - 1)
            matrix[fst currentPosition][snd currentPosition] <- '.'
            matrix[fst nextPosition][snd nextPosition] <- '@'

            removeHeadIfZero moves
            moveLarge nextPosition matrix moves

    if (moves.Count = 0) then
        matrix
    else
        let currentDirection, currentSteps = Seq.head moves
        let nextX, nextY = nextPositionStandard currentPosition currentDirection
        let currentX, currentY = currentPosition

        if matrix[nextX][nextY] = '#' then // cant move more in this direction
            moves.RemoveAt(0)
            moveLarge currentPosition matrix moves

        else if matrix[nextX][nextY] = '.' then // can move in this direction
            moves[0] <- (currentDirection, currentSteps - 1)
            matrix[nextX][nextY] <- '@'
            matrix[currentX][currentY] <- '.'
            removeHeadIfZero moves
            moveLarge (nextX, nextY) matrix moves

        else if currentDirection = StandardDirection.Up then
            processVerticalMovement matrix currentPosition (nextX, nextY) currentDirection moves (Seq.sortBy fst)
        else if currentDirection = StandardDirection.Down then
            processVerticalMovement matrix currentPosition (nextX, nextY) currentDirection moves (Seq.sortByDescending fst)

        else // move left or right
            let move x y direction =
                let delta = if direction = StandardDirection.Left then 1 else -1
                let mutable finalY = y

                while finalY <> snd currentPosition do
                    matrix[x][finalY] <- matrix[x][finalY + delta]
                    matrix[x][finalY + delta] <- '.'
                    finalY <- finalY + delta

                moves[0] <- (currentDirection, currentSteps - 1)
                removeHeadIfZero moves
                moveLarge (nextX, nextY) matrix moves

            let nextEmptyPosition =
                tryFindNextEmptyPosition matrix currentDirection currentPosition

            match nextEmptyPosition with
            | Some(x, y) -> move x y currentDirection
            | None ->
                moves.RemoveAt(0)
                moveLarge currentPosition matrix moves

let part2 () =
    let (matrix, startPosition), moves = readMatrixAndGroups filePath '@'

    let groups = moves |> List.head |> Array.concat |> groupAndCount

    match startPosition with
    | Some startPosition ->
        let startPosition = fst startPosition, snd startPosition * 2
        let matrix = moveLarge startPosition (largeWarehouse matrix) (ResizeArray(groups))
        gpsValue matrix '['
    | None -> failwith "No start position found"
