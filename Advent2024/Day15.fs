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

let tryFindNextEmptyPosition2
    (matrix: ResizeArray<ResizeArray<char>>)
    (direction: StandardDirection)
    (currentPosition: int * int)
    =

    let rec searchInDirection left right (checkedPositions: HashSet<Option<(int * int)>>) (keepSearching: Search) =
        let nextLeftX, nextLeftY = nextPositionStandard left direction
        let nextRightX, nextRightY = nextPositionStandard right direction

        match matrix[nextLeftX][nextLeftY], matrix[nextRightX][nextRightY] with
        | '#', _ ->
            keepSearching.value <- false
            checkedPositions.Add(None) |> ignore
            checkedPositions
        | _, '#' ->
            keepSearching.value <- false
            checkedPositions.Add(None) |> ignore
            checkedPositions
        | '.', '.' ->
            checkedPositions.Add(Some(nextLeftX, nextLeftY)) |> ignore
            checkedPositions.Add(Some(nextRightX, nextRightY)) |> ignore
            checkedPositions
        | '.', '[' ->
            checkedPositions.Add(Some(nextLeftX, nextLeftY)) |> ignore
            checkedPositions.Add(Some(nextRightX, nextRightY)) |> ignore
            searchInDirection (nextRightX, nextRightY) (nextRightX, nextRightY + 1) checkedPositions keepSearching
        | ']', '.' ->
            checkedPositions.Add(Some(nextLeftX, nextLeftY)) |> ignore
            checkedPositions.Add(Some(nextRightX, nextRightY)) |> ignore
            searchInDirection (nextLeftX, nextLeftY - 1) (nextLeftX, nextLeftY) checkedPositions keepSearching
        | ']', '[' ->
            checkedPositions.Add(Some(nextLeftX, nextLeftY)) |> ignore
            checkedPositions.Add(Some(nextRightX, nextRightY)) |> ignore

            searchInDirection (nextLeftX, nextLeftY - 1) (nextLeftX, nextLeftY) checkedPositions keepSearching
            |> ignore

            searchInDirection (nextRightX, nextRightY) (nextRightX, nextRightY + 1) checkedPositions keepSearching
        | _ ->
            checkedPositions.Add(Some(nextLeftX, nextLeftY)) |> ignore
            checkedPositions.Add(Some(nextRightX, nextRightY)) |> ignore
            searchInDirection (nextLeftX, nextLeftY) (nextRightX, nextRightY) checkedPositions keepSearching

    let mutable keepSearching = { value = true }
    let x, y = currentPosition
    let checkedPositions = HashSet<Option<(int * int)>>()

    if direction = StandardDirection.Up && matrix[x - 1][y] = ']' then
        searchInDirection (x - 1, y - 1) (x - 1, y) checkedPositions keepSearching
        |> Seq.rev
        |> Seq.toList
    elif direction = StandardDirection.Up && matrix[x - 1][y] = '[' then
        searchInDirection (x - 1, y) (x - 1, y + 1) checkedPositions keepSearching
        |> Seq.rev
        |> Seq.toList
    elif direction = StandardDirection.Down && matrix[x + 1][y] = ']' then
        searchInDirection (x + 1, y - 1) (x + 1, y) checkedPositions keepSearching
        |> Seq.rev
        |> Seq.toList
    else
        searchInDirection (x + 1, y) (x + 1, y + 1) checkedPositions keepSearching
        |> Seq.rev
        |> Seq.toList

let rec moveLarge
    (currentPosition: int * int)
    (matrix: ResizeArray<ResizeArray<char>>)
    (moves: ResizeArray<StandardDirection * int>)
    =

    if (moves.Count = 0) then
        //printMatrix matrix
        matrix
    else
        // printMatrix matrix
        // printfn "Current position: %A" (moves.Item(0))

        // remove the head move if no more steps are left
        let removeHeadIfZero (moves: ResizeArray<StandardDirection * int>) =
            if (snd moves[0] = 0) then
                moves.RemoveAt(0)

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

            let nextEmptyPositions =
                tryFindNextEmptyPosition2 matrix currentDirection currentPosition

            let none = nextEmptyPositions |> Seq.tryFind (fun pos -> pos = None)

            match none with
            | Some _ ->
                moves.RemoveAt(0)
                moveLarge currentPosition matrix moves
            | None ->
                nextEmptyPositions
                |> Seq.choose id
                |> Seq.sortBy fst
                |> Seq.iter (fun pos ->
                    let x, y = pos
                    let prevChar = matrix[x + 1][y]
                    matrix[x][y] <- prevChar
                    matrix[x + 1][y] <- '.')

                moves[0] <- (currentDirection, currentSteps - 1)
                matrix[fst currentPosition][snd currentPosition] <- '.'
                matrix[nextX][nextY] <- '@'

                removeHeadIfZero moves
                moveLarge (nextX, nextY) matrix moves

        else if currentDirection = StandardDirection.Down then

            let nextEmptyPositions =
                tryFindNextEmptyPosition2 matrix currentDirection currentPosition

            let none = nextEmptyPositions |> Seq.tryFind (fun pos -> pos = None)

            match none with
            | Some _ ->
                moves.RemoveAt(0)
                moveLarge currentPosition matrix moves
            | None ->
                nextEmptyPositions
                |> Seq.choose id
                |> Seq.sortByDescending fst
                |> Seq.iter (fun pos ->
                    let x, y = pos
                    let prevChar = matrix[x - 1][y]
                    matrix[x][y] <- prevChar
                    matrix[x - 1][y] <- '.')

                moves[0] <- (currentDirection, currentSteps - 1)
                matrix[fst currentPosition][snd currentPosition] <- '.'
                matrix[nextX][nextY] <- '@'

                removeHeadIfZero moves
                moveLarge (nextX, nextY) matrix moves

        else
            let move x y delta =
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
            | Some(x, y) ->
                if currentDirection = StandardDirection.Left then
                    move x y 1
                else
                    move x y -1

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
