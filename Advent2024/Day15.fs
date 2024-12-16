module Advent2024.Day15

open Advent2024.Common
open Advent2024.Matrix

let toDirection direction =
    match direction with
    | '^' -> Up
    | '>' -> Right
    | '<' -> Left
    | 'v' -> Down
    | _ -> failwith "Invalid direction"

let groupAndCount (input: char array) =
    input
    |> Array.fold
        (fun acc c ->
            let direction = toDirection c

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

let print (matrix: ResizeArray<ResizeArray<char>>) =
    for row in matrix do
        for char in row do
            printf "%c" char

        printfn ""

    printfn ""


let rec move (currentPosition: int * int) (matrix: ResizeArray<ResizeArray<char>>) (moves: ResizeArray<StandardDirection * int>) =

    if (moves.Count = 0) then
        matrix
    else
        // printfn "Current position: %A" (moves.Item(0))
        // print matrix

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
                matrix[x][y] <- '.'
                matrix[fst currentPosition][snd currentPosition] <- '.'
                moves[0] <- (currentDirection, currentSteps - 1)

                if (snd moves[0] = 0) then
                    moves.RemoveAt(0)

                move (x, y) matrix moves
            | None ->
                moves.RemoveAt(0)
                move currentPosition matrix moves

let gpsValue (matrix: ResizeArray<ResizeArray<char>>) =
    seq {
        for i in 0 .. matrix.Count - 1 do
            for j in 0 .. matrix[i].Count - 1 do
                if matrix[i][j] = 'O' then
                    yield (i, j)
    }
    |> Seq.map (fun (i, j) -> i * 100 + j)
    |> Seq.sum

let toResizeArray2d (matrix: char array list) =
    let array2d = ResizeArray<ResizeArray<char>>()

    for i in 0 .. matrix.Length - 1 do
        let row = ResizeArray<char>()

        for j in 0 .. matrix[i].Length - 1 do
            row.Add(matrix[i][j])

        array2d.Add(row)

    array2d

let part1 () =
    let (matrix, startPosition), list =
        readMatrixAndGroups "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day15.txt" '@'

    let groups = groupAndCount ((List.head list) |> Array.concat)

    match startPosition with
    | Some startPosition ->
        let matrix = move startPosition (toResizeArray2d matrix) (ResizeArray(groups))
        gpsValue matrix
    | None -> failwith "No start position found"
