module Advent2024.Day6

open System.IO
open Advent2024.Common

let readFromFile filePath =
    let mutable targetPosition = -1, -1

    let matrix =
        File.ReadLines(filePath)
        |> Seq.mapi (fun rowIndex line ->
            let charArray = Array.ofSeq line

            line
            |> Seq.tryFindIndex (fun char -> char = '^')
            |> Option.iter (fun columnIndex -> targetPosition <- (rowIndex, columnIndex))

            charArray)
        |> Seq.toList

    matrix, targetPosition

let rotate direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let directions = [| Right; Left; Up; Down |]

let getNextPosition (x, y) direction =
    match direction with
    | Right -> (x, y + 1)
    | Left -> (x, y - 1)
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)

let isValidPosition rows cols (x, y) =
    x >= 0 && x < rows && y >= 0 && y < cols

let matrixSize (matrix: char array list) =
    let rows = List.length matrix
    let cols = List.head matrix |> Array.length
    rows, cols

let part1 matrix (startPosition: int * int) =
    let rows, cols = matrixSize matrix

    let rec countSteps (currentPosition: int * int) (direction: StandardDirection) (steps: int) =
        let nextPosition = getNextPosition currentPosition direction

        if isValidPosition rows cols nextPosition then
            let nextChar = matrix[fst nextPosition][snd nextPosition]

            if nextChar = '#' then
                countSteps currentPosition (rotate direction) steps
            elif nextChar = 'X' || nextChar = '^' then
                countSteps nextPosition direction steps
            else
                matrix[fst nextPosition][snd nextPosition] <- 'X'
                countSteps nextPosition direction (steps + 1)
        else
            steps

    countSteps startPosition Up 1

// [<EntryPoint>]
// let main argv =
//     let input, index =
//         readFromFile "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/Day6.txt"
//
//     // printfn $"%A{input}"
//     // printfn $"%A{index}"
//
//     let result = part1 input index
//     printfn $"%A{result}"
//    // printfn $"%A{input}"
//     0
