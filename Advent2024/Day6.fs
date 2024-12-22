module Advent2024.Day6

open Advent2024.Common
open Advent2024.Matrix

let rotate90 direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let part1 () =
    let matrix, startPosition =
        readAndFindFirst "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day6.txt" '^'

    let rows, cols = matrixSize matrix

    let rec countSteps (currentPosition: int * int) (direction: StandardDirection) (steps: int) =
        let nextPosition = nextPositionStandard currentPosition direction

        if isValidPosition rows cols nextPosition then
            let nextChar = matrix[fst nextPosition][snd nextPosition]

            if nextChar = '#' then
                countSteps currentPosition (rotate90 direction) steps
            elif nextChar = 'X' || nextChar = '^' then
                countSteps nextPosition direction steps
            else
                matrix[fst nextPosition][snd nextPosition] <- 'X'
                countSteps nextPosition direction (steps + 1)
        else
            steps

    match startPosition with
    | Some startPosition -> countSteps startPosition Up 1
    | None -> failwith "No start position found"
