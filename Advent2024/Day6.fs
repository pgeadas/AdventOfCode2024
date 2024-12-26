module Advent2024.Day6

open System.Collections.Generic
open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day6.txt"

let rotate90 direction =
    match direction with
    | Up -> Right
    | Right -> Down
    | Down -> Left
    | Left -> Up

let part1 () =
    let matrix, startPosition = readAndFindFirst filePath '^'

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


let part2 () =
    let matrix, startPosition = readAndFindFirst filePath '^'

    let rows, cols = matrixSize matrix

    let rec countSteps
        (currentPosition: int * int)
        (direction: StandardDirection)
        (visited: HashSet<(int * int) * StandardDirection>)
        =
        let nextPosition = nextPositionStandard currentPosition direction

        if visited.Contains(nextPosition, direction) then
            1
        else
            visited.Add(nextPosition, direction) |> ignore

            if isValidPosition rows cols nextPosition then
                let nextChar = matrix[fst nextPosition][snd nextPosition]

                if nextChar = '#' then
                    countSteps currentPosition (rotate90 direction) visited

                elif nextChar = 'X' || nextChar = '^' then
                    countSteps nextPosition direction visited
                else
                    matrix[fst nextPosition][snd nextPosition] <- 'X'
                    let count = countSteps nextPosition direction visited
                    matrix[fst nextPosition][snd nextPosition] <- '.'
                    count
            else
                0

    match startPosition with
    | Some startPosition ->
        let mutable count = 0

        for i in 0 .. rows - 1 do
            for j in 0 .. cols - 1 do
                if matrix[i][j] = '.' || matrix[i][j] = '^' then
                    matrix[i][j] <- '#'
                    count <- count + countSteps startPosition Up (HashSet())
                    matrix[i][j] <- '.'

        count
    | None -> failwith "No start position found"
