module Advent2024.Day4

open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day4.txt"

let (directions: ExtendedDirection array) =
    [| Standard Right
       Standard Left
       Standard Up
       Standard Down
       Diagonal UpRight
       Diagonal UpLeft
       Diagonal DownRight
       Diagonal DownLeft |]

let findXMAS (matrix: char array list) =
    let rows, cols = matrixSize matrix
    let mutable totalFound = 0

    // Check if word exists starting from position in given direction
    let rec checkWord (x, y) direction word =
        if List.isEmpty word then
            totalFound <- totalFound + 1
            true
        else if isValidPosition rows cols (x, y) then
            let currentChar = matrix[x][y]

            if currentChar = List.head word then
                let nextPos = nextPositionExtended (x, y) direction
                checkWord nextPos direction (List.tail word)
            else
                false
        else
            false

    // Search from every position
    for x in 0 .. rows - 1 do
        for y in 0 .. cols - 1 do
            for direction in directions do
                let word = [ 'X'; 'M'; 'A'; 'S' ]
                checkWord (x, y) direction word |> ignore

    totalFound

let countValidStars (matrix: char array list) =
    let rows, cols = matrixSize matrix
    let isValidPosition (x, y) = isValidPosition rows cols (x, y)

    // Define relative positions for diagonals
    let diagonalPairs = [ (-1, -1), (1, 1); (-1, 1), (1, -1) ]

    // Check if for an 'A', diagonals form valid star with alternating 'M' and 'S'
    let checkStar (x, y) =
        diagonalPairs
        |> List.forall (fun ((dx1, dy1), (dx2, dy2)) ->
            let pos1 = (x + dx1, y + dy1)
            let pos2 = (x + dx2, y + dy2)

            if isValidPosition pos1 && isValidPosition pos2 then
                let char1 = matrix[fst pos1][snd pos1]
                let char2 = matrix[fst pos2][snd pos2]
                (char1 = 'M' && char2 = 'S') || (char1 = 'S' && char2 = 'M')
            else
                false)

    // Traverse the matrix and identify valid star patterns
    let count =
        [ for x in 0 .. rows - 1 do
              for y in 0 .. cols - 1 do
                  if matrix[x][y] = 'A' && checkStar (x, y) then
                      yield 1 ]
        |> List.sum

    count

let part1 () = findXMAS (readMatrix filePath)

let part2 () = countValidStars (readMatrix filePath)
