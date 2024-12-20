module Advent2024.Day4

open System
open Advent2024.Common
open Advent2024.Matrix

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
    let usedPositions = Collections.Generic.HashSet<int * int>()
    let mutable totalFound = 0

    // Check if word exists starting from position in given direction
    let rec checkWord (x, y) direction word positions =
        if List.isEmpty word then
            // Found complete word, mark positions as used
            positions |> List.iter (fun pos -> usedPositions.Add(pos) |> ignore)
            totalFound <- totalFound + 1
            true
        else if isValidPosition rows cols (x, y) then
            let currentChar = matrix[x][y]

            if currentChar = List.head word then
                let nextPos = nextPositionExtended (x, y) direction
                checkWord nextPos direction (List.tail word) ((x, y) :: positions)
            else
                false
        else
            false

    // Search from every position
    for x in 0 .. rows - 1 do
        for y in 0 .. cols - 1 do
            for direction in directions do
                let word = [ 'X'; 'M'; 'A'; 'S' ]
                checkWord (x, y) direction word [] |> ignore

    // Create result matrix with dots for unused positions
    let occupied =
        matrix
        |> List.mapi (fun x row ->
            row
            |> Array.mapi (fun y char -> if usedPositions.Contains(x, y) then char else '.'))

    //printfn $"%A{occupied}"
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

    // Result matrix initialized with dots
    let resultMatrix = Array.init rows (fun _ -> Array.create cols '.')

    // Traverse the matrix and identify valid star patterns
    let count =
        [ for x in 0 .. rows - 1 do
              for y in 0 .. cols - 1 do
                  if matrix[x][y] = 'A' && checkStar (x, y) then
                      // Mark center 'A'
                      resultMatrix[x][y] <- 'A'
                      // Mark diagonals if they are valid
                      diagonalPairs
                      |> List.iter (fun ((dx1, dy1), (dx2, dy2)) ->
                          let x1, y1 = (x + dx1, y + dy1)
                          let x2, y2 = (x + dx2, y + dy2)

                          if isValidPosition (x1, y1) then
                              resultMatrix[x1][y1] <- matrix[x1][y1]

                          if isValidPosition (x2, y2) then
                              resultMatrix[x2][y2] <- matrix[x2][y2])

                      yield 1 ]
        |> List.sum

    count

let part1 () =
    findXMAS (readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day4.txt")

let part2 () =
    countValidStars (readMatrix "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day4.txt")
