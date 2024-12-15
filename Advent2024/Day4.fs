module Advent2024.Day4

open System
open Advent2024.Common

let readAllLines readLineFn =
    let rec readLines matrix =
        let input = readLineFn ()

        if String.IsNullOrWhiteSpace(input) then
            List.rev matrix
        else
            let values = input |> List.ofSeq
            readLines (values :: matrix)

    readLines []

let (directions: ExtendedDirection array) =
    [| Standard Right
       Standard Left
       Standard Up
       Standard Down
       Diagonal UpRight
       Diagonal UpLeft
       Diagonal DownRight
       Diagonal DownLeft |]

let isValidPosition rows cols (x, y) =
    x >= 0 && x < rows && y >= 0 && y < cols

let matrixSize (matrix: char list list) =
    let rows = List.length matrix
    let cols = List.head matrix |> List.length
    rows, cols

let findXMAS (matrix: char list list) =
    let rows, cols = matrixSize matrix
    let usedPositions = System.Collections.Generic.HashSet<int * int>()
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
            |> List.mapi (fun y char -> if usedPositions.Contains(x, y) then char else '.'))

    printfn $"%A{occupied}"
    totalFound

let countValidStars (matrix: char list list) =
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
                          let pos1 = (x + dx1, y + dy1)
                          let pos2 = (x + dx2, y + dy2)

                          if isValidPosition pos1 then
                              resultMatrix[fst pos1][snd pos1] <- matrix[fst pos1][snd pos1]

                          if isValidPosition pos2 then
                              resultMatrix[fst pos2][snd pos2] <- matrix[fst pos2][snd pos2])

                      yield 1 ]
        |> List.sum

    // Print the resulting matrix
    for row in resultMatrix do
        printfn "%s" (row |> Array.map string |> String.concat "")

    count

let part1 () =
    findXMAS (readAllLines Console.ReadLine)

let part2 () =
    countValidStars (readAllLines Console.ReadLine)
