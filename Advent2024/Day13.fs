module Advent2024.Day13

open System
open System.IO
open Advent2024.Common

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day13.txt"

type Button = { Coordinate: Coordinate }

type Prize = { Coordinate: Coordinate }

type Game =
    { ButtonA: Button
      ButtonB: Button
      Prize: Prize }

let buildMatrix size (A: Button) (B: Button) =
    let matrix = ResizeArray<ResizeArray<Coordinate>>()

    for _ in 0..size do
        let row = ResizeArray<Coordinate>()

        for _ in 0..size do
            row.Add(Coordinate.Create(0, 0))

        matrix.Add(row)

    matrix[0][0] <- Coordinate.Create(0, 0)

    for i in 1..size do
        matrix[i][0] <- matrix[i - 1][0] + A.Coordinate

    for j in 1..size do
        matrix[0][j] <- matrix[0][j - 1] + B.Coordinate

    for i in 1..size do
        for j in 1..size do
            matrix[i][j] <- matrix[0][j - 1] + matrix[i - 1][0]

    matrix

let parseCoordinate (input: string) =
    let parts = input.Split([| '='; '+' |], StringSplitOptions.RemoveEmptyEntries)

    let x = parts[1].Split(',').[0].Trim() |> int
    let y = parts[2].Trim() |> int
    Coordinate.Create(x, y)

let parseGame (lines: string array) =
    let buttonA =
        lines[0].Split(':').[1].Trim()
        |> parseCoordinate
        |> fun coord -> ({ Coordinate = coord }: Button)

    let buttonB =
        lines[1].Split(':').[1].Trim()
        |> parseCoordinate
        |> fun coord -> ({ Coordinate = coord }: Button)

    let prize =
        lines[2].Split(':').[1].Trim()
        |> parseCoordinate
        |> fun coord -> ({ Coordinate = coord }: Prize)

    { ButtonA = buttonA
      ButtonB = buttonB
      Prize = prize }

let readGames filePath =
    let lines = File.ReadAllLines(filePath)

    lines
    |> Array.chunkBySize 4 // Each game takes 3 lines + 1 empty line
    |> Array.map (Array.take 3) // Take only the 3 content lines
    |> Array.map parseGame
    |> Array.toList

let findPrize prize (matrix: ResizeArray<ResizeArray<Coordinate>>) =
    // Find matching coordinates
    let matches =
        seq {
            for i in 0 .. matrix.Count - 1 do
                for j in 0 .. matrix[i].Count - 1 do
                    let coord = matrix[i][j]

                    if coord.X = prize.Coordinate.X && coord.Y = prize.Coordinate.Y then
                        yield (i - 1, j - 1, coord)
        }
        |> Seq.map (fun (a, b, _) -> (a * 3 + b))

    if Seq.isEmpty matches then 0 else (Seq.min matches)

// Brute force solution (not suitable for part2)
let part1_1 () =
    let games = readGames filePath

    games
    |> List.map (fun game -> buildMatrix 100 game.ButtonA game.ButtonB |> findPrize game.Prize)
    |> List.sum

// Solving using linear optimization ( Diophantine equations )
// We want to find values a and b, that minimize the cost of the claw machine. The cost is given by: C = 3a + b
// The two equations given by the problem are:
// Ax * a + Bx * b = Px
// Ay * a + By * b = Py,
// where a and b represent the number of presses of the Button A and ButtonB that will result in the Prize.
let solveClawMachine (buttonA: Button) (buttonB: Button) (prize: Prize) (offset: int64) =
    let Ax = int64 buttonA.Coordinate.X
    let Ay = int64 buttonA.Coordinate.Y
    let Bx = int64 buttonB.Coordinate.X
    let By = int64 buttonB.Coordinate.Y
    let Px = int64 prize.Coordinate.X + offset
    let Py = int64 prize.Coordinate.Y + offset

    // Scale the equations to eliminate `a` (we could also do it the other way around and eliminate b first)
    let _, bCoeff1, pCoeff1 = (Ay * Ax, Ay * Bx, Ay * Px) // Multiply Equation 1 by A_y
    let _, bCoeff2, pCoeff2 = (Ax * Ay, Ax * By, Ax * Py) // Multiply Equation 2 by A_x

    // Subtract scaled equations ( `a` is then eliminated )
    let bCoeff = bCoeff2 - bCoeff1
    let pCoeff = pCoeff2 - pCoeff1

    // Solve for `b`
    if bCoeff = 0L || pCoeff % bCoeff <> 0L then
        None // No integer solution
    else
        let b = pCoeff / bCoeff

        // Substitute `b` back into Equation 1 to solve for `a`
        if Ax = 0L || (Px - Bx * b) % Ax <> 0L then
            None // No integer solution
        else
            let a = (Px - Bx * b) / Ax

            // Verify if the solution is valid
            if Ax * a + Bx * b = Px && Ay * a + By * b = Py then
                Some(a, b)
            else
                None

let part1 () =
    let games = readGames filePath

    games
    |> List.map (fun game -> solveClawMachine game.ButtonA game.ButtonB game.Prize 0L)
    |> List.choose id
    |> List.map (fun (a, b) -> (a * 3L + b))
    |> List.sum

let part2 () =
    let games = readGames filePath

    games
    |> List.map (fun game -> solveClawMachine game.ButtonA game.ButtonB game.Prize 10000000000000L)
    |> List.choose id
    |> List.map (fun (a, b) -> (a * 3L + b))
    |> List.sum
