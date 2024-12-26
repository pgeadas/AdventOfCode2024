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

let buildMatrix (A: Button) (B: Button) =
    let size = 100
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
        |> Seq.map (fun (i, j, _) -> (i * 3 + j))

    if Seq.isEmpty matches then 0 else (Seq.min matches)

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


let part1 () =
    let games = readGames filePath

    games
    |> List.map (fun game -> buildMatrix game.ButtonA game.ButtonB |> findPrize game.Prize)
    |> List.sum
