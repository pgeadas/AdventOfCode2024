module Advent2024.Day18

open System.IO
open Advent2024.Common
open Advent2024.ShortestPath

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day18.txt"

let mapSize = 71
let start: Coordinate = { X = 0; Y = 0 }
let finish: Coordinate = { X = mapSize - 1; Y = mapSize - 1 }

let readInput filePath =
    let lines = File.ReadLines(filePath)

    lines |> Seq.map _.Split(',') |> Seq.map (fun parts -> (int parts[1], int parts[0]) |> Coordinate.Create) |> Seq.toList

let findShortestPathCost (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let moveCost: Cost = 1
    findShortestPathCost matrix startPos endPos moveCost

let part1 () =
    let obstacles = readInput filePath
    let matrix = List.init mapSize (fun _ -> Array.init mapSize (fun _ -> '.'))

    for i in 0..1023 do
        let obstacle = obstacles[i]
        matrix[obstacle.X][obstacle.Y] <- '#'

    findShortestPathCost matrix finish start

let part2 () =
    let obstacles = readInput filePath
    let matrix = List.init mapSize (fun _ -> Array.init mapSize (fun _ -> '.'))

    for i in 0..1023 do
        let obstacle = obstacles[i]
        matrix[obstacle.X][obstacle.Y] <- '#'

    let mutable res = 0
    let mutable index = 1023

    while (res <> -1) do
        index <- index + 1
        let obstacle = obstacles[index]
        matrix[obstacle.X][obstacle.Y] <- '#'
        res <- findShortestPathCost matrix finish start

    $"{obstacles[index].Y},{obstacles[index].X}"
