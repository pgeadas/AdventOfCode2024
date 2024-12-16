module Advent2024.Day14

open System
open System.IO
open Advent2024.Common

type Velocity = { dx: int; dy: int }

type Robot =
    { originalPos: Coordinate
      mutable position: Coordinate
      velocity: Velocity }

    // we can use the mod operator to wrap around the coordinates
    // Normalizing with the expression "((value % n) + n) % n" ensures non-negative results.
    member this.MoveSteps(x, y, steps) =
        let newX = ((this.originalPos.X + this.velocity.dx * steps) % x + x) % x
        let newY = ((this.originalPos.Y + this.velocity.dy * steps) % y + y) % y
        this.position <- Coordinate.Create(newX, newY)
        this

let parseRobotLine (line: string) =
    let parts =
        line.Split([| "p="; " "; "v="; "," |], StringSplitOptions.RemoveEmptyEntries)

    let x = parts[0] |> int
    let y = parts[1] |> int
    let dx = parts[2] |> int
    let dy = parts[3] |> int

    { originalPos = Coordinate.Create(x, y)
      position = Coordinate.Create(x, y)
      velocity = { dx = dx; dy = dy } }

let readRobots filePath =
    File.ReadAllLines(filePath) |> Array.map parseRobotLine |> Array.toList

let moveRobots width height steps (robots: Robot list) =
    robots |> List.map _.MoveSteps(width, height, steps)

type Quadrant =
    | TopLeft
    | TopRight
    | BottomLeft
    | BottomRight

let determineQuadrant width height (position: Coordinate) =
    let midX = width / 2
    let midY = height / 2

    match position.X, position.Y with
    | x, y when x < midX && y < midY -> Some TopLeft
    | x, y when x > midX && y < midY -> Some TopRight
    | x, y when x < midX && y > midY -> Some BottomLeft
    | x, y when x > midX && y > midY -> Some BottomRight
    | _ -> None

let countRobotsByQuadrant width height (robots: Robot list) =
    robots
    |> List.fold
        (fun (topLeft, topRight, bottomLeft, bottomRight) robot ->
            match determineQuadrant width height robot.position with
            | Some TopLeft -> (topLeft + 1, topRight, bottomLeft, bottomRight)
            | Some TopRight -> (topLeft, topRight + 1, bottomLeft, bottomRight)
            | Some BottomLeft -> (topLeft, topRight, bottomLeft + 1, bottomRight)
            | Some BottomRight -> (topLeft, topRight, bottomLeft, bottomRight + 1)
            | None -> (topLeft, topRight, bottomLeft, bottomRight))
        (0, 0, 0, 0)

let printMatrix width height (robotPositions: Robot list) =
    // Create a 2D array for the current step in the matrix
    let matrix = Array.init height (fun _ -> Array.create width '.')

    // Mark robot positions with 'X'
    robotPositions
    |> List.iter (fun robot ->
        let x = robot.position.X
        let y = robot.position.Y
        matrix[y][x] <- 'X')

    for row in matrix do
        printfn "%s" (String(row))

    let hasConsecutiveXChars amount (row: char array) =
        let rec checkConsecutive count idx =
            if idx >= row.Length then
                false
            elif row[idx] = 'X' then
                if count + 1 = amount then
                    true
                else
                    checkConsecutive (count + 1) (idx + 1)
            else
                checkConsecutive 0 (idx + 1)

        if row.Length < amount then false else checkConsecutive 0 0

    let filterRows = matrix |> Array.filter (hasConsecutiveXChars 10)

    if filterRows.Length > 0 then
        true
    else
        printfn ""
        false

let printRobotsMovement width height steps (robots: Robot list) =
    for step in 0..steps do
        printfn "Step %d:" step

        for robot in robots do
            robot.MoveSteps(width, height, step) |> ignore

        if (printMatrix width height robots) then
            exit 0

let part1 () =
    let robots =
        readRobots "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day14.txt"

    let width, height, steps = 101, 103, 100
    let movedRobots = moveRobots width height steps robots
    let q1, q2, q3, q4 = countRobotsByQuadrant width height movedRobots
    let res = q1 * q2 * q3 * q4
    res

let part2 () =
    let robots =
        readRobots "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day14.txt"

    let width, height, steps = 101, 103, 10_000
    printRobotsMovement width height steps robots
