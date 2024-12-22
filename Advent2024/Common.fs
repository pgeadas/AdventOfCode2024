module Advent2024.Common

type StandardDirection =
    | Right
    | Left
    | Up
    | Down

    member this.TurnRight() =
        match this with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up

    member this.TurnLeft() =
        match this with
        | Up -> Left
        | Left -> Down
        | Down -> Right
        | Right -> Up

    member this.Index() =
        match this with
        | Up -> 0
        | Down -> 1
        | Left -> 2
        | Right -> 3

    static member Indexed = [ (Up, 0); (Down, 1); (Left, 2); (Right, 3) ]

    static member Count = 4

    static member Values = [ Up; Down; Left; Right ]

let nextPositionStandard (x, y) (direction: StandardDirection) =
    match direction with
    | Right -> (x, y + 1)
    | Left -> (x, y - 1)
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)

let inferDirection (currentPosX, currentPosY) (previousPosX, previousPosY) =
    match currentPosX - previousPosX, currentPosY - previousPosY with
    | 0, 1 -> Right
    | 0, -1 -> Left
    | 1, 0 -> Down
    | -1, 0 -> Up
    | _ -> failwith "Step bigger than 1 is not supported"

type DiagonalDirection =
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft

type ExtendedDirection =
    | Standard of StandardDirection
    | Diagonal of DiagonalDirection

let nextPositionExtended (x, y) (direction: ExtendedDirection) =
    match direction with
    | Standard Right -> (x, y + 1)
    | Standard Left -> (x, y - 1)
    | Standard Up -> (x - 1, y)
    | Standard Down -> (x + 1, y)
    | Diagonal UpRight -> (x - 1, y + 1)
    | Diagonal UpLeft -> (x - 1, y - 1)
    | Diagonal DownRight -> (x + 1, y + 1)
    | Diagonal DownLeft -> (x + 1, y - 1)

[<Struct>]
type Coordinate =
    { X: int
      Y: int }

    static member Create(x, y) = { X = x; Y = y }

    member this.Add(other: Coordinate) =
        { X = this.X + other.X
          Y = this.Y + other.Y }

    static member (+)(a: Coordinate, b: Coordinate) = { X = a.X + b.X; Y = a.Y + b.Y }

    static member (-)(a: Coordinate, b: Coordinate) = { X = a.X - b.X; Y = a.Y - b.Y }

    override this.ToString() = $"X={this.X}, Y={this.Y}"
