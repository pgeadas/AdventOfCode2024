module Advent2024.Common

type StandardDirection =
    | Right
    | Left
    | Up
    | Down

let nextPositionStandard (x, y) (direction: StandardDirection) =
    match direction with
    | Right -> (x, y + 1)
    | Left -> (x, y - 1)
    | Up -> (x - 1, y)
    | Down -> (x + 1, y)

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

type Coordinate(x: int, y: int) =
    member this.X = x
    member this.Y = y

    member this.Add(other: Coordinate) =
        Coordinate(this.X + other.X, this.Y + other.Y)

    static member (+)(a: Coordinate, b: Coordinate) = Coordinate(a.X + b.X, a.Y + b.Y)

    override this.ToString() = $"X={this.X}, Y={this.Y}"
