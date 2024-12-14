module Advent2024.Common

type StandardDirection =
    | Right
    | Left
    | Up
    | Down

type DiagonalDirection =
    | UpRight
    | UpLeft
    | DownRight
    | DownLeft

type ExtendedDirection =
    | Standard of StandardDirection
    | Diagonal of DiagonalDirection

type Coordinate(x: int, y: int) =
    member this.X = x
    member this.Y = y

    member this.Add(other: Coordinate) =
        Coordinate(this.X + other.X, this.Y + other.Y)

    static member (+)(a: Coordinate, b: Coordinate) = Coordinate(a.X + b.X, a.Y + b.Y)

    override this.ToString() = $"X={this.X}, Y={this.Y}"
