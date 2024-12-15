module Advent2024.Node

open System
open System.Collections.Generic

type Node<'T when 'T: comparison>(value: 'T) =
    let adjacents = HashSet<Node<'T>>()

    member this.Value = value
    member this.Adjacents = adjacents

    member this.AddAdjacent(node: Node<'T>) = adjacents.Add(node) |> ignore

    interface IComparable with
        member this.CompareTo(obj) =
            match obj with
            | :? Node<'T> as other -> compare this.Value other.Value
            | _ -> invalidArg "obj" "Cannot compare values of different types."

    override this.Equals(obj) =
        match obj with
        | :? Node<'T> as other -> this.Value = other.Value
        | _ -> false

    override this.GetHashCode() = hash this.Value

    override this.ToString() =
        let adjacentValues =
            adjacents
            |> Seq.toList
            |> List.map _.Value
            |> List.map string
            |> String.concat ", "

        sprintf "Node(%A) -> [%s]" this.Value adjacentValues
