module Advent2024.Day7

open System.IO
open System

type Component = uint64

type Result =
    { Value: uint64
      Components: Component list }

type Operation =
    | Plus
    | Multiply
    | Merge

let parseLine (line: string) =
    let parts = line.Split([| ':' |], StringSplitOptions.RemoveEmptyEntries)
    let resultValue = parts[0].Trim() |> uint64

    let components =
        parts[1].Trim().Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map uint64
        |> Array.toList

    { Value = resultValue
      Components = components }

let readResults filePath =
    File.ReadAllLines(filePath) |> Array.map parseLine |> Array.toList

let mergeNumbers (a: uint64) (b: uint64) =
    let rec countDigits (n:uint64) =
        if n < 10UL then 1
        else 1 + countDigits (n / 10UL)

    let digitsInB = countDigits b
    let factor = uint64 (pown 10 digitsInB)
    a * factor + b

let applyOperation (a: uint64) (b: uint64) operation =
    match operation with
    | Plus -> a + b
    | Multiply -> a * b
    | Merge -> mergeNumbers a b

let rec permutateOperations length operations =
    match length with
    | 1 -> operations |> List.map (fun op -> [ op ])
    | _ ->
        operations
        |> List.collect (fun op -> permutateOperations (length - 1) operations |> List.map (fun ops -> op :: ops))

let calculateValues operations (components: Component list) =
    let permutations = permutateOperations (List.length components - 1) operations

    permutations
    |> List.map (fun ops ->
        let head = List.head components
        List.fold2 applyOperation head (List.tail components) ops)

let tryFindValue (values: uint64 list) value =
    values |> List.tryFind (fun v -> v = value)

let calculateCalibrationValues operations =
    let results =
        readResults "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day7.txt"

    let calculations =
        results |> List.map _.Components |> List.map (calculateValues operations)

    results
    |> List.mapi (fun i result -> tryFindValue (calculations.Item(i)) result.Value)
    |> List.choose id
    |> List.sum

let part1 () =
    calculateCalibrationValues [ Plus; Multiply ]

let part2 () =
    calculateCalibrationValues [ Plus; Multiply; Merge ]
