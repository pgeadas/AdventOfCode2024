module Advent2024.Day5

open System
open System.Collections.Generic
open System.IO
open Advent2024.Node

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day5.txt"

let readAndTransform filePath =
    let rec splitOnEmptyLine remaining acc =
        match remaining with
        | [] -> (List.rev acc, [])
        | line :: rest ->
            if String.IsNullOrWhiteSpace(line) then
                (acc, rest)
            else
                splitOnEmptyLine rest (line :: acc)

    let lines = File.ReadLines filePath |> Seq.toList

    let toPair (arr: string array) = arr[0], arr[1]
    let transformBefore (line: string) = line.Trim().Split('|') |> toPair
    let transformAfter (line: string) = line.Trim().Split(',') |> List.ofArray

    let beforeEmptyLine, afterEmptyLine = splitOnEmptyLine lines []

    let pairs = beforeEmptyLine |> List.map transformBefore
    let instructions = afterEmptyLine |> List.map transformAfter

    (pairs, instructions)

let toNodes1 (pageOrderings: (string * string) list) =
    pageOrderings
    |> List.map (fun (origin, destination) -> Node<string>(origin), Node<string>(destination))

let toNodes2 (instructions: string list list) =
    instructions |> List.map (fun list -> list |> List.map Node)

let isPathValid (instructions: Node<'T> list) (nodes: HashSet<Node<'T>>) =
    let rec validate (path: Node<'T> list) =
        match path with
        | []
        | [ _ ] -> true
        | firstName :: secondName :: rest ->
            match nodes.TryGetValue(firstName) with
            | true, firstNode ->
                match nodes.TryGetValue(secondName) with
                | true, secondNode ->
                    if firstNode.Adjacents.Contains(secondNode) then
                        validate (secondName :: rest)
                    else
                        false
                | false, _ -> false // Second node not found
            | false, _ -> false // First node not found

    validate instructions

let findValidPathsIndexes (instructions: Node<'T> list list) (nodes: HashSet<Node<'T>>) =
    instructions
    |> List.mapi (fun index instructions -> if isPathValid instructions nodes then Some index else None)
    |> List.choose id

let createGraph (pageOrderings: (Node<string> * Node<string>) list) =
    let graph = HashSet<Node<string>>()

    pageOrderings
    |> List.iter (fun (origin, destination) ->
        graph.Add(origin) |> ignore
        graph.Add(destination) |> ignore)

    pageOrderings
    |> List.iter (fun (origin, destination) ->
        match graph.TryGetValue(origin) with
        | true, node ->
            match graph.TryGetValue(destination) with
            | true, dest -> node.AddAdjacent(dest)
            | _ -> failwith "cant happen"
        | _ -> failwith "cant happen")

    graph

let getMiddleNodes (nodesInstructions: Node<string> list list) =
    let getMiddleValue (list: Node<string> list) =
        let midIndex = (List.length list) / 2
        list[midIndex]

    nodesInstructions |> List.map getMiddleValue

let topologicalSort (graph: HashSet<Node<'T>>) =
    let visited = HashSet<Node<'T>>()
    let sorted = List<Node<'T>>()

    let rec dfs (node: Node<'T>) =
        if visited.Contains(node) then
            failwith "Cycle detected"

        visited.Add(node) |> ignore

        for adj in node.Adjacents do
            if not (visited.Contains(adj)) then
                dfs adj

        sorted.Insert(0, node)

    for node in graph do
        if not (visited.Contains(node)) then
            dfs node

    sorted |> Seq.toList

let sortGraph (graph: HashSet<Node<'T>>) =
    let sortedNodes = topologicalSort graph
    sortedNodes |> List.mapi (fun i node -> node.Value, i) |> Map.ofList

let getInstructionsAtIndices (instructions: Node<string> list list) (indices: int list) =
    indices |> List.map (fun index -> instructions[index])

let getIncorrectIndices (instructions: Node<'T> list list) (indices: int list) =
    let allIndices = Set.ofList [ 0 .. List.length instructions - 1 ]
    let validIndices = Set.ofList indices
    Set.difference allIndices validIndices |> Set.toList

let fixInstruction (instruction: Node<string> list) (topologicalSortedMap: Map<string, int>) =
    instruction |> List.sortBy (fun node -> topologicalSortedMap[node.Value])

let part1 () =
    let pageOrderings, instructions = readAndTransform filePath
    let nodesPageOrderings = toNodes1 pageOrderings
    let nodesInstructions = toNodes2 instructions

    let graph = createGraph nodesPageOrderings
    //printfn $"graph: %A{Seq.toList graph}"
    let validIndexes = findValidPathsIndexes nodesInstructions graph

    let middleNodes =
        getMiddleNodes (getInstructionsAtIndices nodesInstructions validIndexes)

    let sumMiddleNodes = middleNodes |> List.map _.Value |> List.map int |> List.sum

    // validIndexes, sumMiddleNodes
    sumMiddleNodes

// Failed attempt to solve the problem using topological order.
let part2 (instructions: Node<string> list list) (indices: int list) graph =
    let invalidIndices = getIncorrectIndices instructions indices
    let wrongInstructions = getInstructionsAtIndices instructions invalidIndices
    let topologicalSortedMap = sortGraph graph

    let fixedInstructions =
        wrongInstructions
        |> List.map (fun instruction -> fixInstruction instruction topologicalSortedMap)

    let middleNodes = getMiddleNodes fixedInstructions
    let sumMiddleNodes = middleNodes |> List.map _.Value |> List.map int |> List.sum
    sumMiddleNodes
