module Advent2024.Day23

open System.Collections.Generic
open System.IO

type Node = string

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day23.txt"

let createNodeMap (filePath: string) =
    let parseLine (line: string) =
        let parts = line.Split('-')
        parts[0], parts[1]

    // Add adjacency to a node in the map
    let addAdjacency (map: Dictionary<Node, ResizeArray<Node>>) (node1: Node, node2: Node) =
        if map.ContainsKey(node1) then
            map[node1].Add(node2)
        else
            map[node1] <- ResizeArray<Node>([ node2 ])
        // Since the graph is undirected, add the reverse as well
        if map.ContainsKey(node2) then
            map[node2].Add(node1)
        else
            map[node2] <- ResizeArray<Node>([ node1 ])

    let nodeMap = Dictionary<Node, ResizeArray<Node>>()

    File.ReadLines(filePath)
    |> Seq.iter (fun line ->
        let node1, node2 = parseLine line
        addAdjacency nodeMap (node1, node2))

    nodeMap

let findTripleConnectedNodes (nodeMap: Dictionary<Node, ResizeArray<Node>>) =
    let triples = ResizeArray<Node * Node * Node>()

    let checkNode node =
        match nodeMap.TryGetValue(node) with
        | true, neighbors -> // Check combinations of adjacent nodes
            for i in 0 .. (neighbors.Count - 2) do
                for j in (i + 1) .. (neighbors.Count - 1) do
                    let neighbor1 = neighbors[i]
                    let neighbor2 = neighbors[j]

                    // check if it is not removed already
                    if nodeMap.ContainsKey(neighbor1) && nodeMap.ContainsKey(neighbor2) then
                        let neighbors1 = nodeMap[neighbor1]
                        let neighbors2 = nodeMap[neighbor2]

                        if neighbors1.Contains(neighbor2) && neighbors2.Contains(neighbor1) then
                            triples.Add((node, neighbor1, neighbor2))

            nodeMap.Remove(node) |> ignore

        | false, _ -> ()

    for node in nodeMap.Keys do
        checkNode node

    triples

let filterTriplesBy (letter: string) (connectedTriples: seq<Node * Node * Node>) =
    let startsWithT (node: Node) = node.StartsWith(letter)

    seq {
        for node, neighbor1, neighbor2 in connectedTriples do
            if startsWithT node || startsWithT neighbor1 || startsWithT neighbor2 then
                yield (node, neighbor1, neighbor2)
    }

let part1 () =
    let nodeMap = createNodeMap filePath
    let connectedTriples = findTripleConnectedNodes nodeMap
    connectedTriples |> filterTriplesBy "t" |> Seq.length

let getMaxScoreNodes (scores: Dictionary<Node * Node * Node, int>) =
    let maxScore = scores |> Seq.maxBy _.Value |> _.Value

    scores
    |> Seq.filter (fun kvp -> kvp.Value = maxScore)
    |> Seq.map _.Key
    |> Seq.fold (fun acc (node, node2, node3) -> node :: node2 :: node3 :: acc) []
    |> List.distinct

let countConnectedNodes connectedTriples =
    let counts = Dictionary<Node, int>()

    let count node =
        match counts.TryGetValue(node) with
        | true, count -> counts[node] <- count + 1
        | false, _ -> counts[node] <- 1

    for node, neighbor1, neighbor2 in connectedTriples do
        count node
        count neighbor1
        count neighbor2

    counts

let scoreNodes (counts: Dictionary<Node, int>) connectedTriples =
    let scores = Dictionary<Node * Node * Node, int>()

    for node, neighbor1, neighbor2 in connectedTriples do
        scores.Add((node, neighbor1, neighbor2), counts[node] + counts[neighbor1] + counts[neighbor2])

    scores

let part2 () =
    let nodeMap = createNodeMap filePath
    let connectedTriples = findTripleConnectedNodes nodeMap
    // count the number of times each node appears in the connected triples
    let counts = countConnectedNodes connectedTriples
    // score the triples based on the previous counts
    let scores = scoreNodes counts connectedTriples
    getMaxScoreNodes scores |> List.sort |> String.concat ","
