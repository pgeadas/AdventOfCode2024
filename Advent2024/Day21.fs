module Advent2024.Day21

open System.Collections.Generic
open System.IO
open System
open Advent2024.Common
open Advent2024.Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day21.txt"

type Keypad = char array list

let numKeypad: Keypad =
    [ [| '7'; '8'; '9' |]
      [| '4'; '5'; '6' |]
      [| '1'; '2'; '3' |]
      [| '#'; '0'; 'A' |] ]

let dirKeypad: Keypad = [ [| '#'; '^'; 'A' |]; [| '<'; 'v'; '>' |] ]

let createLookupTable (keypad: Keypad) =
    keypad
    |> List.mapi (fun rowIndex row ->
        row
        |> Array.mapi (fun colIndex value -> (value, Coordinate.Create(rowIndex, colIndex))))
    |> Seq.concat
    |> dict // Tuple pairs to Dictionary

let numpadLookup = createLookupTable numKeypad

let dirpadLookup = createLookupTable dirKeypad

let positionOfChar (lookupTable: IDictionary<char, Coordinate>) char =
    match lookupTable.TryGetValue char with
    | true, position -> Some position
    | false, _ -> None

let readAllLines filePath =
    File.ReadLines(filePath)
    |> Seq.filter (fun line -> not (String.IsNullOrWhiteSpace(line)))

// let createAdjacencyMatrix (keypad: Keypad) =
//     let rows, cols = keypad.Length, keypad.[0].Length
//
//     let adjacency =
//         Array.init rows (fun x ->
//             Array.init cols (fun y ->
//                 Array.init StandardDirection.Count (fun index ->
//                     // Make sure we respect the index order defined in StandardDirection
//                     let direction, _ = StandardDirection.Indexed[index]
//                     let adjX, adjY = nextPositionStandard (x, y) direction
//
//                     if isValidPosition rows cols (adjX, adjY) && keypad[adjX].[adjY] <> '#' then
//                         Some keypad[adjX].[adjY]
//                     else
//                         None)))
//
//     adjacency

type Cost = int
let moveCost: Cost = 1

let findShortestPaths (matrix: char array list) (startPos: Coordinate) (endPos: Coordinate) =
    let rows, cols = matrixSize matrix

    let cost = Array.init rows (fun _ -> Array.init cols (fun _ -> Int32.MaxValue))

    // Track multiple predecessors for each position
    let predecessors =
        Array.init rows (fun _ -> Array.init cols (fun _ -> ResizeArray<Coordinate>()))

    let queue = PriorityQueue<Coordinate * Cost, int>()

    cost[startPos.X].[startPos.Y] <- 0
    queue.Enqueue((startPos, 0), 0)

    let move (position: Coordinate) (currentCost: Cost) (fromPos: Coordinate) =
        let totalCost = currentCost + moveCost
        let currentBestCost = cost[position.X].[position.Y]

        if totalCost <= currentBestCost then

            if totalCost < currentBestCost then
                cost[position.X].[position.Y] <- totalCost
                // If this is a new best cost, clear previous predecessors
                predecessors[position.X].[position.Y].Clear()
                queue.Enqueue((position, totalCost), totalCost)

            // Add the new path to predecessors, since it is a new best cost
            predecessors[position.X].[position.Y].Add(fromPos)

    let rec processQueue () =
        if queue.Count = 0 then
            None
        else
            let currentPos, currentCost = queue.Dequeue()

            if currentPos = endPos then
                Some(currentCost, currentPos)
            else
                for dir in StandardDirection.Values do
                    let nextPos =
                        nextPositionStandard (currentPos.X, currentPos.Y) dir |> Coordinate.Create

                    if isValidCoordinate rows cols nextPos && matrix[nextPos.X][nextPos.Y] <> '#' then
                        move nextPos currentCost currentPos

                processQueue ()

    // Reconstruct all paths from predecessors
    let rec buildPaths pos =
        let predList = predecessors[pos.X].[pos.Y]

        if predList.Count = 0 then
            [ [ pos ] ]
        else
            [ for prevPos in predList do
                  for path in buildPaths prevPos do
                      yield pos :: path ]

    match processQueue () with
    | Some(cost, endPos) ->
        let paths = (buildPaths endPos) // we need to return all paths, because even though they have the same cost, the order of presses will influence the next robot (because of the keypad order)
        Some(paths)
    | None -> None

let toStartEndPosGroups lookupTable (codes: string list) =
    codes
    |> List.map _.ToCharArray()
    |> List.map (fun chars ->
        chars
        |> Array.map (positionOfChar lookupTable)
        |> Array.choose id
        |> Array.pairwise
        |> Array.toList)

let prependA (codes: string list) = codes |> List.map (fun s -> "A" + s)

let findShortestPathsForTuples keypad startAndEndPos =
    startAndEndPos
    |> List.map (fun (startPos, endPos) -> findShortestPaths (List.ofSeq keypad) startPos endPos)
    |> List.choose id

let buildInstructionsList paths =
    let fromDirection direction =
        match direction with
        | Up -> '^'
        | Right -> '>'
        | Down -> 'v'
        | Left -> '<'

    let buildPath (path: Coordinate list) =
        path
        |> List.pairwise
        |> List.map (fun (coord1, coord2) ->
            let previousDir = inferDirection (coord1.X, coord1.Y) (coord2.X, coord2.Y)
            fromDirection previousDir)

    let res = paths |> List.map (fun pathList -> pathList |> List.map buildPath)

    let mergeChars (chars: char list) =
        ("A", chars) ||> List.fold (fun acc char -> string char + acc)

    // we get the list of possible min paths with A appended
    res |> List.map (fun path -> path |> List.map mergeChars)

let rec permuteNestedLists nestedLists =
    match nestedLists with
    | [] -> [ [] ]
    | head :: tail ->
        let tailPermutations = permuteNestedLists tail
        // Combine each element of the head list with permutations of the tail
        head |> List.collect (fun h -> tailPermutations |> List.map (fun t -> h :: t))

let filterStringsBySize (stringLists: string list list) (sizes: int list) =
    if List.length stringLists <> List.length sizes then
        failwith "The number of string lists and size specifications must match."

    List.zip stringLists sizes
    |> List.map (fun (stringList, size) ->
        // Filter each string in the sublist based on the specified size
        stringList |> List.filter (fun str -> String.length str = size))

let concatInstructions (instructions: string list list list) =
    instructions
    |> List.map (fun list -> list |> List.map (fun list -> list |> String.concat ""))

let doStuff lookupTable (codes: string list list) =
    let prependedNumPadInstructions = codes |> List.map prependA

    let startEndPosGroups =
        prependedNumPadInstructions |> List.map (toStartEndPosGroups lookupTable)

    let paths =
        startEndPosGroups
        |> List.map (fun list -> list |> List.map (fun list -> list |> findShortestPathsForTuples dirKeypad))

    let instructionsList =
        paths |> List.map (fun list -> list |> List.map permuteNestedLists)

    let instructions =
        instructionsList
        |> List.map (fun list -> list |> List.map buildInstructionsList)

    let result =
        instructions
        |> List.map (fun list -> list |> concatInstructions)
        |> List.map (fun list -> list |> List.collect id)

    let minSizes =
        result |> List.map (fun list -> list |> List.map _.Length |> List.min)

    printfn "minSizes: %A" minSizes

    let result =
        result
        |> List.mapi (fun i list -> list |> List.filter (fun s -> s.Length = minSizes.Item(i)))

    printfn "%A" result
    result

let part1 () =
    let codes = readAllLines filePath
    let prependedCodes = codes |> Seq.toList |> prependA
    let startEndPosGroups = toStartEndPosGroups numpadLookup prependedCodes

    let paths = startEndPosGroups |> List.map (findShortestPathsForTuples numKeypad)

    let numPadInstructions =
        paths |> List.map permuteNestedLists |> List.map buildInstructionsList

    let res = numPadInstructions |> concatInstructions

    printfn "%A" res
    let minSizes = res |> List.map (fun list -> list |> List.map _.Length |> List.min)

    printfn "min1: %A" minSizes

    let robot2 = doStuff dirpadLookup res

    let robot3 = doStuff dirpadLookup robot2

    let nums =
        codes
        |> Seq.map (fun code -> code.Substring(0, code.Length - 1) |> int)
        |> Seq.toList

    let minSizes =
        robot3 |> List.map (fun list -> list |> List.map _.Length |> List.min)

    printfn "min3: %A" minSizes
    printfn "%A" nums

    List.zip minSizes nums |> List.map (fun (size, num) -> size * num) |> List.sum
