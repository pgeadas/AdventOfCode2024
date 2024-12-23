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

    let move (nextPos: Coordinate) (currentCost: Cost) (fromPos: Coordinate) =
        let totalCost = currentCost + moveCost
        let currentBestCost = cost[nextPos.X].[nextPos.Y]

        if totalCost <= currentBestCost then

            if totalCost < currentBestCost then
                cost[nextPos.X].[nextPos.Y] <- totalCost
                // If this is a new best cost, clear previous predecessors
                predecessors[nextPos.X].[nextPos.Y].Clear()
                queue.Enqueue((nextPos, totalCost), totalCost)

            // Add the new path to predecessors, since it is a new best cost
            predecessors[nextPos.X].[nextPos.Y].Add(fromPos)

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

    let fromDirection direction =
        match direction with
        | Up -> '^'
        | Right -> '>'
        | Down -> 'v'
        | Left -> '<'

    // Reconstruct all paths from predecessors
    let rec buildPaths (pos, prevDir) =
        let predList = predecessors[pos.X].[pos.Y]

        if predList.Count = 0 then
            [ [ pos, prevDir ] ]
        else
            [ for prevPos in predList do
                  for path in buildPaths (prevPos, prevDir) do
                      let previousDir = inferDirection (pos.X, pos.Y) (prevPos.X, prevPos.Y)
                      let prevDir = fromDirection previousDir
                      yield (pos, prevDir) :: path ]

    match processQueue () with
    | Some(_, endPos) ->
        // we need to return all paths, because even though they have the same cost, the order of presses will influence the next robot (because of the keypad order)
        let paths = (buildPaths (endPos, ' '))
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

let buildInstructionsList (paths: (Coordinate * char) list list list) =
    let mergeChars (chars: (Coordinate * char) list) =
        ("A", chars)
        ||> List.fold (fun acc (_, char) -> if char <> ' ' then string char + acc else acc)

    // we get the list of possible min paths with A appended
    paths |> List.map (List.map mergeChars)

let rec permuteNestedLists nestedLists =
    match nestedLists with
    | [] -> [ [] ]
    | head :: tail ->
        let tailPermutations = permuteNestedLists tail
        // Combine each element of the head list with permutations of the tail
        head |> List.collect (fun h -> tailPermutations |> List.map (fun t -> h :: t))

let concatInstructions (instructions: string list list list) =
    instructions |> List.map (List.map (String.concat ""))

let calculateMoves lookupTable (codes: string list list) =
    let prependedNumPadInstructions = codes |> List.map prependA

    let startEndPosGroups =
        prependedNumPadInstructions |> List.map (toStartEndPosGroups lookupTable)

    let paths =
        startEndPosGroups |> List.map (List.map (findShortestPathsForTuples dirKeypad))

    let instructions =
        paths
        |> List.map (List.map permuteNestedLists)
        |> List.map (List.map buildInstructionsList)
        |> List.map concatInstructions
        |> List.map (List.collect id)

    let minSizes = instructions |> List.map (List.map _.Length) |> List.min

    instructions
    |> List.mapi (fun i list -> list |> List.filter (fun s -> s.Length = minSizes.Item(i)))

// TODO: Use memoization, it is a bit slow but still works!
let part1 () =
    let codes = readAllLines filePath
    let prependedCodes = codes |> Seq.toList |> prependA
    let startEndPosGroups = toStartEndPosGroups numpadLookup prependedCodes
    let paths = startEndPosGroups |> List.map (findShortestPathsForTuples numKeypad)

    let numPadInstructions =
        paths
        |> List.map permuteNestedLists
        |> List.map buildInstructionsList
        |> concatInstructions

    let keypad1Instructions = calculateMoves dirpadLookup numPadInstructions
    let keypad2Instructions = calculateMoves dirpadLookup keypad1Instructions

    let codeValues =
        codes
        |> Seq.map (fun code -> code.Substring(0, code.Length - 1) |> int)
        |> Seq.toList

    let minSizes =
        keypad2Instructions
        |> List.map (fun list -> list |> List.map _.Length |> List.min)

    List.zip minSizes codeValues
    |> List.map (fun (size, num) -> size * num)
    |> List.sum