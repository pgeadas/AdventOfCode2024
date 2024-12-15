module Advent2024.Day9

open System.IO

let readInput () =
    File.ReadLines("/Users/pgeadas/RiderProjects/Advent2024/Advent2024/inputs/Day9.txt")
    |> Seq.head
    |> Seq.map (string >> int)
    |> Array.ofSeq

let rec unfold (input: int array) currIndex fileIndex acc =
    if currIndex >= Array.length input then
        acc
    elif currIndex % 2 = 0 then
        let replicated = List.init input[currIndex] (fun _ -> string fileIndex)
        unfold input (currIndex + 1) (fileIndex + 1) (acc @ replicated)
    else
        let replicated = List.init input[currIndex] (fun _ -> ".")
        unfold input (currIndex + 1) fileIndex (acc @ replicated)

let countFileNumber currentIndex = currentIndex |> string |> _.Length

let compact input dots =
    let input = input |> Array.ofList

    let rec compact array currIndex lastIndex (acc: string list) =
        if currIndex >= Array.length input - dots then
            acc
        elif input[currIndex] = "." then
            let lastValue = input[lastIndex]

            if lastValue = "." then
                compact array currIndex (lastIndex - 1) acc
            else
                compact array (currIndex + 1) (lastIndex - 1) (acc @ [ lastValue ])
        else
            compact array (currIndex + 1) lastIndex (acc @ [ input[currIndex] ])

    compact input 0 (input.Length - 1) []

let checksum (input: string list) : uint64 =
    input
    |> List.map (string >> int)
    |> List.mapi (fun i v -> uint64 i * uint64 v)
    |> List.sum

let groupAndCount (input: string list) =
    input
    |> List.fold
        (fun acc c ->
            match acc with
            | (prevChar, count) :: tail when prevChar = c -> (prevChar, count + 1) :: tail
            | _ -> (c, 1) :: acc)
        []
    |> List.rev

let insertAtDots (input: (string * int) list) =
    let input = ResizeArray(input)

    let rec defragment (input: ResizeArray<string * int>) right =
        if right <= 0 then
            input
        else
            let tail = input[right]
            let fileX, fileSize = tail

            if fileX = "." then // compact dots
                let dots = input |> Seq.skip right |> Seq.takeWhile (fun (x, _) -> x = ".")

                if Seq.length dots > 1 then
                    let emptySpace = Seq.sumBy snd dots
                    input.RemoveRange(right, Seq.length dots)
                    input.Insert(right, (".", emptySpace))

                defragment input (right - 1)
            else
                let emptySpaceOption =
                    input |> Seq.take right |> Seq.tryFind (fun (x, y) -> x = "." && y >= fileSize)

                match emptySpaceOption with
                | Some emptySpace ->
                    let _, emptySize = emptySpace
                    let emptySpaceIndex = input.IndexOf emptySpace

                    let moveTailToFittingSpace =
                        input.Insert(right, (".", fileSize))
                        input.Remove(tail) |> ignore
                        input.Insert(emptySpaceIndex, tail)
                        input.Remove(emptySpace) |> ignore

                    if fileSize > emptySize then
                        defragment input (right - 1)
                    elif emptySize = fileSize then
                        moveTailToFittingSpace
                        defragment input (right - 1)
                    else
                        let remainingSpace = emptySize - fileSize
                        moveTailToFittingSpace
                        input.Insert(emptySpaceIndex + 1, (".", remainingSpace)) // add remaining empty space
                        defragment input right // we added an extra element, so do not decrease the right

                | None -> defragment input (right - 1)

    defragment input (Seq.length input - 1)

let checksumPart2 (input: (string * int) list) : uint64 =
    let rec count (start: int) (acc: uint64) (lst: (string * int) list) =
        match lst with
        | [] -> acc
        | (fileId, fileSize) :: tail ->
            let finish = start + fileSize
            let range = [ start .. finish - 1 ]
            let (sum: uint64) = range |> List.sumBy (fun index -> uint64 index * uint64 fileId)
            count finish (acc + sum) tail

    input
    |> List.map (fun (fileId, fileSize) -> if fileId = "." then ("0", fileSize) else (fileId, fileSize))
    |> count 0 0UL

let part1 () =
    let input = readInput ()
    let unfolded = unfold input 0 0 []
    let dots = unfolded |> Seq.filter (fun c -> c = ".") |> Seq.length
    let compacted = compact unfolded dots
    checksum compacted

let part2 () =
    let input = readInput ()
    let unfolded = unfold input 0 0 []
    let grouped = groupAndCount unfolded
    let defragmented = insertAtDots grouped |> List.ofSeq
    checksumPart2 defragmented
