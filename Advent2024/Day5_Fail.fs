module Advent2024.Day5_Fail

open System
open System.Collections.Generic

// Failed attempt to solve the problem with an OrderedSet, but keeping it here for reference

type OrderedSet<'T when 'T: equality>(initialElements: 'T list) =
    let existingValues = HashSet<'T>(initialElements)
    let queue = ResizeArray<'T>(initialElements)

    new() = OrderedSet([])

    member this.Add(value: 'T) =
        let removeFromQueue =
            if existingValues.Contains(value) then
                let index = queue.IndexOf(value)

                if index >= 0 then
                    queue.RemoveAt(index)

        removeFromQueue
        queue.Add(value)

    member this.Contains(value: 'T) = existingValues.Contains(value)

    member this.AsList() = List.ofSeq queue

let readAllLines readLineFn =
    let rec readLines list =
        let input = readLineFn ()

        if String.IsNullOrWhiteSpace(input) then
            List.rev list
        else
            let toPair (arr: string array) = arr[0], arr[1]

            let values = input.Trim().Split('|') |> toPair
            readLines (values :: list)

    readLines []

let readAllLinesList readLineFn =
    let rec readLines list =
        let input = readLineFn ()

        if String.IsNullOrWhiteSpace(input) then
            List.rev list
        else
            let values = input.Trim().Split(',') |> List.ofArray
            readLines (values :: list)

    readLines []

let existsInSet (values: string list) (set: OrderedSet<string>) = values |> List.forall set.Contains

let checkIfUpdatesAreValid (updates: string list list) (dict: Dictionary<string, OrderedSet<string>>) =
    let rec checkRemaining updatesList results =
        match updatesList with
        | [] -> List.rev results
        | first :: rest ->
            let instructions = dict.TryGetValue(first)

            match instructions with
            | true, set ->
                let result = existsInSet rest set
                checkRemaining rest (result :: results)
            | false, _ -> checkRemaining rest (true :: results)

    updates |> List.map (fun updatesList -> checkRemaining updatesList [])

let toDictionary (pageOrderings: (string * string) list) =
    let dict = Dictionary<string, OrderedSet<string>>()

    pageOrderings
    |> List.iter (fun (key, value) ->
        if dict.ContainsKey(key) then
            dict[key].Add(value) |> ignore
        else
            dict[key] <- OrderedSet<string>([ value ]))

    dict

let part1 (pageOrderings: (string * string) list) updates =
    let dict = toDictionary pageOrderings
    Seq.iter (fun key -> printfn $"%A{key}: %A{dict[key]}") dict.Keys
