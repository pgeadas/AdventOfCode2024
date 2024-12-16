module Advent2024.Matrix

open System.IO

/// <summary>
/// Reads a matrix from a file while searching for the first occurrence of a target character within the matrix.
/// </summary>
/// <param name="filePath">The path of the file containing the matrix.</param>
/// <param name="targetChar">The character to search for within the matrix.</param>
/// <returns>
/// A tuple containing:
/// <list type="bullet">
///   <item>The matrix represented as a list of character arrays, with each array corresponding to a row in the file.</item>
///   <item>The position of the first occurrence of <paramref name="targetChar"/> as an Option of (rowIndex, columnIndex),
///   or None if the character is not found.</item>
/// </list>
/// </returns>
let readAndFindFirst filePath targetChar =
    let processLine (matrix, targetPos) (rowIndex, line) =
        let charArray = Array.ofSeq line

        match targetPos with
        | None ->
            let newTargetPos =
                Seq.tryFindIndex (fun char -> char = targetChar) line
                |> Option.map (fun colIndex -> (rowIndex, colIndex))

            (charArray :: matrix, newTargetPos)
        | Some _ -> (charArray :: matrix, targetPos)

    let matrix, targetPos =
        File.ReadLines(filePath) |> Seq.indexed |> Seq.fold processLine ([], None)

    (List.rev matrix, targetPos)

/// <summary>
/// Reads a matrix from a file and searches for all occurrences of a target character within the matrix.
/// </summary>
/// <param name="filePath">The path of the file containing the matrix.</param>
/// <param name="targetChar">The character to search for within the matrix.</param>
/// <param name="mapper">A function to map each character to another type.</param>
/// <returns>
/// A tuple containing:
/// <list type="bullet">
///   <item>The matrix represented as a list of arrays, with each array corresponding to a row in the file.</item>
///   <item>A list of positions of all occurrences of <paramref name="targetChar"/> as tuples (rowIndex, columnIndex).</item>
/// </list>
/// </returns>
let readAndFindAll<'T> filePath targetChar (mapper: char -> 'T) =
    try
        mapper targetChar
    with _ ->
        failwith "Target character cannot be mapped to the specified type."
    |> ignore

    let processLine (matrix, targets) (rowIndex, line) =
        let mappedRow = line |> Seq.map mapper |> Array.ofSeq

        let lineTargets =
            line
            |> Seq.mapi (fun colIndex char -> if char = targetChar then Some(rowIndex, colIndex) else None)
            |> Seq.choose id
            |> Seq.toList

        (mappedRow :: matrix, lineTargets @ targets)

    let matrix, targetPositions =
        File.ReadLines(filePath) |> Seq.indexed |> Seq.fold processLine ([], [])

    (List.rev matrix, List.rev targetPositions)

/// <summary>
/// Reads a characters matrix from a file and searches for all occurrences of a target character within the matrix.
/// </summary>
/// <param name="filePath">The path of the file containing the matrix.</param>
/// <param name="targetChar">The character to search for within the matrix.</param>
/// <returns>
/// A tuple containing:
/// <list type="bullet">
///   <item>The matrix represented as a list of character arrays, with each array corresponding to a row in the file.</item>
///   <item>A list of positions of all occurrences of targetChar as tuples (rowIndex, columnIndex).</item>
/// </list>
/// </returns>
let readAndFindAllChar filePath targetChar = readAndFindAll filePath targetChar id

/// <summary>
/// Reads a matrix from a file and searches for all occurrences of a target character within the matrix.
/// The target character and matrix are mapped to the literal integer they represent and not to their character's integer value.
/// </summary>
/// <param name="filePath">The path of the file containing the matrix.</param>
/// <param name="targetChar">The character to search for within the matrix.</param>
/// <returns>
/// A tuple containing:
/// <list type="bullet">
///   <item>The matrix represented as a list of integer arrays, with each array corresponding to a row in the file.</item>
///   <item>A list of positions of all occurrences of targetChar as tuples (rowIndex, columnIndex).</item>
/// </list>
/// </returns>
let readAndFindAllInt filePath targetChar =
    let charToDigit c =
        if c >= '0' && c <= '9' then
            int c - int '0'
        else
            failwithf "Character %c is not a valid digit" c

    readAndFindAll filePath targetChar charToDigit

/// <summary>
/// Calculates the size of the given matrix.
/// </summary>
/// <param name="matrix">A matrix represented as a list of arrays, with each array corresponding to a row.</param>
/// <returns>A tuple containing the number of rows and columns in the matrix.</returns>
let matrixSize matrix =
    let rows = List.length matrix
    let cols = List.head matrix |> Array.length
    rows, cols

/// <summary>
/// Determines if a given position is valid within the bounds of the matrix.
/// </summary>
/// <param name="rows">The total number of rows in the matrix.</param>
/// <param name="cols">The total number of columns in the matrix.</param>
/// <param name="x">The x-coordinate (row index) of the position being checked.</param>
/// <param name="y">The y-coordinate (column index) of the position being checked.</param>
/// <returns><c>true</c> if the position (x, y) is within the bounds of the matrix; otherwise, <c>false</c>.</returns>
let isValidPosition rows cols (x, y) =
    x >= 0 && x < rows && y >= 0 && y < cols

/// <summary>
/// Reads a matrix and groups of strings from a file, separated by empty lines.
/// </summary>
/// <param name="filePath">The path of the file containing the matrix and additional data.</param>
/// <param name="targetChar">The character to search for within the matrix.</param>
/// <returns>
/// A tuple containing:
/// <list type="bullet">
///   <item>The matrix and target position from the first group.</item>
///   <item>A list of string lists representing subsequent groups separated by empty lines.</item>
/// </list>
/// </returns>
let readMatrixAndGroups filePath targetChar =
    let processLine (matrix, targetPos) (rowIndex, line) =
        let charArray = Array.ofSeq line

        match targetPos with
        | None ->
            let newTargetPos =
                Seq.tryFindIndex (fun char -> char = targetChar) line
                |> Option.map (fun colIndex -> (rowIndex, colIndex))

            (charArray :: matrix, newTargetPos)
        | Some _ -> (charArray :: matrix, targetPos)

    let lines = File.ReadLines(filePath) |> Seq.indexed

    // Read until first empty line to get matrix
    let matrix, targetPos, remainingLines =
        let rec readMatrix acc lines =
            match Seq.tryHead lines with
            | None -> acc, Seq.empty
            | Some(_, "") -> acc, Seq.skip 1 lines
            | Some(idx, line) ->
                let newAcc = processLine acc (idx, line)
                readMatrix newAcc (Seq.skip 1 lines)

        let (matrix, pos), remaining = readMatrix ([], None) lines
        (List.rev matrix, pos, remaining)

    // Group remaining lines
    let groups =
        remainingLines
        |> Seq.map snd // Get just the line content
        |> Seq.fold
            (fun (currentGroup, allGroups) line ->
                if line = "" then
                    if List.isEmpty currentGroup then
                        ([], allGroups)
                    else
                        ([], Array.ofSeq (List.rev currentGroup) :: allGroups)
                else
                    (Array.ofSeq line :: currentGroup, allGroups))
            ([], [])
        |> function
            | [], groups -> List.rev groups
            | lastGroup, groups -> List.rev (Array.ofSeq (List.rev lastGroup) :: groups)

    (matrix, targetPos), groups
