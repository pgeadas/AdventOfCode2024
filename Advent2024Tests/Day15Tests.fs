module Advent2024Tests.Day15Tests

open Advent2024.Common
open Expecto

// TIP: using `ftest` instead of test will ignore the other tests in the test list. Useful for debugging.
// TIP: using `testSequenced` will run the tests in the order they are defined instead of running them in parallel.

let toCharArray (str: string) =
    str.Split("\n")
    |> Array.filter (fun s -> s.Length > 0)
    |> Array.map _.ToCharArray()

let convertToResizeArray (matrix: char[][]) : ResizeArray<ResizeArray<char>> =
    ResizeArray(matrix |> Array.map (fun row -> ResizeArray(row)))

let resizeArray2DToList (matrix: ResizeArray<ResizeArray<'T>>) : List<List<'T>> =
    matrix |> Seq.map List.ofSeq |> List.ofSeq

let pushBoxesUp =
    testList
        "push boxes up"
        [

          test "Push single box up" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[][]..##
##....[].@..##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##......[]..##
##....[].@..##
##....[]....##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Up, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the box to be pushed up"
          }

          test "Push single blocked box up" {
              let initial =
                  """
##############
##..........##
##.....[]...##
##....[][]..##
##....[].@..##
##..........##
##############"""

              let expected =
                  """
##############
##.....[]...##
##......[]..##
##....[].@..##
##....[]....##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Up, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the box to be pushed up"
          }

          test "Push double blocked box up" {
              let initial =
                  """
##############
##..........##
##.....[]...##
##....[][]..##
##....[][]..##
##.......@..##
##############"""

              let expected =
                  """
##############
##.....[]...##
##......[]..##
##....[][]..##
##....[].@..##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Up, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (5, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed up"
          }

          test "Push multiple blocked boxes up" {
              let initial =
                  """
##############
##..........##
##.....[][].##
##....[][]..##
##....[].@..##
##..........##
##############"""

              let expected =
                  """
##############
##.....[][].##
##......[]..##
##....[].@..##
##....[]....##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Up, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed up"
          }

          test "Push triple blocked boxes up" {
              let initial =
                  """
##############
##..........##
##....[]....##
##....[][]..##
##.....[]...##
##.....@....##
##############"""

              let expected =
                  """
##############
##....[]....##
##....[][]..##
##.....[]...##
##.....@....##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Up, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (5, 7) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed up"
          } ]

let pushBoxesDown =
    testList
        "push single box down"
        [

          test "Push single box" {
              let initial =
                  """
##############
##......##..##
##.......@..##
##....[][]..##
##....[]....##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[].@..##
##....[][]..##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Down, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (2, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the box to be pushed down"
          }

          test "Push multiple blocked boxes down" {
              let initial =
                  """
##############
##.....@....##
##.....[]...##
##....[][]..##
##....[]....##
##..........##
##############"""

              let expected =
                  """
##############
##..........##
##.....@....##
##.....[]...##
##....[][]..##
##....[]....##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Down, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (1, 7) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed down"
          }

          // Parameterized tests
          yield!
              [ for row, col in [ (1, 9); (1, 10) ] ->
                    test $"Push single blocked boxes down, from ({row},{col})" {
                        let initial =
                            """
##############
##..........##
##.....[][].##
##....[][]..##
##....[]....##
##..........##
##############"""

                        let expected =
                            """
##############
##..........##
##.....[].@.##
##....[].[].##
##....[][]..##
##..........##
##############"""

                        let matrix = toCharArray initial
                        let resizableMatrix = convertToResizeArray matrix

                        let expectedMatrix = toCharArray expected

                        let resizableExpectedMatrix =
                            convertToResizeArray expectedMatrix |> resizeArray2DToList

                        let moves = ResizeArray([ (StandardDirection.Down, 1) ])

                        let result =
                            Advent2024.Day15.moveLarge (1, 10) resizableMatrix moves |> resizeArray2DToList

                        Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed down"
                    } ] ]

let pushBoxesRight =
    testList
        "push boxes to the right"
        [

          test "Push single box right, with boxes on the left" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[][]..##
##.[][].@[].##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[][]..##
##.[][]..@[]##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Right, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 8) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the box to be pushed right"
          }

          test "Push double boxes right, squeezed with 2 boxes on the left" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[][]..##
##[][]@[][].##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[][]..##
##[][].@[][]##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Right, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 6) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed right"
          }

          test "Push double box right" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[]....##
##...@[][]..##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[]....##
##....@[][].##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Right, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 5) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed right"
          } ]

let pushBoxesLeft =
    testList
        "push boxes to the left"
        [

          test "Push single box left" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[][]..##
##....[]@...##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[][]..##
##...[]@....##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Left, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 8) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the box to be pushed left"
          }

          test "Push double box left" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[]....##
##...[][]@..##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[]....##
##..[][]@...##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Left, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 9) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed left"
          }

          test "Push double box left, with boxes on the right" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[]....##
##.[][]@.[].##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[]....##
##[][]@..[].##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Left, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 7) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed left"
          }


          test "Push double box to the left squeezed with boxes on the right" {
              let initial =
                  """
##############
##......##..##
##..........##
##....[]....##
##.[][]@[][]##
##..........##
##############"""

              let expected =
                  """
##############
##......##..##
##..........##
##....[]....##
##[][]@.[][]##
##..........##
##############"""

              let matrix = toCharArray initial
              let resizableMatrix = convertToResizeArray matrix

              let expectedMatrix = toCharArray expected

              let resizableExpectedMatrix =
                  convertToResizeArray expectedMatrix |> resizeArray2DToList

              let moves = ResizeArray([ (StandardDirection.Left, 1) ])

              let result =
                  Advent2024.Day15.moveLarge (4, 7) resizableMatrix moves |> resizeArray2DToList

              Expect.sequenceEqual result resizableExpectedMatrix "Expected the boxes to be pushed left"
          } ]

[<Tests>]
let tests =
    testSequenced
    <| testList "all box movement tests" [ pushBoxesUp; pushBoxesDown; pushBoxesRight; pushBoxesLeft ]
