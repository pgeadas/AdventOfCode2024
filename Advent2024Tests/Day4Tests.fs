module Advent2024Tests.Day4Tests

open Advent2024
open Expecto
open Matrix

let filePath = "/Users/pgeadas/RiderProjects/Advent2024/Advent2024Tests/inputs/Day4Test.txt"

let part1Tests =
    testList
        "readAllLines Tests"
        [ test "Should read all lines and convert them to an array of chars" {
              let input =
                  fun () ->
                      """MMMSXXMASM
                    MSAMXMSMSA
                    AMXSXMAAMM
                    MSAMASMSMX
                    XMASAMXAMM
                    XXAMMXXAMA
                    SMSMSASXSS
                    SAXAMASAAA
                    MAMMMXMMMM
                    MXMXAXMASX"""

              let expected =
                  input().Split('\n')
                  |> Array.toList
                  |> List.map _.Trim().ToCharArray()

              let result = readMatrix filePath
              Expect.equal result expected "Should return the correct result for part1"
          } ]
