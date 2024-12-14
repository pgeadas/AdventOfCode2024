module Advent2024Tests.Day4Tests

open Expecto
open Advent2024.Day4

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
                    MXMXAXMASX
                    MMAMMMMMMM"""

              let expected =
                  input().Split('\n')
                  |> Array.toList
                  |> List.map (fun line -> line.Trim().ToCharArray() |> Array.toList)

              let result = readAllLines input
              Expect.equal result expected "Should return the correct result for part1"
          } ]
