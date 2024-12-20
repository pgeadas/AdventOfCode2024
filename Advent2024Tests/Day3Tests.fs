module Advent2024Tests.Day3Tests

open Expecto
open Advent2024.Day3

let getAllMatchesTest () =
    let input =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

    let expected = [ (2, 4); (5, 5); (11, 8); (8, 5) ]
    let result = getAllMatches pattern input
    Expect.equal result expected "Should return a list of tuples extracted from input"

let part1Tests =
    testList
        "tests for part1"
        [ test "Should return the correct result for part1" {
              let input =
                  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

              let expected = 161
              let result = executePart1 input
              Expect.equal result expected "Should return the correct result for part1"
          } ]

let part2Tests =
    testList
        "tests for part2"
        [ test "Should return the correct result for part2" {

              let input =
                   "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

              let expected = 48
              let result = executePart2 input
              Expect.equal result expected "Should return the correct result for part2"
          } ]
