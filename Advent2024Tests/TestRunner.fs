module Advent2024Tests.TestRunner

open Advent2024
open Expecto

[<EntryPoint>]
let main argv =
    let allTests =
        testList
            "All Tests"
            [
              Day2Tests.matrixTests
              Day2Tests.readAllLinesTests
              Day3Tests.part1Tests
              Day3Tests.part2Tests
              Day4Tests.part1Tests
              Day21Tests.tests
              Day15Tests.tests ]

    runTestsWithCLIArgs [] argv allTests
