module Advent2024.Day2Tests

open Expecto
open Advent2024.Day2

let readAllLinesTests =
    testList
        "Day2 Tests"
        [ test "readAllLines correctly parses numbers and stops on empty line" {
              let testInputs = "10 20 30 40"
              let expectedOutput = [ 10; 20; 30; 40 ]
              let actualOutput = parseLineToNumbers testInputs
              Expect.equal actualOutput expectedOutput "The outputs did not match"
          } ]


let processMatrix (matrix: int list list) = matrix |> List.concat |> List.sum

let matrixTests =
    testList
        "Matrix Tests"
        [ test "Process matrix and" {
              let matrix = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
              let expectedSum = 45
              let actualSum = processMatrix matrix
              Expect.equal actualSum expectedSum "The sum of all elements should be 45."
          }

          test "Check if matrix is processed correctly" {
              let matrix = [ [ 0; 0; 1 ]; [ 0; 1; 0 ]; [ 1; 0; 0 ] ]
              let expectedSum = 3
              let actualSum = processMatrix matrix
              Expect.equal actualSum expectedSum "The sum of all elements should be 3."
          } ]
