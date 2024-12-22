module Advent2024Tests.Day21Tests

open Expecto
open Advent2024.Day21

let tests =
    testList
        "permuteNestedLists Tests"
        [

          test "Empty list returns single empty list" {
              let input = []
              let expected = [ [] ]
              let result = permuteNestedLists input
              Expect.containsAll result expected "Expected a single empty list"
          }

          test "Single nested list returns the list itself" {
              let input = [ [ "a" ] ]
              let expected = [ [ "a" ] ]
              let result = permuteNestedLists input
              Expect.containsAll result expected "Expected the same single nested list"
          }

          test "Permutations of multiple lists" {
              let input = [ [ [ "L1" ] ]; [ [ "L2.1" ]; [ "L2.2" ] ]; [ [ "L3" ] ] ]

              let expected =
                  [ [ [ "L1" ]; [ "L2.1" ]; [ "L3" ] ]; [ [ "L1" ]; [ "L2.2" ]; [ "L3" ] ] ]

              let result = permuteNestedLists input
              Expect.containsAll result expected "Expected permutations of middle list"
          }

          test "Permutations of more multiple lists" {
              let input = [ [ [ "L1.1"; "L1.2" ] ]; [ [ "L2.1" ]; [ "L2.2" ] ]; [ [ "L3.1"; "L3.2" ; "L3.3" ] ] ]

              let expected =
                  [ [ [ "L1.1"; "L1.2" ]; [ "L2.1" ]; [ "L3.1"; "L3.2" ; "L3.3" ] ]
                    [ [ "L1.1"; "L1.2" ]; [ "L2.2" ]; [ "L3.1"; "L3.2" ; "L3.3" ] ] ]

              let result = permuteNestedLists input
              Expect.containsAll result expected "Expected permutations of middle list"
          }

          test "Permutations of even more multiple lists" {
              let input =
                  [ [ [ "L1.1" ]; [ "L1.2" ] ]
                    [ [ "L2.1" ]; [ "L2.2" ] ]
                    [ [ "L3.1" ]; [ "L3.2" ] ] ]

              let expected =
                  [ [ [ "L1.1" ]; [ "L2.1" ]; [ "L3.1" ] ]
                    [ [ "L1.1" ]; [ "L2.2" ]; [ "L3.1" ] ]
                    [ [ "L1.1" ]; [ "L2.2" ]; [ "L3.2" ] ]
                    [ [ "L1.1" ]; [ "L2.1" ]; [ "L3.2" ] ]
                    [ [ "L1.2" ]; [ "L2.1" ]; [ "L3.1" ] ]
                    [ [ "L1.2" ]; [ "L2.1" ]; [ "L3.2" ] ]
                    [ [ "L1.2" ]; [ "L2.2" ]; [ "L3.1" ] ]
                    [ [ "L1.2" ]; [ "L2.2" ]; [ "L3.2" ] ] ]

              let result = permuteNestedLists input
              Expect.containsAll result expected "Expected permutations of middle list"
          }

          ]
