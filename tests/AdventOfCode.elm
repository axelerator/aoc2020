module AdventOfCode exposing (..)

import Day1
import Day10
import Day1Input
import Day2
import Day2Input
import Day3
import Day3Input
import Day4
import Day4Input
import Day5
import Day5Input
import Day6
import Day6Input
import Day7
import Day7Input
import Day8
import Day9
import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import List exposing (length, map)
import List.Extra exposing (unique)
import Parser
import Set
import Test exposing (..)
import Tuple exposing (first, second)



{-

         suite : Test
         suite =
             describe "Day1"
                 [ test "Day one test input calculates correctly"
                     (\_ -> Expect.equal 514579 (Day1.accounting Day1Input.test))
                 , test "Day one test actual input calculates correctly"
                     (\_ -> Expect.equal 955584 (Day1.accounting Day1Input.actual))
                 , test "Day one test input calculates correctly with triple"
                     (\_ -> Expect.equal 241861950 (Day1.accounting3 Day1Input.test))
                 , test "Day one test input calculates correctly with triple with actual input"
                     (\_ -> Expect.equal 287503934 (Day1.accounting3 Day1Input.actual))
                 ]


         suite2 : Test
         suite2 =
             describe
                 "Day2"
                 [ test "Entry parses nothing"
                     (\_ -> Expect.equal Nothing (Day2.mkEntry ""))
                 , test "Entry parses '1-3 a: abcde'"
                     (\_ -> Expect.equal (Just (Day2.Entry ( 1, 3 ) 'a' "abcde")) (Day2.mkEntry "1-3 a: abcde"))
                 , test "Valid Entry is valid"
                     (\_ -> Expect.equal True (Day2.valid (Just <| Day2.Entry ( 1, 3 ) 'a' "abcde")))
                 , test "Invald Test input generates invalid result"
                     (\_ -> Expect.equal False (Day2.valid <| Day2.mkEntry "3-4 h: dnhv"))
                 , test "Test input generates expected result"
                     (\_ -> Expect.equal 2 (Day2.allValid Day2Input.test))
                 , test "Actual output"
                     (\_ -> Expect.equal 424 (Day2.allValid Day2Input.actual))
                 , test "Test input generates expected result with alternative valid"
                     (\_ -> Expect.equal 1 (Day2.allValid2 Day2Input.test))
                 , test "Actual output with alternative rule"
                     (\_ -> Expect.equal 747 (Day2.allValid2 Day2Input.actual))
                 ]


         suite3 : Test
         suite3 =
             describe
                 "Day 3"
                 [ test "Can parse tree"
                     (\_ -> Expect.equal (Ok [ Day3.Tree ]) <| Day3.parseLine 1 "#")
                 , test "Can parse space"
                     (\_ -> Expect.equal (Ok [ Day3.Space ]) <| Day3.parseLine 1 ".")
                 , test "Can parse line"
                     (\_ -> Expect.equal (Ok [ Day3.Space, Day3.Tree ]) <| Day3.parseLine 2 ".#")
                 , test "Can parse test input"
                     (\_ -> expectOk <| Day3.parsePattern Day3Input.test)
                 , test "Parse test input to correct size"
                     (\_ ->
                         expectOkWith
                             (Day3.parsePattern Day3Input.test)
                             (\pattern -> Expect.equal ( 11, 11 ) pattern.size)
                     )
                 , test "Can solve test input"
                     (\_ -> Expect.equal (Ok 7) (Day3.solve Day3Input.test Day3Input.simpleSlope))
                 , test "Can solve test actual input"
                     (\_ -> Expect.equal (Ok 247) (Day3.solve Day3Input.actual Day3Input.simpleSlope))
                 , test "Can solve test input with complex slopes"
                     (\_ -> Expect.equal (Ok 336) (Day3.solve Day3Input.test Day3Input.complexSlopes))
                 , test "Can solve test actual input with multiple slopes"
                     (\_ -> Expect.equal (Ok 2983070376) (Day3.solve Day3Input.actual Day3Input.complexSlopes))
                 ]


         suite4 : Test
         suite4 =
             describe
                 "Day 4"
                 [ test "parses right number of passports from test input"
                     (\_ ->
                         expectOkWith
                             (Parser.run Day4.passportCollectionParser Day4Input.test)
                             (\passports -> Expect.equal 4 (List.length passports))
                     )
                 , test "Identifies right number of valid passports on test input"
                     (\_ ->
                         Expect.equal 2 (Day4.validPassportCount Day4Input.test)
                     )
                 , test "parses right number of passports from actual input"
                     (\_ ->
                         expectOkWith
                             (Parser.run Day4.passportCollectionParser Day4Input.actual)
                             (\passports -> Expect.equal 265 (List.length passports))
                     )
                 , test "Identifies right number of valid passports on actual input"
                     (\_ ->
                         Expect.equal 200 (Day4.validPassportCount Day4Input.actual)
                     )
                 , test "Identifies right number of strict valid passports on actual input"
                     (\_ ->
                         Expect.equal 116 (Day4.strictValidPassportCount Day4Input.actual)
                     )
                 ]


         suite6 : Test
         suite6 =
             describe
                 "Day 6"
                 [ test "test input"
                     (\_ -> Expect.equal 11 <| Day6.solve1 Day6Input.test)
                 , test "actualt input"
                     (\_ -> Expect.equal 6297 <| Day6.solve1 Day6Input.actual)
                 , test "test input part2 "
                     (\_ -> Expect.equal 6 <| Day6.solve2 Day6Input.test)
                 , test "actual input part2"
                     (\_ -> Expect.equal 3158 <| Day6.solve2 Day6Input.actual)
                 ]


         expectOk : Result err value -> Expectation
         expectOk res =
             case res of
                 Err _ ->
                     fail "Not Ok!"

                 _ ->
                     pass


         expectOkWith : Result err value -> (value -> Expectation) -> Expectation
         expectOkWith res expect =
             case res of
                 Err _ ->
                     fail "Not Ok!"

                 Ok value ->
                     expect value


         suite5 : Test
         suite5 =
             describe "Day5"
                 [ test "Can decode seat"
                     (\_ ->
                         Expect.equal
                             (map Tuple.second Day5Input.testSeats)
                             (map (Tuple.first >> Day5.decodeSeat) Day5Input.testSeats)
                     )
                 , test "Can solve part 1 "
                     (\_ -> Expect.equal 930 (Day5.solve1 Day5Input.actual))
                 , test "Can solve part 2"
                     (\_ -> Expect.equal (Set.singleton 515) (Day5.solve2 Day5Input.actual))
                 ]



      suite7 : Test
      suite7 =
          describe "Day7"
              [ test "parse quantifier"
                  (\_ ->
                      Expect.equal
                          (Ok ( 1, "light red" ))
                          (Parser.run Day7.quantifier "1 light red bag")
                  )
              , test "parse quantifier with multiple bags"
                  (\_ ->
                      Expect.equal
                          (Ok ( 2, "light red" ))
                          (Parser.run Day7.quantifier "2 light red bags")
                  )
              , test "parse color"
                  (\_ ->
                      Expect.equal
                          (Ok "light red")
                          (Parser.run Day7.colorP "light red")
                  )
              , test "parse rfoo"
                  (\_ ->
                      Expect.equal
                          (Ok "foo|bar")
                          (Parser.run Day7.parseLine__ "foo bar x")
                  )
              , test "parse rule"
                  (\_ ->
                      Expect.equal
                          (Ok ( "light red", [ ( 1, "bright white" ), ( 2, "muted yellow" ) ] ))
                          (Parser.run Day7.parseLine_ "light red bags contain 1 bright white bag, 2 muted yellow bags.")
                  )
              , test "Part 1 with test input is as expected"
                  (\_ ->
                      Expect.equal
                          [ "bright white", "muted yellow", "light red", "dark orange" ]
                          (Day7.parentColors Day7.testRules "shiny gold")
                  )
              , test "Part 1 with actual input is as expected"
                  (\_ ->
                      Expect.equal
                          335
                          (Day7.solve1 Day7Input.actual)
                  )
              , test "Part 2 with test input"
                  (\_ ->
                      Expect.equal
                          32
                          Day7.solve2test
                  )
              , test "Part 2 with test second input"
                  (\_ ->
                      Expect.equal
                          126
                          Day7.solve2test2
                  )
              , test "Part 2 with actual input"
                  (\_ ->
                      Expect.equal
                          2431
                          Day7.solve2
                  )
              ]


   suite9 : Test
   suite9 =
       describe
           "Day 9"
           [ test "Finds right number for test with 5 preamble"
               (\_ ->
                   Expect.equal (Just 127) (Day9.solve 5 Day9.test)
               )
           , test "Finds right number for actual input"
               (\_ ->
                   Expect.equal (Just 257342611) (Day9.solve 25 Day9.actual)
               )
           , test "Finds right sequence for test"
               (\_ ->
                   Expect.equal (Just 62) (Day9.solve2 127 Day9.test)
               )
           , test "Finds right sequence for actual"
               (\_ ->
                   Expect.equal (Just 35602097) (Day9.solve2 257342611 Day9.actual)
               )
           ]
-}


suite7 : Test
suite7 =
    describe "Day7"
        [ test "parse quantifier"
            (\_ ->
                Expect.equal
                    (Ok ( 1, "light red" ))
                    (Parser.run Day7.quantifier "1 light red bag")
            )
        , test "parse quantifier with multiple bags"
            (\_ ->
                Expect.equal
                    (Ok ( 2, "light red" ))
                    (Parser.run Day7.quantifier "2 light red bags")
            )
        , test "parse color"
            (\_ ->
                Expect.equal
                    (Ok "light red")
                    (Parser.run Day7.colorP "light red")
            )
        , test "parse rfoo"
            (\_ ->
                Expect.equal
                    (Ok "foo|bar")
                    (Parser.run Day7.parseLine__ "foo bar x")
            )
        , test "parse rule"
            (\_ ->
                Expect.equal
                    (Ok ( "light red", [ ( 1, "bright white" ), ( 2, "muted yellow" ) ] ))
                    (Parser.run Day7.parseLine_ "light red bags contain 1 bright white bag, 2 muted yellow bags.")
            )
        , test "Part 1 with test input is as expected"
            (\_ ->
                Expect.equal
                    [ "bright white", "muted yellow", "light red", "dark orange" ]
                    (Day7.parentColors Day7.testRules "shiny gold")
            )
        ]


suite10optionals =
    describe
        "Day 10 finding optionals"
        [ test "no optional with single elem list"
            (\_ ->
                Expect.equal [] (Day10.optionalIndices [ 1 ])
            )
        , test "no optional"
            (\_ ->
                Expect.equal [] (Day10.optionalIndices [ 1, 4 ])
            )
        , test "simple optional"
            (\_ ->
                Expect.equal [ 0 ] (Day10.optionalIndices [ 1, 2 ])
            )
        , test "no optional 2 4"
            (\_ ->
                Expect.equal [] (Day10.optionalIndices [ 2, 4 ])
            )
        , test "no optional 3 4"
            (\_ ->
                Expect.equal [] (Day10.optionalIndices [ 3, 4 ])
            )
        , test " 1 and 2  optional in 1 2 3"
            (\_ ->
                Expect.equal [ 1, 0 ] (Day10.optionalIndices [ 1, 2, 3 ])
            )
        , test " 1 and 2  optional in 1 2 4"
            (\_ ->
                Expect.equal [ 1, 0 ] (Day10.optionalIndices [ 1, 2, 4 ])
            )
        , test "1 and 3  optional 1 3 4"
            (\_ ->
                Expect.equal [ 1, 0 ] (Day10.optionalIndices [ 1, 3, 4 ])
            )
        , test "1,2 and 3  optional 1 2 3 4"
            (\_ ->
                Expect.equal [ 2, 1, 0 ] (Day10.optionalIndices [ 1, 2, 3, 4 ])
            )
        ]


suite6 : Test
suite6 =
    describe
        "Day 6"
        [ test "test input"
            (\_ -> Expect.equal 11 <| Day6.solve1 Day6Input.test)
        , test "actualt input"
            (\_ -> Expect.equal 6297 <| Day6.solve1 Day6Input.actual)
        , test "test input part2 "
            (\_ -> Expect.equal 6 <| Day6.solve2 Day6Input.test)
        , test "actual input part2"
            (\_ -> Expect.equal 3158 <| Day6.solve2 Day6Input.actual)
        ]


expectOk : Result err value -> Expectation
expectOk res =
    case res of
        Err _ ->
            fail "Not Ok!"

        _ ->
            pass


expectOkWith : Result err value -> (value -> Expectation) -> Expectation
expectOkWith res expect =
    case res of
        Err _ ->
            fail "Not Ok!"

        Ok value ->
            expect value


suite8 : Test
suite8 =
    describe "Day8"
        [ test "Can solve part 1 with test data "
            (\_ -> Expect.equal 5 (Day8.solve1 Day8.test))
        , test "Can solve part 1 with actual data "
            (\_ -> Expect.equal 1744 (Day8.solve1 Day8.actual))
        , test "Can solve part 2 with test data "
            (\_ -> Expect.equal 8 (Day8.solve2 Day8.test2))
        , test "Can solve part 2 with actual data "
            (\_ -> Expect.equal 1174 (Day8.solve2 Day8.actual))
        ]


suite10 : Test
suite10 =
    describe
        "Day 10"
        [ test "Part 1 with test input"
            (\_ ->
                Expect.equal 35 (Day10.solve1 Day10.test)
            )
        , test "Part 1 with test input2"
            (\_ ->
                Expect.equal 220 (Day10.solve1 Day10.test2)
            )
        , test "Part 1 with actual input"
            (\_ ->
                Expect.equal 1914 (Day10.solve1 Day10.actual)
            )
        , test "Assert no duplicates"
            (\_ ->
                let
                    numbers =
                        Day10.parseNumbers Day10.actual
                in
                Expect.equal (length numbers) (length <| unique numbers)
            )
        , test "Part 2 with test minimal cases"
            (\_ ->
                Expect.equal
                    (map first Day10.testCases)
                    (map (\i -> Day10.solve3_ <| Day10.parseNumbersWords i) <| map second Day10.testCases)
            )
        ]



{-
   , test "Part 2 with test input 2"
       (\_ ->
           Expect.equal 19208 (Day10.solve3 Day10.test2)
       )
-}
