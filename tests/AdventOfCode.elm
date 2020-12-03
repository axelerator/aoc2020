module AdventOfCode exposing (..)

import Day1
import Day1Input
import Day2
import Day2Input
import Day3
import Day3Input
import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


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
