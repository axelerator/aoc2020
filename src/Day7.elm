module Day7 exposing (..)

import Day7Input
import List exposing (concat, drop, filter, foldr, head, length, map, member, sum)
import List.Extra exposing (unique)
import Maybe.Extra exposing (values)
import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, chompWhile, getChompedString, int, keyword, loop, oneOf, run, succeed, symbol)
import String exposing (split, words)
import Tuple exposing (first, second)


type alias Color =
    String


type alias Quantifier =
    ( Int, String )


type alias Rule =
    ( Color, List Quantifier )


testRules =
    parseRules Day7Input.test


actualRules =
    parseRules Day7Input.actual


solve1 input =
    length <| parentColors actualRules "shiny gold"


solve2test =
    traverse testRules "shiny gold" - 1


solve2test2 =
    traverse (parseRules Day7Input.test2) "shiny gold" - 1


solve2 =
    traverse (parseRules Day7Input.actual) "shiny gold" - 1


findRule : List Rule -> Color -> Maybe Rule
findRule rules col =
    let
        predicate ( otherRuleColor, _ ) =
            otherRuleColor == col
    in
    head <| filter predicate rules


traverse : List Rule -> Color -> Int
traverse rules color =
    let
        match =
            findRule rules color

        next ( count, nextC ) =
            count * traverse rules nextC
    in
    case match of
        Just ( _, quantities ) ->
            1 + (sum <| map next quantities)

        Nothing ->
            0


parseRules input =
    let
        lines =
            String.lines input

        resultToMaybe r =
            case r of
                Ok x ->
                    Just x

                Err m ->
                    Nothing
    in
    Maybe.Extra.values <| map resultToMaybe <| map (\line -> run parseLine_ line) lines


mkRule : String -> String -> List Quantifier -> Rule
mkRule modifier color quantifiers =
    ( modifier ++ " " ++ color, quantifiers )


word : Parser String
word =
    getChompedString <|
        succeed ()
            |. chompWhile
                (\c -> c /= ' ')



-- light red bags contain 1 bright white bag, 2 muted yellow bags.


parseLine__ : Parser String
parseLine__ =
    let
        mkFoo a b =
            a ++ "|" ++ b
    in
    succeed mkFoo
        |= word
        |. symbol " "
        |= word
        |. symbol " x"


parseLine_ : Parser Rule
parseLine_ =
    succeed mkRule
        |= word
        |. symbol " "
        |= word
        |. symbol " bags contain "
        |= quantifierList


mkQuantifier : Int -> String -> String -> Quantifier
mkQuantifier count modifier color =
    ( count, modifier ++ " " ++ color )


colorP : Parser Color
colorP =
    let
        mkCol mod c =
            mod ++ " " ++ c
    in
    succeed mkCol
        |= word
        |. symbol " "
        |= word


quantifier : Parser Quantifier
quantifier =
    succeed mkQuantifier
        |= int
        |. symbol " "
        |= word
        |. symbol " "
        |= word
        |. oneOf
            [ symbol " bags", symbol " bag" ]


quantifierList : Parser (List Quantifier)
quantifierList =
    loop [] quantifiersHelp


quantifiersHelp : List Quantifier -> Parser (Step (List Quantifier) (List Quantifier))
quantifiersHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= quantifier
            |. oneOf
                [ symbol ", ", symbol "." ]
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]


parseLine : String -> Maybe Rule
parseLine line =
    let
        ruleParts =
            split " bags contain " line
    in
    case ruleParts of
        left :: right :: _ ->
            Just ( left, [] )

        _ ->
            Nothing


parentColors : List Rule -> Color -> List Color
parentColors rules color =
    let
        canContain ( parentColor, includes ) =
            member color <| map second includes

        directParents =
            map first <| filter canContain rules
    in
    unique <| directParents ++ (concat <| map (parentColors rules) directParents)
