module Day4 exposing (..)

import List exposing (all, filter, head, length, map, member)
import Parser exposing ((|.), (|=), Parser, Step(..), chompWhile, getChompedString, keyword, loop, oneOf, run, succeed, symbol)
import String exposing (fromList, toInt, toList)


validPassportCount : String -> Int
validPassportCount input =
    case run passportCollectionParser input of
        Ok passports ->
            length <|
                filter validPassport passports

        Err _ ->
            0


validPassport : Passport -> Bool
validPassport pairs =
    let
        missingCid =
            not <|
                member Cid <|
                    map Tuple.first pairs
    in
    (length pairs == 8) || (length pairs == 7 && missingCid)


strictValidPassportCount : String -> Int
strictValidPassportCount input =
    case run passportCollectionParser input of
        Ok passports ->
            length <|
                filter validPassportStrict passports

        Err _ ->
            0


validPassportStrict : Passport -> Bool
validPassportStrict pairs =
    validPassport pairs && all pairValid pairs


pairValid ( key, value ) =
    let
        validator =
            head <| filter (\( k, _ ) -> k == key) validators
    in
    case validator of
        Nothing ->
            True

        Just ( _, validFunc ) ->
            validFunc value


validators =
    [ ( Byr, numberBetween 1920 2002 )
    , ( Iyr, numberBetween 2010 2020 )
    , ( Eyr, numberBetween 2020 2030 )
    , ( Hgt, hgtValidator )
    , ( Hcl, hclValidator )
    , ( Ecl, eclValidator )
    , ( Pid, pidValidator )
    ]


eclValidator col =
    member col [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


numberBetween : Int -> Int -> String -> Bool
numberBetween min max str =
    case toInt str of
        Just i ->
            i >= min && i <= max

        Nothing ->
            False


hgtValidator hgt =
    case String.toList hgt of
        x :: y :: 'i' :: 'n' :: _ ->
            numberBetween 59 76 <| String.fromList [ x, y ]

        x :: y :: z :: 'c' :: 'm' :: _ ->
            numberBetween 150 193 <| String.fromList [ x, y, z ]

        _ ->
            False


numbers =
    toList "1234567890"


isDigit c =
    member c numbers


hexChars =
    toList "abcdef1234567890"


isHex c =
    member c hexChars


pidValidator pid =
    String.length pid == 9 && (all isDigit <| toList pid)


hclValidator hcl =
    case toList hcl of
        '#' :: hex ->
            all isHex hex

        _ ->
            False


type Key
    = Byr
    | Iyr
    | Eyr
    | Hgt
    | Hcl
    | Ecl
    | Pid
    | Cid


keywords =
    [ ( Byr, "byr" )
    , ( Iyr, "iyr" )
    , ( Eyr, "eyr" )
    , ( Hgt, "hgt" )
    , ( Hcl, "hcl" )
    , ( Ecl, "ecl" )
    , ( Pid, "pid" )
    , ( Cid, "cid" )
    ]


whitespaceChars =
    [ ' ', '\n' ]


keywordParsers =
    map (\( key, str ) -> succeed key |. keyword str) keywords


type alias Pair =
    ( Key, String )


type alias Passport =
    List Pair


whitespace : Parser ()
whitespace =
    chompWhile (\c -> List.member c whitespaceChars)


mkPair : Key -> String -> Pair
mkPair key value =
    ( key, value )


keyParser : Parser Key
keyParser =
    oneOf keywordParsers


valueParser : Parser String
valueParser =
    getChompedString <|
        succeed ()
            |. chompWhile
                (\c -> not <| List.member c whitespaceChars)


pairParser : Parser Pair
pairParser =
    succeed mkPair
        |= keyParser
        |. symbol ":"
        |= valueParser


passportParser : Parser (List Pair)
passportParser =
    loop [] passportHelp


passportHelp : List Pair -> Parser (Step (List Pair) (List Pair))
passportHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= pairParser
            |. oneOf
                [ symbol " ", symbol "\n" ]
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]


passportCollectionParser : Parser (List Passport)
passportCollectionParser =
    loop [] passportCollectionParserHelp


passportCollectionParserHelp revStmts =
    oneOf
        [ succeed (\stmt -> Loop (stmt :: revStmts))
            |= passportParser
            |. symbol "\n"
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revStmts))
        ]
