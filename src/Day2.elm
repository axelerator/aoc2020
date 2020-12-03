module Day2 exposing (..)

import List exposing (head)


type alias Range =
    ( Int, Int )


type alias Entry =
    { count : Range
    , char : Char
    , password : String
    }


mkEntry : String -> Maybe Entry
mkEntry line =
    let
        parts =
            String.words line
    in
    case parts of
        countStr :: charWithColon :: password :: _ ->
            let
                char =
                    case head <| String.toList charWithColon of
                        Just aChar ->
                            aChar

                        Nothing ->
                            '~'

                count =
                    case String.split "-" countStr of
                        min :: max :: [] ->
                            case ( String.toInt min, String.toInt max ) of
                                ( Just minI, Just maxI ) ->
                                    ( minI, maxI )

                                _ ->
                                    ( -1, -1 )

                        _ ->
                            ( -1, -1 )
            in
            Just
                { count = count
                , char = char
                , password = password
                }

        _ ->
            Nothing


valid : Maybe Entry -> Bool
valid mbEntry =
    case mbEntry of
        Nothing ->
            False

        Just entry ->
            let
                isChar c =
                    c == entry.char

                actualCount =
                    List.length <|
                        List.filter isChar (String.toList entry.password)

                ( min, max ) =
                    entry.count
            in
            actualCount >= min && actualCount <= max


valid2 : Maybe Entry -> Bool
valid2 mbEntry =
    case mbEntry of
        Nothing ->
            False

        Just entry ->
            let
                isChar c =
                    c == entry.char

                actualCount =
                    List.length <|
                        List.filter isChar (String.toList entry.password)

                ( i0, i1 ) =
                    entry.count

                tail1 =
                    List.drop (i0 - 1) <| String.toList entry.password

                ( c0, tail2 ) =
                    case tail1 of
                        c :: cs ->
                            ( c, cs )

                        _ ->
                            ( '~', [] )

                c1 =
                    case List.drop (i1 - i0 - 1) tail2 of
                        c :: _ ->
                            c

                        _ ->
                            '~'
            in
            ((c1 == entry.char) && (c0 /= entry.char))
                || ((c1 /= entry.char) && (c0 == entry.char))


allValid : String -> Int
allValid input =
    let
        ls =
            String.lines input

        entries =
            List.map mkEntry ls
    in
    List.length <| List.filter valid entries


allValid2 : String -> Int
allValid2 input =
    let
        ls =
            String.lines input

        entries =
            List.map mkEntry ls
    in
    List.length <| List.filter valid2 entries
