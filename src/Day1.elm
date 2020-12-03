module Day1 exposing (..)

import List exposing (map)
import List.Extra exposing (find, uniquePairs)
import Maybe.Extra exposing (values)


accounting : String -> Int
accounting input =
    let
        ls =
            String.lines input

        numbers =
            values <| map String.toInt ls

        pairs =
            uniquePairs numbers

        sum2020 ( x, y ) =
            x + y == 2020

        pair =
            find sum2020 pairs
    in
    case pair of
        Just ( x, y ) ->
            x * y

        Nothing ->
            0


accounting3 : String -> Int
accounting3 input =
    let
        ls =
            String.lines input

        numbers =
            values <| map String.toInt ls

        triples =
            uniqueTriples numbers

        sum2020 ( x, y, z ) =
            x + y + z == 2020

        pair =
            find sum2020 triples
    in
    case pair of
        Just ( x, y, z ) ->
            x * y * z

        Nothing ->
            0


uniqueTriples : List a -> List ( a, a, a )
uniqueTriples xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            let
                pairsWithCurrent =
                    uniquePairs xs_

                triplesWithCurrent =
                    List.map (\( y, z ) -> ( x, y, z )) pairsWithCurrent
            in
            triplesWithCurrent ++ uniqueTriples xs_
