module Day3 exposing (..)

import List exposing (drop, head, isEmpty, length, map, product)
import Maybe.Extra exposing (values)
import Result exposing (andThen)
import Result.Extra


solve : String -> List Slope -> Result Error Int
solve input slopes =
    case parsePattern input of
        Ok pattern ->
            Ok <| product <| map (step pattern ( 0, ( 0, 0 ) )) slopes

        Err e ->
            Err e


type alias Pattern =
    { size : Size
    , rows : List PatternRow
    }


type alias PatternRow =
    List Spot


type Spot
    = Tree
    | Space


type alias Vector =
    ( Int, Int )


type alias Size =
    Vector


type alias Slope =
    Vector


type alias Position =
    Vector


type alias Step =
    ( Int, Position )


type Error
    = InvalidCharacter String
    | InvalidLineLength String Int
    | EmptyPattern
    | Errors (List Error)


parseLine : Int -> String -> Result Error PatternRow
parseLine expectedLength line =
    let
        transform c =
            case c of
                '.' ->
                    Just Space

                '#' ->
                    Just Tree

                _ ->
                    Nothing

        spots =
            values <| map transform <| String.toList line
    in
    if String.length line /= expectedLength then
        Err <| InvalidLineLength line expectedLength

    else if length spots /= expectedLength then
        Err <| InvalidCharacter line

    else
        Ok spots


parseLines : ( Int, List String ) -> Result Error (List PatternRow)
parseLines ( expectedLength, lines ) =
    Result.Extra.combine <| map (parseLine expectedLength) lines


mkPatternFromLines : List PatternRow -> Result Error Pattern
mkPatternFromLines rows =
    if isEmpty rows then
        Err EmptyPattern

    else
        Ok
            { rows = rows
            , size =
                ( Maybe.withDefault 0 <|
                    Maybe.map length <|
                        head rows
                , length rows
                )
            }


noEmptyPattern : String -> Result Error ( Int, List String )
noEmptyPattern input =
    case String.lines input of
        [] ->
            Err EmptyPattern

        "" :: _ ->
            Err EmptyPattern

        (firstLine :: _) as lines ->
            Ok ( String.length firstLine, lines )


parsePattern : String -> Result Error Pattern
parsePattern input =
    noEmptyPattern input
        |> andThen parseLines
        |> andThen mkPatternFromLines


step : Pattern -> Step -> Slope -> Int
step pattern ( sum, position ) slope =
    let
        ( x, y ) =
            position

        ( _, height ) =
            pattern.size

        nextPos =
            add position slope

        currentSpot =
            spotAt pattern position

        newSum =
            case currentSpot of
                Tree ->
                    sum + 1

                Space ->
                    sum
    in
    if y >= height then
        newSum

    else
        step pattern ( newSum, nextPos ) slope


spotAt : Pattern -> Position -> Spot
spotAt pattern pos =
    let
        ( x, y ) =
            normalize pattern pos

        row =
            head <| drop y pattern.rows
    in
    case row of
        Nothing ->
            Space

        Just spots ->
            case head <| drop x spots of
                Nothing ->
                    Space

                Just spot ->
                    spot


add : Vector -> Vector -> Vector
add ( x0, y0 ) ( x1, y1 ) =
    ( x0 + x1, y0 + y1 )


normalize : Pattern -> Vector -> Vector
normalize pattern ( x, y ) =
    let
        ( width, _ ) =
            pattern.size
    in
    ( modBy width x, y )
