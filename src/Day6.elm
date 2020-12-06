module Day6 exposing (..)

import Set


solve1 : String -> Int
solve1 input =
    let
        groupToSet lines =
            List.foldr (\line set -> Set.union set <| Set.fromList <| String.toList line) Set.empty lines

        sets =
            List.map groupToSet <| groups input
    in
    List.sum <| List.map Set.size sets


all =
    Set.fromList <| String.toList "qwertzuiopasdfghjklyxcvbnm"


solve2 : String -> Int
solve2 input =
    let
        groupToSet lines =
            List.foldr (\line set -> Set.intersect set <| Set.fromList <| String.toList line) all lines

        sets =
            List.map groupToSet <| groups input
    in
    List.sum <| List.map Set.size sets


groups : String -> List (List String)
groups input =
    let
        f line ( open, closed ) =
            if line == "" then
                ( [], open :: closed )

            else
                ( line :: open, closed )

        ( lastGroup, otherGroups ) =
            List.foldl f ( [], [] ) <| String.lines input
    in
    lastGroup :: otherGroups
