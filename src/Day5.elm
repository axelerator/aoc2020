module Day5 exposing (..)

import Binary exposing (fromBooleans, toDecimal)
import List exposing (map, maximum, range)
import Maybe exposing (withDefault)
import Set exposing (Set, diff, fromList)
import String exposing (left, lines, right, toList)


solve2 =
    diff (fromList <| range 54 930) << (fromList << seats)


solve1 =
    withDefault 0 << maximum << seats


seats =
    map decodeSeat << lines


decodeBin high =
    toDecimal << fromBooleans << map ((==) high) << toList


decodeSeat s =
    decodeBin 'B' (left 7 s) * 8 + decodeBin 'R' (right 3 s)
