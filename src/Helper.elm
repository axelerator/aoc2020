module Helper exposing (..)

import List exposing (map)
import Maybe.Extra exposing (values)
import String exposing (lines, toInt, words)


parseNumbers =
    values << map toInt << lines


parseNumbersWords =
    values << map toInt << words
