module Day11 exposing (..)

import Array exposing (Array)
import Helper exposing (..)
import List exposing (concat, drop, filter, foldl, head, indexedMap, length, map, maximum, member, minimum, range, reverse, sort, sum, take)
import List.Extra exposing (find, splitAt, takeWhile, unique, uniquePairs)
import Maybe.Extra exposing (values)
import Set exposing (Set)
import Tuple exposing (first, second)


solve1 input =
    solve1_ <| parse input


solve1_ : Area -> Int
solve1_ prev =
    let
        next =
            step prev
    in
    if next == prev then
        occupiedTotal prev

    else
        solve1_ next


solve2 input =
    solve2_ <| parse input


solve2_ : Area -> Int
solve2_ prev =
    let
        next =
            step2 prev
    in
    if next == prev then
        occupiedTotal prev

    else
        solve2_ next


occupiedTotal : Area -> Int
occupiedTotal area =
    let
        isOcc f =
            case f of
                Seat Taken ->
                    True

                _ ->
                    False

        countOccupied fields =
            Array.length <| Array.filter isOcc fields
    in
    arraySum <| Array.map countOccupied area.rows


type Field
    = Floor
    | Seat State


type State
    = Free
    | Taken


type alias Row =
    Array Field


type alias Area =
    { rows : Array Row
    , width : Int
    , height : Int
    }


parse : String -> Area
parse input =
    let
        mkSeat c =
            case c of
                'L' ->
                    Seat Free

                '#' ->
                    Seat Taken

                _ ->
                    Floor

        mkRow line =
            Array.fromList <| map mkSeat <| String.toList line

        rows =
            Array.fromList <| map mkRow <| String.lines input

        width =
            case Array.get 0 rows of
                Just row ->
                    Array.length row

                _ ->
                    0

        height =
            Array.length rows
    in
    { rows = rows
    , height = height
    , width = width
    }


toString : Area -> String
toString area =
    let
        toChar f =
            case f of
                Seat Taken ->
                    '#'

                Seat Free ->
                    'L'

                Floor ->
                    '.'

        mkLine row =
            String.fromList <| map toChar <| Array.toList row

        lines =
            Array.map mkLine area.rows
    in
    String.join "\n" <| Array.toList lines


step : Area -> Area
step area =
    { area
        | rows = Array.indexedMap (stepRow area) area.rows
    }


step2 : Area -> Area
step2 area =
    { area | rows = Array.indexedMap (stepRow2 area) area.rows }


type alias Position =
    ( Int, Int )


directions =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 0, 1 )
    , ( -1, 1 )
    , ( -1, 0 )
    ]


radiusSteps =
    range 1 15


directionSteps =
    let
        mkSteps direction =
            map (\distance -> mult distance direction) radiusSteps
    in
    map mkSteps directions


mult : Int -> ( Int, Int ) -> ( Int, Int )
mult i ( x, y ) =
    ( i * x, i * y )


arraySum =
    Array.foldl (+) 0


nextSeat : Area -> Position -> List ( Int, Int ) -> Maybe State
nextSeat area ( x, y ) directionsSteps =
    let
        f ( dx, dy ) lastRes =
            case lastRes of
                Nothing ->
                    case fieldAt area ( x + dx, y + dy ) of
                        Floor ->
                            lastRes

                        Seat state ->
                            Just state

                _ ->
                    lastRes
    in
    foldl f Nothing directionsSteps


nextSeat_ : Area -> Position -> Int -> ( Int, Int ) -> Maybe State
nextSeat_ area (( x, y ) as pos) distance (( dx, dy ) as direction) =
    let
        (( x_, y_ ) as nextPos) =
            ( x + (dx * distance), y + (dy * distance) )

        nextField =
            fieldAt area nextPos
    in
    if x_ < 0 || x_ > area.width || y_ < 0 || y_ > area.height then
        Nothing

    else
        case nextField of
            Floor ->
                nextSeat_ area pos (distance + 1) direction

            Seat state ->
                Just state


seatsInSight : Area -> Position -> ( Int, Int )
seatsInSight area position =
    let
        states =
            values <| map (nextSeat_ area position 1) directions

        free =
            length <| filter ((==) Free) states

        taken =
            length <| filter ((==) Taken) states
    in
    ( free, taken )


fieldAt : Area -> Position -> Field
fieldAt area ( x, y ) =
    case Array.get y area.rows of
        Nothing ->
            Floor

        Just fields ->
            case Array.get x fields of
                Just field ->
                    field

                Nothing ->
                    Floor


adjacentIds : Int -> Position -> List Position
adjacentIds distance (( x, y ) as pos) =
    let
        x0 =
            x - distance

        x1 =
            x + distance

        y0 =
            y - distance

        y1 =
            y + distance

        rangeX =
            range x0 x1

        rangeY =
            range y0 y1

        mkPos =
            Tuple.pair

        mkX x_ =
            map (mkPos x_) rangeY

        self p =
            p /= pos
    in
    filter self <| concat <| map mkX rangeX


occupiedCount : Area -> List Position -> Int
occupiedCount area positions =
    length <| filter (isOccupied area) positions


isOccupied : Area -> Position -> Bool
isOccupied area ( x, y ) =
    let
        row =
            Array.get y area.rows
    in
    case row of
        Nothing ->
            False

        Just fields ->
            case Array.get x fields of
                Just (Seat Taken) ->
                    True

                _ ->
                    False


stepRow : Area -> Int -> Row -> Row
stepRow area y fields =
    let
        f x field =
            let
                pos =
                    ( x, y )
            in
            case field of
                Floor ->
                    Floor

                Seat Free ->
                    if (occupiedCount area <| adjacentIds 1 pos) == 0 then
                        Seat Taken

                    else
                        Seat Free

                Seat Taken ->
                    if (occupiedCount area <| adjacentIds 1 pos) >= 4 then
                        Seat Free

                    else
                        Seat Taken
    in
    Array.indexedMap f fields


stepRow2 : Area -> Int -> Row -> Row
stepRow2 area y fields =
    let
        f x field =
            let
                pos =
                    ( x, y )
            in
            case field of
                Floor ->
                    Floor

                Seat Free ->
                    let
                        ( _, taken ) =
                            seatsInSight area pos
                    in
                    if taken == 0 then
                        Seat Taken

                    else
                        Seat Free

                Seat Taken ->
                    let
                        ( _, taken ) =
                            seatsInSight area pos
                    in
                    if taken >= 5 then
                        Seat Free

                    else
                        Seat Taken
    in
    Array.indexedMap f fields


test =
    """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"""


after1Step =
    """#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"""



{-

   #.LL.LL.LL

   #.LL.L#.##
-}


after2Step =
    """#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##"""


after5Step =
    """#.#L.L#.##
#LLL#LL.L#
L.#.L..#..
#L##.##.L#
#.#L.LL.LL
#.#L#L#.##
..L.L.....
#L#L##L#L#
#.LLLLLL.L
#.#L#L#.##"""


t2_2 =
    """#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#"""



{-
   #.##.##.##
-}


t2_3 =
    """#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#"""


t2_last =
    """#.L#.L#.L#
#LLLLLL.LL
L.L.L..#..
##L#.#L.L#
L.L#.LL.L#
#.LLLL#.LL
..#.L.....
LLL###LLL#
#.LLLLL#.L
#.L#LL#.L#"""


actual =
    """LL.LL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLL.LLLL.LLLLLLLL
.LLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LL.LLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLL.LLLLL.LLLLLLLL
.LLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLL..LLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL
L.L.....LL....LLLL....L.L.L...L..L..L.LL.LL.LL.L......L.L..L...L..L.L....LL.......LLL.LL.L.
LLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLL.
LLLLLLLL.LLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLLL
LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
.L........LL.L.....L.L..LLLL.LL.L........L...L.L.L......L.........L..LLLLL.......L...LL....
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLL..LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.
LLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LL.LLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLL.L.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLLLL.LLLLLL.LLLLLLLL.LLLLLLL.L.LLLLLLL.LLLLLLLL
LLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLLLLL.LLLLLLLLL...LLLLLL
LLLLL.LLLLLL..LLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLL.LLLLLLLLL.LLLL.LLL
.....L.......LL..LL....LLL.LL.LL.L.......LL.L....L.L.L.L.L.L..L......L........L...L..L...L.
LLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.L.LLLLLLL.LLL.LLLL
LLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLL.LLL.LLLLLLLLLLL.LLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLL.LL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL
...L.L...L.L....LL.L.........L.....LLL....L..L..L..L.LLL....L.LL.L.L..L.L..L..LL..L..L..LL.
LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLL.LL.LLLLL
LLLLL.LLLLLLLLLLLLL..LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLL.L..LLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLL..LLLLLLLLLLLLLLLLL
LLLLL.L.LLLL.LLLLLL.LLLLL.LLL.LLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLL.L.LLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
L.LL.L....L....LL.L...LL..........L.......L.LL...LL.LLLLL.....LLL..L.L..L......L..LL..L...L
LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL
L.LLL.LLLLLL.LLLLLL.LLLLL.LLL.LLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLL.LL.LLLLLLLL
L.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LL.LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL
..L.LLL.L.L.....L...LL....L......L.L.LL..L..L.L..L....LL........LLL.L.LL.......L...L...L..L
LLLLLLL.LLLL.LL.LLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLL.LL.LLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.L.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLL
.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLL
LLLLL.LLLLLL..LL..LLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL..LLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LL.LLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLL.L.LLLL.LLL
.......L.......L.LL...L..L....L.L...L..LL.LLLL.L.L....LLL.L.LL.LL.........LL.L..L...L...L.L
LLLL.LLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.L.LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LL.LLL.LLLLLL.LLLLL.L.LLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
..L...L...LLLLLLLL..L.LL...LL.L.....L.L.....L.....L...L....L.L..........L.L.....L..L..L....
LLLLL.LLLLLL.LLLLLLLLLLLL.L.LLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLL.
LLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LL.LLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLLLL.LLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL..LLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LL.LLLL.LLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLL.L.LLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
.LL..L.......L...L.L.L......L....L.....L......L........L.L...LLLL...L.L...LLLL.......L.L...
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLL.LL.LLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL..LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLLLL.LL.L.LLLLLLLL.LLLLLLL..LLLLLLLLLLLLLLLLL.LLLLLLLL
LL.LL.LLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLL.L.LLLLL.LL.LLLLLLL.LLLLLLLLL.LLLLLLL.
LLLLL.LLLLLL.LLLLLL..LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LL.LLLLLL.LLLLLLLL
LLLLL..LLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.L.LLLLLLLLL..LL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLL.LL
L...L.L..L..L..L...LLL........LLLL.LL..LL.......LLLLL.L.L..L.L.L...L.L.....LL.....LLLL..L..
LLLLL.LLL..LLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLL.LL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL..LL.LL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLL.LLLL.LL.LLLLLLLL
LLLLL.LLLL.L.LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLL.LLLLL.L.LLLLLL.LLLLLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL
LL.LL.LLLLLL.LL.LLL.LLLLLLLLLLL.L.LLLLLLL.LLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLL.LL.LLL.LLLLLL.LLLLL.LLLLLLL.L.LLLLLLLLLL.LLLLLLLL.L.LLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLL.LLL.LLLLLLL.LLLLLLLLLLLL.LLLLL
....L.L.L......LLL....L..L.L...........L.L......LLL.LL.LL.L.LL.....L.LLLL......L...L.LL.LL.
LLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLL.LL.LLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLLL.LLLLLLLL
LLLLL.L.LLLLLLLLLLLLLLLLLLLLLLL.L.LLLLLLLLLLLL.LLLLLLLL.LLLLL.LL.LLLLLLL.LL.LLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLL.L.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLL.LLLLLLLLLLLLL.LLLLLLLLLL.LL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLL
LLLLL.LLLLLL.L.LLLL.LLLLLLLL.LLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.L
....L........L.....LLL....L......LL.L...LLLL.LLL.L.LL.......L.L.L.L......LL..L.LLLL....LL.L
LLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLL..LLLLLLLL.LLLLLLLLLLL
LLLLL.LLL.LL.LLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLL.LLL.LL..LLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLL
LLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL
LLLLL.LLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLLLL.L.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLL.LLLLLLLL"""
