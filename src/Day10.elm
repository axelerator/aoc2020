module Day10 exposing (..)

import List exposing (drop, filter, foldl, head, length, map, maximum, member, minimum, reverse, sort, sum, take)
import List.Extra exposing (find, splitAt, takeWhile, unique, uniquePairs)
import Maybe.Extra exposing (values)
import Set exposing (Set)
import String exposing (lines, toInt, words)
import Tuple exposing (second)


solve1 : String -> Int
solve1 input =
    let
        numbers =
            sort <| parseNumbers input

        count i ( prev, ones, threes ) =
            if i == (prev + 1) then
                ( i, ones + 1, threes )

            else if i == (prev + 3) then
                ( i, ones, threes + 1 )

            else
                ( i, ones, threes )

        ( _, inc1, inc3 ) =
            foldl count ( 0, 0, 0 ) numbers
    in
    inc1 * (inc3 + 1)


parseNumbers =
    values << map toInt << lines


parseNumbersWords =
    values << map toInt << words


valid target numbers =
    let
        toTop =
            case head <| reverse numbers of
                Just top ->
                    top >= (target - 3)

                Nothing ->
                    False

        checkGap i ( lastI, consistent ) =
            ( i, consistent && (i <= lastI + 3) )

        ( _, continuous ) =
            foldl checkGap ( 0, True ) numbers
    in
    continuous && toTop


solve3 input =
    0


solve3_ numbers =
    let
        optionals =
            optionalIndices numbers

        target =
            case head <| reverse numbers of
                Just n ->
                    n + 3

                _ ->
                    -1
    in
    1
        + (traverse2
            target
            Set.empty
            optionals
           <|
            List.indexedMap Tuple.pair numbers
          )


numbersWithout : Set Int -> List ( Int, Int ) -> List Int
numbersWithout indices indexedNumbers =
    let
        unlessExcluded ( idx, num ) akku =
            if Set.member idx indices then
                akku

            else
                num :: akku
    in
    reverse <| foldl unlessExcluded [] indexedNumbers


traverse2 : Int -> Set Int -> List Int -> List ( Int, Int ) -> Int
traverse2 target alreadyMissing otherOptionals indexedNumbers =
    let
        _ =
            Debug.log "target" target

        _ =
            Debug.log "alreadyMissing" alreadyMissing

        _ =
            Debug.log "otherOptionals" otherOptionals

        mkCandidate (( o1, o2 ) as pair) =
            ( pair
            , numbersWithout
                (Set.union alreadyMissing <| Set.fromList [ o1, o2 ])
                indexedNumbers
            )

        pairs =
            uniquePairs otherOptionals

        candidates =
            map mkCandidate pairs

        f ( ( o1, o2 ), candidate ) =
            if valid target candidate then
                let
                    nextMissing =
                        Set.insert idx alreadyMissing

                    leftOptionals =
                        filter ((/=) idx) otherOptionals

                    _ =
                        Debug.log "valid" ( idx, candidate )
                in
                1 + traverse2 target nextMissing leftOptionals indexedNumbers

            else
                let
                    _ =
                        Debug.log "not valid" ( idx, candidate )
                in
                0
    in
    sum <| map f candidates


type alias Context =
    { head : List Int
    , tail : List Int
    , index : Int
    , optionals : List Int
    }


findOptionals : Int -> Int -> Context -> Context
findOptionals target i context =
    let
        head =
            context.head ++ [ i ]

        tail =
            drop 1 context.tail

        candidate =
            context.head ++ tail

        optionals =
            if valid target candidate then
                context.index :: context.optionals

            else
                context.optionals
    in
    { head = head
    , tail = tail
    , index = context.index + 1
    , optionals = optionals
    }


optionalIndices : List Int -> List Int
optionalIndices numbers =
    let
        target =
            case head <| reverse numbers of
                Just n ->
                    n + 3

                _ ->
                    -1

        initialContext =
            { head = []
            , tail = numbers
            , index = 0
            , optionals = []
            }

        finalContext =
            foldl (findOptionals target) initialContext numbers
    in
    finalContext.optionals



{-
   (0), 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, (22)
   (0), 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, (22)
   (0), 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, (22)
   (0), 1, 4, 5, 7, 10, 12, 15, 16, 19, (22)
   (0), 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, (22)
   (0), 1, 4, 6, 7, 10, 12, 15, 16, 19, (22)
   (0), 1, 4, 7, 10, 11, 12, 15, 16, 19, (22)
   (0), 1, 4, 7, 10, 12, 15, 16, 19, (22)


   (0) 1 2 3 - 4
   (0) 1 3   - 2
   (0) 1 2   - 2
   (0) 1     - 1

   (0) 2 3   - 2
   (0) 2     - 1

   (0) 3     - 1

-}


testCases =
    {-
       [ ( 1, "1" )
       , ( 1, "2" )
       , ( 1, "3" )
       , ( 2, "1 2" )
       , ( 2, "1 3" )
       , ( 2, "2 3" )
    -}
    [ ( 4, "1 2 3" )

    --
    --, ( 1, "1 4" )
    {-
       , ( 1, "2 4" )
       , ( 1, "2 4" )
       , ( 2, "2 3 4" )
       , ( 2, "1 3 4" )
       , ( 7, "1 2 3 4" )
    -}
    {-
       (0) 1 2 3 4 (7)
       (0)   2 3 4 (7)
       (0) 1   3 4 (7)
       (0) 1 2   4 (7)
       (0) 1     4 (7)
       (0)     3 4 (7)
       (0)   2   4 (7)
    -}
    ]


test =
    """16
10
15
5
1
11
7
19
6
12
4"""


test2 =
    """28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"""


actual =
    """26
97
31
7
2
10
46
38
112
54
30
93
18
111
29
75
139
23
132
85
78
99
8
113
87
57
133
41
104
98
58
90
13
91
20
68
103
127
105
114
138
126
67
32
145
115
16
141
1
73
45
119
51
40
35
150
118
53
80
79
65
135
74
47
128
64
17
4
84
83
147
142
146
9
125
94
140
131
134
92
66
122
19
86
50
52
108
100
71
61
44
39
3
72"""
