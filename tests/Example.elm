module Example exposing (..)

import Advent
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


test1Input =
    """
1721
979
366
299
675
1456
"""


suite : Test
suite =
    test "Day one test input calculates correctly"
        (\_ -> Expect.equal 514579 (Advent.accounting test1Input))
