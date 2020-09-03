module ListExceptTests exposing (..)

-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect exposing (Expectation)
import ListExcept
import Test exposing (..)


suite : Test
suite =
    describe "The ListExcept module"
        [ describe "ListExcept.except"
            [ test "filters out values from one list that are present in another" <|
                \_ ->
                    -- > ListExcept.except [1,2] [1,2,3,4]
                    -- [3,4] : List number
                    Expect.equal
                        [ 3, 4 ]
                        (ListExcept.except [ 1, 2 ] [ 1, 2, 3, 4 ])
            ]
        ]
