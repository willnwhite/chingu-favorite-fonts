module FontRequestsTests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "The FontRequests module"
        [ describe "FontRequests.update"
            [ test "filters out values from one list that are present in another" <|
                \_ ->
                    Expect.equal
                        [ 3, 4 ]
                        (ListExcept.except [ 1, 2 ] [ 1, 2, 3, 4 ])
            ]
        ]
