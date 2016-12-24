module GridTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Grid exposing (..)
import Array exposing (toList)


all : Test
all =
    describe "Grid"
        [ test "places (0, 0) in the middle"
            (\() ->
                let
                    result =
                        create 3 0
                            |> set2 { x = 0, y = 0 } 1
                            |> toLists

                    expected =
                        [ [ 0, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 0 ] ]
                in
                    equal expected result
            )
        ]
