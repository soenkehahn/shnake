module GridTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Position exposing (..)
import Grid exposing (..)
import Array exposing (toList)
import Fuzz


all : Test
all =
    describe "Grid"
        [ test "places (0, 0) in the middle"
            (\() ->
                let
                    result =
                        create 3 0
                            |> setCell { x = 0, y = 0 } 1
                            |> toLists

                    expected =
                        [ [ 0, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 0 ] ]
                in
                    equal expected result
            )
        , let
            coordinate =
                Fuzz.intRange -10 10

            pos =
                Fuzz.map2 Position coordinate coordinate
          in
            fuzz pos
                "get and setCell commute"
                (\position ->
                    equal (Just True)
                        (get position (setCell position True (create 21 False)))
                )
        ]
