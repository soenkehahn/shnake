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
                        newGrid 3 0
                            |> setCell { x = 1, y = 1 } 1
                            |> toLists

                    expected =
                        [ [ 0, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 0 ] ]
                in
                    equal expected result
            )
        , let
            coordinate =
                Fuzz.intRange 0 20

            pos =
                Fuzz.map2 Position coordinate coordinate
          in
            fuzz pos
                "get and setCell commute"
                (\position ->
                    equal (Just True)
                        (get position (setCell position True (newGrid 21 False)))
                )
        , describe "inGrid"
            [ fuzz (Fuzz.tuple3 ( Fuzz.int, Fuzz.int, Fuzz.intRange 1 100 ))
                "works"
                (\( x, y, size ) ->
                    let
                        canGet position grid =
                            case get position grid of
                                Nothing ->
                                    False

                                Just _ ->
                                    True

                        position =
                            Position x y

                        grid =
                            newGrid size ()
                    in
                        (inGrid size position)
                            |> equal (canGet position grid)
                )
            ]
        ]
