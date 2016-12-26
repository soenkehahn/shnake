module LevelsTest exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Position exposing (..)
import Level.Model exposing (..)
import Levels exposing (..)
import Stream exposing (..)
import Level.Solution exposing (..)


all : Test
all =
    describe "Levels"
        [ describe "simulatePlayer"
            [ test "can simulate a player to pass a simple level"
                (\() ->
                    let
                        level =
                            Level 3 (Position 1 1) [ Position 2 1 ] []

                        strategy =
                            [ Right ]
                    in
                        equal (simulatePlayer level strategy) Wins
                )
            , test "can simulate a simple failing player"
                (\() ->
                    let
                        level =
                            Level 3 (Position 1 1) [ Position 2 1 ] []

                        strategy =
                            [ Down, Left ]
                    in
                        equal (simulatePlayer level strategy) Looses
                )
            ]
        ]
