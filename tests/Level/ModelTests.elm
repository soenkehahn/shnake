module Level.ModelTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Level.Model exposing (..)
import Position exposing (..)


all : Test
all =
    describe "Level.ModelTests"
        [ describe "normalize"
            [ test "it normalizes food"
                (\() ->
                    let
                        food =
                            Position 3 3

                        level =
                            Level 9 (Position 0 0) [ food, food ] []
                    in
                        (normalize level).food |> equal [ food ]
                )
            , test "it normalizes walls"
                (\() ->
                    let
                        wall =
                            Position 3 3

                        level =
                            Level 9 (Position 0 0) [] [ wall, wall ]
                    in
                        (normalize level).walls |> equal [ wall ]
                )
            ]
        ]
