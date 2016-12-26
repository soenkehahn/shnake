module LevelsTest exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Position exposing (..)
import Level.Model exposing (..)
import Levels exposing (..)
import Stream exposing (..)
import Level.Solution exposing (..)
import TestUtils exposing (..)


all : Bool -> Test
all runSlowTests =
    describe "Levels"
        ([ test "first level"
            (\() ->
                (Levels.all 0)
                    |> isJust
                        (\level ->
                            simulatePlayer level [ Left ] |> equal Wins
                        )
            )
         ]
            ++ if not runSlowTests then
                []
               else
                [ test "second level"
                    (\() ->
                        (Levels.all 1)
                            |> isJust
                                (\level ->
                                    simulatePlayer level [ Left, Left, Down ]
                                        |> equal Wins
                                )
                    )
                ]
        )
