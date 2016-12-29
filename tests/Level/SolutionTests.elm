module Level.SolutionTests exposing (..)

import Level.Solution exposing (..)
import Test exposing (..)
import Expect exposing (..)
import Stream exposing (..)
import Position exposing (..)
import Levels exposing (..)
import Level.Model exposing (..)
import TestUtils exposing (..)
import Level.Definitions exposing (..)


all : Bool -> Test
all runSlowTests =
    describe "Level.Solution"
        [ describe "findLevelByStrategy"
            ([ test "fills found levels with walls"
                (\() ->
                    -- fixme: walls
                    pass
                )
             ]
            )
        , describe "findShortestSolution"
            (if not runSlowTests then
                []
             else
                [ test "it finds a solution to a simple level"
                    (\() ->
                        let
                            level =
                                Level 3 (Position 0 0) [ Position 2 2 ] []

                            strategy =
                                findShortestSolution 10 level
                        in
                            strategy
                                |> isJust (simulatePlayer level >> equal Wins)
                    )
                , test "it returns the empty strategy when a level that is already solved"
                    (\() ->
                        let
                            level =
                                Level 3 (Position 0 0) [] []

                            strategy =
                                findShortestSolution 10 level
                        in
                            strategy |> equal (Just [])
                    )
                , describe "a more complex level"
                    (let
                        complexLevel =
                            Level 5
                                (Position 0 0)
                                [ Position 4 4 ]
                                [ Position 0 1
                                , Position 1 1
                                , Position 2 1
                                , Position 3 3
                                , Position 4 3
                                ]
                     in
                        [ test "it finds a solution to a more complex level"
                            (\() ->
                                let
                                    strategy =
                                        findShortestSolution 10 complexLevel
                                in
                                    strategy
                                        |> isJust (simulatePlayer complexLevel >> equal Wins)
                            )
                        , test "it aborts if it can't find a solution below the given length"
                            (\() ->
                                let
                                    strategy =
                                        findShortestSolution 3 complexLevel
                                in
                                    strategy |> equal Nothing
                            )
                        ]
                    )
                ]
            )
        , describe "allStrategies"
            [ test "it lists all strategies"
                (\() ->
                    let
                        result =
                            next 10 allStrategies

                        expected =
                            [ []
                            , [ Up ]
                            , [ Down ]
                            , [ Left ]
                            , [ Right ]
                            , [ Up, Up ]
                            , [ Down, Up ]
                            , [ Left, Up ]
                            , [ Right, Up ]
                            , [ Up, Down ]
                            ]
                    in
                        equal expected result
                )
            ]
        , describe "simulatePlayer"
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
        , describe "removeByPrefix"
            [ test "works"
                (\() ->
                    let
                        stream =
                            toStream
                                [ [ 1, 2, 3 ]
                                , [ 2, 3, 4 ]
                                , [ 3, 4, 5 ]
                                ]
                                []

                        filtered =
                            removeByPrefix [ 2, 3 ] stream
                    in
                        next 3 filtered
                            |> equal
                                [ [ 1, 2, 3 ]
                                , [ 3, 4, 5 ]
                                , []
                                ]
                )
            ]
        ]
