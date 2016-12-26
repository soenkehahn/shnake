module Level.SolutionTests exposing (..)

import Level.Solution exposing (..)
import Test exposing (..)
import Expect exposing (..)
import Stream exposing (..)
import Position exposing (..)
import Levels exposing (..)
import Level.Model exposing (..)


all : Bool -> Test
all runSlowTests =
    describe "Level.Solution"
        [ describe "findShortestSolution"
            (if not runSlowTests then
                []
             else
                [ test "it finds a solution to a simple level"
                    (\() ->
                        let
                            level =
                                Level 3 (Position 0 0) [ Position 2 2 ] []

                            strategy =
                                findShortestSolution level
                        in
                            equal (simulatePlayer level strategy) Wins
                    )
                , test "it finds a solution to a more complex level"
                    (\() ->
                        let
                            level =
                                Level 5
                                    (Position 0 0)
                                    [ Position 4 4 ]
                                    [ Position 0 1
                                    , Position 1 1
                                    , Position 2 1
                                    , Position 3 3
                                    , Position 4 3
                                    ]

                            strategy =
                                findShortestSolution level
                        in
                            equal (simulatePlayer level strategy) Wins
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
