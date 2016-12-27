module Level.DefinitionsTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Position exposing (..)
import Level.Model exposing (..)
import Level.Definitions exposing (..)
import Level.Solution exposing (..)


all : Test
all =
    describe "Level.DefinitionsTests"
        [ describe "fitnessLevel"
            [ describe "correct penalty for solutions"
                (let
                    mkLevel food =
                        Level 3 (Position 0 0) food []
                 in
                    [ test "invalid solution"
                        (\() ->
                            fitnessLevel [] (mkLevel [ Position 1 0 ])
                                |> equal invalidSolutionWeight
                        )
                    , test "empty level"
                        (\() ->
                            fitnessLevel [ Right, Right ] (mkLevel [])
                                |> equal (shorterStrategyWeight * 2)
                        )
                    , test "strategy too long"
                        (\() ->
                            fitnessLevel [ Right, Right ] (mkLevel [ Position 1 0 ])
                                |> equal shorterStrategyWeight
                        )
                    , test "right strategy"
                        (\() ->
                            fitnessLevel [ Right, Right ] (mkLevel [ Position 2 0 ])
                                |> equal 0
                        )
                    , test "back-and-forth strategy"
                        (\() ->
                            fitnessLevel [ Down, Up, Right ] (mkLevel [ Position 1 0 ])
                                |> equal (shorterStrategyWeight * 2)
                        )
                    ]
                )
            ]
        ]
