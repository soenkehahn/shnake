module LocalSearchTests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import Debug exposing (..)
import LocalSearch exposing (..)
import Random exposing (..)


all : Test
all =
    describe "LocalSearch"
        [ describe "search"
            (let
                mutate : Mutate ( Int, Int, Int )
                mutate ( a, b, c ) =
                    map2
                        (\which diff ->
                            case which of
                                1 ->
                                    ( a + diff, b, c )

                                2 ->
                                    ( a, b + diff, c )

                                3 ->
                                    ( a, b, c + diff )

                                _ ->
                                    crash "search test: shouldn't happen"
                        )
                        (int 1 3)
                        (int -1 1)

                fitness ( a, b, c ) =
                    [ abs (a - 23)
                    , abs (b - 42)
                    ]

                getOptimized ( a, b, c ) =
                    ( a, b )

                searchLimit =
                    10000
             in
                [ test "finds an Int tuple"
                    (\() ->
                        search searchLimit mutate fitness ( 0, 0, 0 )
                            |> getOptimized
                            |> equal ( 23, 42 )
                    )
                , test "mutates state that doesn't affect the fitness"
                    (\() ->
                        let
                            getUnoptimized ( a, b, c ) =
                                c
                        in
                            search searchLimit mutate fitness ( 0, 0, 0 )
                                |> getUnoptimized
                                |> notEqual 0
                    )
                , test "stops searching after a given number of tries"
                    (\() ->
                        let
                            approximateFitness x =
                                fitness x ++ [ 1 ]
                        in
                            search searchLimit mutate approximateFitness ( 0, 0, 0 )
                                |> getOptimized
                                |> equal ( 23, 42 )
                    )
                , test "doesn't optimize if fitness is 0"
                    (\() ->
                        search searchLimit mutate (\_ -> [ 0 ]) ( 0, 0, 0 )
                            |> equal ( 0, 0, 0 )
                    )
                ]
            )
        , describe "isPerfect"
            [ test "returns True if it contains only zeros"
                (\() ->
                    isPerfect [ 0, 0 ] |> equal True
                )
            , test "returns False if it contains non-zeros"
                (\() ->
                    isPerfect [ 0, 1 ] |> equal False
                )
            ]
        ]
