module Tests exposing (..)

import Test exposing (..)
import Debug exposing (log)
import Test.Html.Query exposing (..)
import Utils exposing (..)
import Test.Html.Selector exposing (..)
import Expect exposing (..)
import String
import Grid exposing (..)
import Array exposing (Array, get)
import App exposing (..)
import GridTests


all : Test
all =
    describe "shnake tests"
        [ GridTests.all
        , describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        Model (Position 0 0) []

                                    expected =
                                        Model newPosition []
                                in
                                    equal expected (fst <| update 3 (Just (ArrowMsg msg)) init)
                 in
                    [ testArrowMsg Up (Position 0 (0 - 1))
                    , testArrowMsg Down (Position 0 1)
                    , testArrowMsg Left (Position (0 - 1) 0)
                    , testArrowMsg Right (Position 1 0)
                    ]
                )
            , describe "NewFood"
                [ test "it remembers the food position"
                    (\() ->
                        let
                            model =
                                Model (Position 0 0) []

                            expected =
                                Model (Position 0 0) [ Position 23 42 ]
                        in
                            equal expected (fst <| update 10 (Just (NewFood (Position 23 42))) model)
                    )
                ]
            ]
        , describe "toGrid"
            [ test "renders the player as a red cell"
                (\() ->
                    let
                        model =
                            Model (Position 0 0) []

                        result =
                            get2 (Position 0 0) (log "grid" <| toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            ]
        ]
