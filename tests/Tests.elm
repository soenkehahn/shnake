module Tests exposing (..)

import Test exposing (..)
import Debug exposing (..)
import Random exposing (..)
import Fuzz
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
                                    equal expected (fst <| update 3 (ArrowMsg msg) init)
                 in
                    [ testArrowMsg Up (Position 0 (0 - 1))
                    , testArrowMsg Down (Position 0 1)
                    , testArrowMsg Left (Position (0 - 1) 0)
                    , testArrowMsg Right (Position 1 0)
                    ]
                )
            , describe "NewFood"
                [ fuzz (Fuzz.map initialSeed Fuzz.int)
                    "creates a random food item inside the grid"
                    (\seed ->
                        let
                            model =
                                Model (Position 0 0) []

                            result : List Position
                            result =
                                (fst <| update 3 (NewFood seed) model).food
                        in
                            Expect.all
                                [ List.length >> equal 1
                                , List.head
                                    >> isJust
                                        (Expect.all
                                            [ (.x >> atLeast -1)
                                            , (.x >> atMost 1)
                                            , (.y >> atLeast -1)
                                            , (.y >> atMost 1)
                                            ]
                                        )
                                ]
                                result
                    )
                ]
            , describe "eating"
                [ test "food gets removed when the player moves to the same cell"
                    (\() ->
                        let
                            model =
                                Model (Position 0 0) [ Position 0 1 ]

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.food []
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
                            get2 (Position 0 0) (toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            , test "renders food as a green cell"
                (\() ->
                    let
                        model =
                            Model (Position 0 0) [ Position 1 1 ]

                        result =
                            get2 (Position 1 1) (toGrid 3 model)

                        expected =
                            Just (Color "green")
                    in
                        equal result expected
                )
            ]
        ]


isJust : (a -> Expectation) -> Maybe a -> Expectation
isJust e m =
    case m of
        Nothing ->
            fail "expected: Just _"

        Just x ->
            e x
