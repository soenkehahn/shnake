module Tests exposing (..)

-- fixme: rename to AppTests

import Test exposing (..)
import Position exposing (..)
import Debug exposing (..)
import Random exposing (..)
import Fuzz
import Test.Html.Query exposing (..)
import Player exposing (..)
import Utils exposing (..)
import Test.Html.Selector exposing (..)
import Expect exposing (..)
import String
import Grid exposing (..)
import Array exposing (Array, get)
import App exposing (..)
import GridTests
import PlayerTests


all : Test
all =
    describe "shnake tests"
        -- fixme: introduce test module to collect all tests
        [ GridTests.all
        , PlayerTests.all
        , describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        Model newPlayer []
                                in
                                    equal newPosition
                                        (fst <| update 3 (ArrowMsg msg) init).player.head
                 in
                    [ testArrowMsg Up (Position 0 (0 - 1))
                      -- fixme: negatives
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
                                Model newPlayer []

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
                , test "random food doesn't get created in cells occupied by the player"
                    (\() -> fail "pending")
                ]
            , describe "eating"
                [ test "food gets removed when the player moves to the same cell"
                    (\() ->
                        let
                            model =
                                Model newPlayer [ Position 0 1 ]

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.food []
                    )
                , test "player's tail grows when they eat"
                    (\() ->
                        let
                            model =
                                Model newPlayer [ Position 0 1 ]

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.player.tail [ Position 0 1 ]
                    )
                ]
            ]
        , describe "toGrid"
            [ test "renders the player as a red cell"
                (\() ->
                    let
                        model =
                            Model newPlayer []

                        result =
                            get2 (Position 0 0) (toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            , test "renders the players tail"
                (\() ->
                    let
                        model =
                            Model { newPlayer | tail = [ Position -1 0 ] } []

                        result =
                            get2 (Position -1 0) (toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            , test "renders food as a green cell"
                (\() ->
                    let
                        model =
                            Model newPlayer [ Position 1 1 ]

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



-- fixme: rename get2
