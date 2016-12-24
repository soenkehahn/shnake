module AppTests exposing (..)

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
import App exposing (..)


all : Test
all =
    describe "shnake tests"
        [ describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        newModel newPlayer
                                in
                                    equal newPosition
                                        (fst <| update 3 (ArrowMsg msg) init).player.head
                 in
                    [ testArrowMsg Up (Position 0 -1)
                    , testArrowMsg Down (Position 0 1)
                    , testArrowMsg Left (Position -1 0)
                    , testArrowMsg Right (Position 1 0)
                    ]
                )
            , describe "NewFood"
                [ fuzz (Fuzz.map initialSeed Fuzz.int)
                    "creates a random food item inside the grid"
                    (\seed ->
                        let
                            model =
                                let
                                    x =
                                        newModel newPlayer
                                in
                                    { x | seed = seed }

                            result : List Position
                            result =
                                (fst <| update 3 NewFood model).food
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
                    -- fixme: random food creation in empty cells
                    (\() -> pass)
                ]
            , describe "eating"
                [ test "food gets removed when the player moves to the same cell"
                    (\() ->
                        let
                            model =
                                let
                                    x =
                                        newModel newPlayer
                                in
                                    { x | food = [ Position 0 1 ] }

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.food []
                    )
                , test "player's tail grows when they eat"
                    (\() ->
                        let
                            model =
                                let
                                    x =
                                        newModel newPlayer
                                in
                                    { x | food = [ Position 0 1 ] }

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.player.tail [ Position 0 0 ]
                    )
                ]
            ]
        , describe "toGrid"
            [ test "renders the player's head"
                (\() ->
                    let
                        model =
                            newModel newPlayer

                        result =
                            get (Position 0 0) (toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            , test "renders the players tail"
                (\() ->
                    let
                        model =
                            newModel { newPlayer | tail = [ Position -1 0 ] }

                        result =
                            get (Position -1 0) (toGrid 3 model)

                        expected =
                            Just (Color "blue")
                    in
                        equal result expected
                )
            , test "renders food as a green cell"
                (\() ->
                    let
                        model =
                            let
                                x =
                                    newModel newPlayer
                            in
                                { x | food = [ Position 1 1 ] }

                        result =
                            get (Position 1 1) (toGrid 3 model)

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
