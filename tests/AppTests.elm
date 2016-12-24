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
    describe "App"
        [ describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        let
                                            x =
                                                newPlayer 3
                                        in
                                            newModel { x | head = Position 1 1 }
                                in
                                    equal newPosition
                                        (fst <| update 3 (ArrowMsg msg) init).player.head
                 in
                    [ testArrowMsg Up (Position 1 0)
                    , testArrowMsg Down (Position 1 2)
                    , testArrowMsg Left (Position 0 1)
                    , testArrowMsg Right (Position 2 1)
                    ]
                )
            , describe "eating"
                [ test "food gets removed when the player moves to the same cell"
                    (\() ->
                        let
                            model =
                                let
                                    x =
                                        newModel <| newPlayer 3
                                in
                                    { x | food = [ Position 1 2 ] }

                            result =
                                fst <| update 3 (ArrowMsg Down) model
                        in
                            equal result.food []
                    )
                , test "player's tail grows when they eat"
                    (\() ->
                        let
                            model =
                                let
                                    x =
                                        newModel <| newPlayer 21
                                in
                                    { x | food = [ Position 10 11 ] }

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.player.tail [ Position 10 10 ]
                    )
                ]
            ]
        , describe "toGrid"
            [ test "renders the player's head"
                (\() ->
                    let
                        model =
                            newModel <| newPlayer 3

                        result =
                            get (Position 1 1) (toGrid 3 model)

                        expected =
                            Just (Color "red")
                    in
                        equal result expected
                )
            , test "renders the players tail"
                (\() ->
                    let
                        model =
                            let
                                x =
                                    newPlayer 3
                            in
                                newModel { x | tail = [ Position 1 0 ] }

                        result =
                            get (Position 1 0) (toGrid 3 model)

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
                                    newModel <| newPlayer 3
                            in
                                { x | food = [ Position 0 0 ] }

                        result =
                            get (Position 0 0) (toGrid 3 model)

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
