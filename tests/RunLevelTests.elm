module RunLevelTests exposing (..)

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
import RunLevel exposing (..)
import Level.Model exposing (..)
import Levels exposing (..)


all : Test
all =
    describe "RunLevel"
        [ describe "init"
            [ test "food under the player gets removed"
                (\() ->
                    let
                        model =
                            init (Level 5 (Position 1 2) [ Position 1 2 ] [])
                    in
                        equal [] model.food
                )
            , test "food under walls gets removed"
                (\() ->
                    let
                        model =
                            init (Level 5 (Position 0 0) [ Position 1 2 ] [ Position 1 2 ])
                    in
                        equal [] model.food
                )
            , test "wall under the player gets removed"
                (\() ->
                    let
                        model =
                            init (Level 5 (Position 1 2) [] [ Position 1 2 ])
                    in
                        equal [] model.walls
                )
            ]
        , describe "update"
            [ describe "arrow messages"
                (let
                    testArrowMsg msg newPosition =
                        test (toString msg)
                            <| \() ->
                                let
                                    init =
                                        let
                                            x =
                                                newPlayer
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
                                        newModel newPlayer
                                in
                                    { x | food = [ Position 0 1 ] }

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
                                        newModel newPlayer
                                in
                                    { x | food = [ Position 0 1 ] }

                            result =
                                fst <| update 21 (ArrowMsg Down) model
                        in
                            equal result.player.tail [ Position 0 0 ]
                    )
                ]
            , describe "walls"
                [ test "can't move through walls"
                    (\() ->
                        let
                            model =
                                newModel newPlayer
                                    |> setWalls [ Position 1 0 ]

                            result =
                                fst <| update 5 (ArrowMsg Right) model
                        in
                            equal result.player.head (Position 0 0)
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
                            let
                                x =
                                    newPlayer
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
            , test "renders walls in gray"
                (\() ->
                    let
                        model =
                            newModel newPlayer
                                |> setWalls [ Position 2 2 ]

                        result =
                            get (Position 2 2) (toGrid 5 model)
                    in
                        equal result (Just (Color "#444"))
                )
            ]
        , describe "isDone"
            [ test "returns true when no food is left"
                (\() ->
                    let
                        model =
                            newModel newPlayer
                    in
                        equal (isDone model) True
                )
            , test "returns false when food is left"
                (\() ->
                    let
                        model =
                            let
                                x =
                                    newModel newPlayer
                            in
                                { x | food = [ Position 1 1 ] }
                    in
                        equal (isDone model) False
                )
            ]
        ]
